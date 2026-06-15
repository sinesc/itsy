//! Derive macro for the [`itsy`](https://docs.rs/itsy/) crate.
//!
//! Generates the marshalling glue (`VMType`, `VMValue`, `VMField`) that lets a Rust struct or enum be
//! passed to and returned from Itsy API functions defined via the `itsy_api!` macro.
//!
//! Supported shapes:
//! - structs with named fields (heap reference type)
//! - C-like enums where no variant carries data (value type)
//! - data-carrying enums with tuple-style variants (heap reference type)

use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{parse_macro_input, spanned::Spanned, Data, DataEnum, DataStruct, DeriveInput, Fields, Ident, Type};

/// Derives the traits required to marshal a struct or enum across the Itsy API boundary.
///
/// All field/variant-field types must consist of primitives, `String` or other `#[derive(VMValue)]`
/// types. The Itsy type is registered under the struct/enum's Rust name, namespaced under the API
/// typename it is exposed through (e.g. `MyAPI::MyStruct`); struct fields and enum variants keep
/// their Rust names.
#[proc_macro_derive(VMValue, attributes(itsy))]
pub fn derive_vmvalue(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    let result = match &ast.data {
        Data::Struct(data) => impl_struct(&ast.ident, data),
        Data::Enum(data) => impl_enum(&ast.ident, data),
        Data::Union(_) => Err(syn::Error::new(ast.span(), "#[derive(VMValue)] does not support unions")),
    };
    match result {
        Ok(tokens) => tokens.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

/// Generates marshalling for a named-field struct (a heap reference type).
fn impl_struct(ident: &Ident, data: &DataStruct) -> syn::Result<proc_macro2::TokenStream> {
    let name_str = ident.to_string();

    let named = match &data.fields {
        Fields::Named(named) => named,
        Fields::Unnamed(_) => return Err(syn::Error::new(data.fields.span(), "#[derive(VMValue)] does not support tuple structs yet")),
        Fields::Unit => return Err(syn::Error::new(data.fields.span(), "#[derive(VMValue)] does not support unit structs")),
    };

    // order fields by name to match the BTreeMap-based heap layout the resolver and compiler use.
    let mut fields: Vec<(String, &Ident, &Type)> = named.named.iter().map(|field| {
        let ident = field.ident.as_ref().expect("named field without identifier");
        (ident.to_string(), ident, &field.ty)
    }).collect();
    fields.sort_by(|a, b| a.0.cmp(&b.0));

    let field_idents: Vec<&Ident> = fields.iter().map(|f| f.1).collect();
    let field_names: Vec<&String> = fields.iter().map(|f| &f.0).collect();
    let field_types: Vec<&Type> = fields.iter().map(|f| f.2).collect();

    let compiler_impl = quote! {
        #[cfg(feature = "compiler")]
        const _: () = {
            use ::itsy::internals::binary::{VMType, ApiTypeDef, ApiTypeKind};
            impl VMType for #ident {
                const ITSY_NAME: &'static str = #name_str;
                fn itsy_type_def() -> Option<ApiTypeDef> {
                    Some(ApiTypeDef {
                        name: #name_str,
                        kind: ApiTypeKind::Struct {
                            fields: vec![ #( (#field_names, <#field_types as VMType>::ITSY_NAME) ),* ],
                        },
                    })
                }
                fn collect_type_defs(out: &mut Vec<ApiTypeDef>) {
                    if out.iter().any(|d| d.name == #name_str) {
                        return;
                    }
                    out.push(<Self as VMType>::itsy_type_def().unwrap());
                    #( <#field_types as VMType>::collect_type_defs(out); )*
                }
            }
        };
    };

    let runtime_impl = quote! {
        #[cfg(feature = "runtime")]
        const _: () = {
            use ::itsy::internals::binary::{VMValue, VMField, read_nested_ref, alloc_value};
            use ::itsy::internals::binary::heap::{Heap, HeapRef, HeapRefOp};

            impl VMValue for #ident {
                type Stack = HeapRef;
                #[allow(unused_assignments, unused_mut, unused_variables)]
                fn from_stack(heap: &Heap, this: HeapRef) -> Self {
                    let mut offset = 0usize;
                    #(
                        let #field_idents = <#field_types as VMField>::read_field(heap, this, offset);
                        offset += <#field_types as VMField>::HEAP_SIZE;
                    )*
                    Self { #( #field_idents ),* }
                }
                fn to_stack(self, heap: &mut Heap) -> HeapRef {
                    let mut buffer: Vec<u8> = Vec::new();
                    #( <#field_types as VMField>::write_field(self.#field_idents, heap, &mut buffer); )*
                    alloc_value(heap, &buffer)
                }
                #[allow(unused_assignments, unused_mut, unused_variables)]
                fn free_stack(heap: &mut Heap, this: HeapRef) {
                    if heap.item_refs(this.index()) == 0 {
                        let mut offset = 0usize;
                        #(
                            <#field_types as VMField>::free_field(heap, this, offset);
                            offset += <#field_types as VMField>::HEAP_SIZE;
                        )*
                    }
                    heap.ref_item(this.index(), HeapRefOp::Free);
                }
            }

            impl VMField for #ident {
                const HEAP_SIZE: usize = ::std::mem::size_of::<HeapRef>();
                fn read_field(heap: &Heap, base: HeapRef, offset: usize) -> Self {
                    let item = read_nested_ref(heap, base, offset);
                    <Self as VMValue>::from_stack(heap, item)
                }
                fn write_field(self, heap: &mut Heap, buf: &mut Vec<u8>) {
                    let item = <Self as VMValue>::to_stack(self, heap);
                    buf.extend_from_slice(&item.to_ne_bytes());
                }
                fn free_field(heap: &mut Heap, base: HeapRef, offset: usize) {
                    let item = read_nested_ref(heap, base, offset);
                    <Self as VMValue>::free_stack(heap, item);
                }
            }
        };
    };

    Ok(quote! { #compiler_impl #runtime_impl })
}

/// Generates marshalling for an enum, dispatching to the primitive (value-type) or data-carrying
/// (reference-type) representation depending on whether any variant carries data.
fn impl_enum(ident: &Ident, data: &DataEnum) -> syn::Result<proc_macro2::TokenStream> {
    let is_primitive = data.variants.iter().all(|variant| matches!(variant.fields, Fields::Unit));
    if is_primitive {
        impl_primitive_enum(ident, data)
    } else {
        impl_data_enum(ident, data)
    }
}

/// Generates marshalling for a C-like enum as an `i32` discriminant (a value type).
fn impl_primitive_enum(ident: &Ident, data: &DataEnum) -> syn::Result<proc_macro2::TokenStream> {
    let name_str = ident.to_string();
    let variant_idents: Vec<&Ident> = data.variants.iter().map(|v| &v.ident).collect();
    let variant_names: Vec<String> = variant_idents.iter().map(|i| i.to_string()).collect();

    let compiler_impl = quote! {
        #[cfg(feature = "compiler")]
        const _: () = {
            use ::itsy::internals::binary::{VMType, ApiTypeDef, ApiTypeKind};
            impl VMType for #ident {
                const ITSY_NAME: &'static str = #name_str;
                fn itsy_type_def() -> Option<ApiTypeDef> {
                    Some(ApiTypeDef {
                        name: #name_str,
                        kind: ApiTypeKind::PrimitiveEnum {
                            variants: vec![ #( (#variant_names, #ident::#variant_idents as i64) ),* ],
                        },
                    })
                }
                fn collect_type_defs(out: &mut Vec<ApiTypeDef>) {
                    if out.iter().any(|d| d.name == #name_str) {
                        return;
                    }
                    out.push(<Self as VMType>::itsy_type_def().unwrap());
                }
            }
        };
    };

    let runtime_impl = quote! {
        #[cfg(feature = "runtime")]
        const _: () = {
            use ::itsy::internals::binary::{VMValue, VMField};
            use ::itsy::internals::binary::heap::{Heap, HeapRef};

            impl VMValue for #ident {
                type Stack = i32;
                fn from_stack(_heap: &Heap, raw: i32) -> Self {
                    #( if raw == #ident::#variant_idents as i32 { return #ident::#variant_idents; } )*
                    panic!(concat!("invalid discriminant for enum ", #name_str, ": {}"), raw);
                }
                fn to_stack(self, _heap: &mut Heap) -> i32 {
                    self as i32
                }
                fn free_stack(_heap: &mut Heap, _raw: i32) {}
            }

            impl VMField for #ident {
                const HEAP_SIZE: usize = <i32 as VMField>::HEAP_SIZE;
                fn read_field(heap: &Heap, base: HeapRef, offset: usize) -> Self {
                    let raw = <i32 as VMField>::read_field(heap, base, offset);
                    <Self as VMValue>::from_stack(heap, raw)
                }
                fn write_field(self, heap: &mut Heap, buf: &mut Vec<u8>) {
                    <i32 as VMField>::write_field(self as i32, heap, buf);
                }
                fn free_field(_heap: &mut Heap, _base: HeapRef, _offset: usize) {}
            }
        };
    };

    Ok(quote! { #compiler_impl #runtime_impl })
}

/// Generates marshalling for a data-carrying enum as a heap object.
fn impl_data_enum(ident: &Ident, data: &DataEnum) -> syn::Result<proc_macro2::TokenStream> {
    let name_str = ident.to_string();

    // per-variant token fragments for the compiler-side definition and runtime match arms
    let mut variant_defs = Vec::new();
    let mut all_field_types: Vec<&Type> = Vec::new();
    let mut from_arms = Vec::new();
    let mut to_arms = Vec::new();
    let mut free_arms = Vec::new();

    for (index, variant) in data.variants.iter().enumerate() {
        let vident = &variant.ident;
        let vname = vident.to_string();

        let field_types: Vec<&Type> = match &variant.fields {
            Fields::Unit => Vec::new(),
            Fields::Unnamed(fields) => fields.unnamed.iter().map(|f| &f.ty).collect(),
            Fields::Named(_) => return Err(syn::Error::new(variant.span(), "#[derive(VMValue)] does not support struct-style enum variants")),
        };
        all_field_types.extend(field_types.iter().copied());

        variant_defs.push(quote! {
            (#vname, vec![ #( <#field_types as VMType>::ITSY_NAME ),* ])
        });

        let bindings: Vec<Ident> = (0..field_types.len()).map(|i| format_ident!("field_{}", i)).collect();
        let is_unit = matches!(variant.fields, Fields::Unit);

        let constructor = if is_unit {
            quote! { Self::#vident }
        } else {
            quote! { Self::#vident( #( #bindings ),* ) }
        };

        from_arms.push(quote! {
            #index => {
                let mut offset = VARIANT_TAG_SIZE;
                #(
                    let #bindings = <#field_types as VMField>::read_field(heap, this, offset);
                    offset += <#field_types as VMField>::HEAP_SIZE;
                )*
                #constructor
            }
        });

        to_arms.push(quote! {
            #constructor => {
                write_variant_tag(&mut buffer, #index);
                #( <#field_types as VMField>::write_field(#bindings, heap, &mut buffer); )*
            }
        });

        free_arms.push(quote! {
            #index => {
                let mut offset = VARIANT_TAG_SIZE;
                #(
                    <#field_types as VMField>::free_field(heap, this, offset);
                    offset += <#field_types as VMField>::HEAP_SIZE;
                )*
            }
        });
    }

    let compiler_impl = quote! {
        #[cfg(feature = "compiler")]
        const _: () = {
            use ::itsy::internals::binary::{VMType, ApiTypeDef, ApiTypeKind};
            impl VMType for #ident {
                const ITSY_NAME: &'static str = #name_str;
                fn itsy_type_def() -> Option<ApiTypeDef> {
                    Some(ApiTypeDef {
                        name: #name_str,
                        kind: ApiTypeKind::DataEnum {
                            variants: vec![ #( #variant_defs ),* ],
                        },
                    })
                }
                fn collect_type_defs(out: &mut Vec<ApiTypeDef>) {
                    if out.iter().any(|d| d.name == #name_str) {
                        return;
                    }
                    out.push(<Self as VMType>::itsy_type_def().unwrap());
                    #( <#all_field_types as VMType>::collect_type_defs(out); )*
                }
            }
        };
    };

    let runtime_impl = quote! {
        #[cfg(feature = "runtime")]
        const _: () = {
            use ::itsy::internals::binary::{VMValue, VMField, read_nested_ref, alloc_value, write_variant_tag, read_variant_tag, VARIANT_TAG_SIZE};
            use ::itsy::internals::binary::heap::{Heap, HeapRef, HeapRefOp};

            impl VMValue for #ident {
                type Stack = HeapRef;
                #[allow(unused_assignments, unused_mut, unused_variables)]
                fn from_stack(heap: &Heap, this: HeapRef) -> Self {
                    match read_variant_tag(heap, this) {
                        #( #from_arms )*
                        other => panic!(concat!("invalid variant tag for enum ", #name_str, ": {}"), other),
                    }
                }
                #[allow(unused_assignments, unused_mut, unused_variables)]
                fn to_stack(self, heap: &mut Heap) -> HeapRef {
                    let mut buffer: Vec<u8> = Vec::new();
                    match self {
                        #( #to_arms )*
                    }
                    alloc_value(heap, &buffer)
                }
                #[allow(unused_assignments, unused_mut, unused_variables)]
                fn free_stack(heap: &mut Heap, this: HeapRef) {
                    if heap.item_refs(this.index()) == 0 {
                        match read_variant_tag(heap, this) {
                            #( #free_arms )*
                            _ => {}
                        }
                    }
                    heap.ref_item(this.index(), HeapRefOp::Free);
                }
            }

            impl VMField for #ident {
                const HEAP_SIZE: usize = ::std::mem::size_of::<HeapRef>();
                fn read_field(heap: &Heap, base: HeapRef, offset: usize) -> Self {
                    let item = read_nested_ref(heap, base, offset);
                    <Self as VMValue>::from_stack(heap, item)
                }
                fn write_field(self, heap: &mut Heap, buf: &mut Vec<u8>) {
                    let item = <Self as VMValue>::to_stack(self, heap);
                    buf.extend_from_slice(&item.to_ne_bytes());
                }
                fn free_field(heap: &mut Heap, base: HeapRef, offset: usize) {
                    let item = read_nested_ref(heap, base, offset);
                    <Self as VMValue>::free_stack(heap, item);
                }
            }
        };
    };

    Ok(quote! { #compiler_impl #runtime_impl })
}
