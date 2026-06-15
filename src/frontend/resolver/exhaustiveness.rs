//! Match exhaustiveness checking via the usefulness algorithm (Maranget).
//!
//! A `match` must cover every possible value of its subject: the compiler relies on this, since a non-matching
//! subject would fall through past the arms without being discarded. This module computes, for a given subject
//! type and the patterns of a match's arms, whether any value is left uncovered and if so a witness pattern
//! describing it. The logic is pure over type metadata ([`MetaContainer`]) and the pattern AST; [`witness`] is the
//! entry point called from `resolve_match_block`.

use crate::frontend::ast;
use crate::shared::MetaContainer;
use crate::shared::meta::{Type, EnumVariant};
use crate::shared::typed_ids::TypeId;

/// A constructor of a type's value space, used to enumerate the cases a match must cover. Types without a finite
/// constructor set (integers, floats, strings) have no `MatchCtor`.
enum MatchCtor {
    /// One of the two boolean values.
    Bool(bool),
    /// An enum variant, identified by its index.
    Variant(usize),
    /// The sole constructor of a struct.
    Struct,
}

/// Computes the witness of a non-exhaustive match: given `col_types`, the types of the value columns, and `matrix`,
/// one row per match arm (each cell `None` standing for a wildcard introduced during specialization), returns
/// `Ok(Some(witness))` describing a value left uncovered, or `Ok(None)` if every value is covered. `witness` holds
/// one rendered pattern string per column. Returns `Err(())` if a column type is not yet resolved, in which case
/// the caller must retry on a later pass.
pub(super) fn witness<'a>(meta: &impl MetaContainer, col_types: &[Option<TypeId>], matrix: &[Vec<Option<&'a ast::Pattern>>]) -> Result<Option<Vec<String>>, ()> {
    // base case: no columns left. an empty row means some arm covered this combination; no rows means a gap.
    let Some(&head_type) = col_types.first() else {
        return Ok(if matrix.is_empty() { Some(Vec::new()) } else { None });
    };
    let head_type_id = head_type.ok_or(())?; // unresolved column type: cannot decide yet
    // expand or-patterns at the head into independent rows so each alternative is considered on its own
    // (e.g. `true | false` becomes two rows covering both bool constructors)
    let matrix = expand_or_heads(matrix);
    let matrix = matrix.as_slice();
    match type_ctors(meta, head_type_id) {
        // finite constructor set (bool, enum, struct): every constructor must be covered
        Some(ctors) => {
            for ctor in &ctors {
                let field_types = ctor_fields(meta, head_type_id, ctor);
                let arity = field_types.len();
                let specialized: Vec<Vec<Option<&'a ast::Pattern>>> = matrix.iter()
                    .filter_map(|row| specialize_row(meta, head_type_id, ctor, arity, row))
                    .collect();
                let mut sub_types = field_types;
                sub_types.extend_from_slice(&col_types[1..]);
                if let Some(witness) = witness(meta, &sub_types, &specialized)? {
                    let head = ctor_to_string(meta, head_type_id, ctor, &witness[..arity]);
                    let mut full = vec![head];
                    full.extend_from_slice(&witness[arity..]);
                    return Ok(Some(full));
                }
            }
            Ok(None)
        },
        // opaque type (int/float/String): no finite signature, so only a catch-all arm can make it exhaustive
        None => {
            let default: Vec<Vec<Option<&'a ast::Pattern>>> = matrix.iter()
                .filter(|row| row[0].map_or(true, is_wildcard))
                .map(|row| row[1..].to_vec())
                .collect();
            Ok(witness(meta, &col_types[1..], &default)?.map(|witness| {
                let mut full = vec!["_".to_string()];
                full.extend(witness);
                full
            }))
        },
    }
}

/// The finite set of constructors of `type_id`, or `None` for types whose value space cannot be enumerated
/// (integers, floats, strings) and which therefore require a catch-all arm to be matched exhaustively.
fn type_ctors(meta: &impl MetaContainer, type_id: TypeId) -> Option<Vec<MatchCtor>> {
    match meta.type_by_id(type_id) {
        Type::bool => Some(vec![ MatchCtor::Bool(true), MatchCtor::Bool(false) ]),
        Type::Enum(enum_) => Some((0..enum_.variants.len()).map(MatchCtor::Variant).collect()),
        Type::Struct(_) => Some(vec![ MatchCtor::Struct ]),
        _ => None,
    }
}

/// The column types a constructor introduces when a row is specialized to it (its fields, in canonical order).
fn ctor_fields(meta: &impl MetaContainer, type_id: TypeId, ctor: &MatchCtor) -> Vec<Option<TypeId>> {
    match ctor {
        MatchCtor::Bool(_) => Vec::new(),
        MatchCtor::Variant(index) => match meta.type_by_id(type_id).as_enum().and_then(|enum_| enum_.variants.get(*index)) {
            Some((_, EnumVariant::Data(field_type_ids))) => field_type_ids.clone(),
            _ => Vec::new(),
        },
        MatchCtor::Struct => meta.type_by_id(type_id).as_struct().map_or_else(Vec::new, |struct_| struct_.fields.values().cloned().collect()),
    }
}

/// Specializes a single matrix row to `ctor`: returns the row's sub-patterns followed by its remaining columns
/// if the head matches `ctor` (a wildcard head matching any constructor), or `None` if the head excludes it.
fn specialize_row<'a>(meta: &impl MetaContainer, type_id: TypeId, ctor: &MatchCtor, arity: usize, row: &[Option<&'a ast::Pattern>]) -> Option<Vec<Option<&'a ast::Pattern>>> {
    use ast::Pattern;
    let sub: Vec<Option<&'a ast::Pattern>> = match row[0] {
        None => vec![None; arity],
        Some(Pattern::Wildcard(_)) | Some(Pattern::Binding(_)) => vec![None; arity],
        Some(Pattern::Literal(literal)) => match ctor {
            MatchCtor::Bool(value) if literal.value.as_bool() == Some(*value) => Vec::new(),
            _ => return None,
        },
        Some(Pattern::Path(path)) => match ctor {
            MatchCtor::Variant(index) if path_variant(meta, type_id, path) == Some(*index) => Vec::new(),
            _ => return None,
        },
        // ranges only apply to numeric (opaque) types, which have no finite ctors and so never reach
        // specialize_row; treat as matching no constructor for completeness
        Some(Pattern::Range(_)) => return None,
        Some(Pattern::VariantTuple(variant)) => match ctor {
            MatchCtor::Variant(index) if path_variant(meta, type_id, &variant.path) == Some(*index) => variant.elements.iter().map(Some).collect(),
            _ => return None,
        },
        Some(Pattern::Struct(structure)) => match ctor {
            // struct has a single constructor; reorder the named field sub-patterns into canonical field order
            MatchCtor::Struct => meta.type_by_id(type_id).as_struct().map_or_else(Vec::new, |struct_| {
                struct_.fields.keys().map(|field_name| structure.fields.iter().find(|(name, _)| &name.name == field_name).map(|(_, pattern)| pattern)).collect()
            }),
            _ => return None,
        },
        // or-patterns at the head are flattened away by expand_or_heads before specialization is reached
        Some(Pattern::Or(_)) => return None,
    };
    let mut specialized = sub;
    specialized.extend_from_slice(&row[1..]);
    Some(specialized)
}

/// Expands any or-pattern at the head (column 0) of a row into one row per alternative, recursing so that
/// nested or-patterns are flattened too. Rows whose head is not an or-pattern are passed through unchanged.
fn expand_or_heads<'a>(matrix: &[Vec<Option<&'a ast::Pattern>>]) -> Vec<Vec<Option<&'a ast::Pattern>>> {
    let mut expanded = Vec::with_capacity(matrix.len());
    for row in matrix {
        expand_row_head(row, &mut expanded);
    }
    expanded
}

/// Appends `row` to `out`, first splitting a leading or-pattern into one row per alternative (recursively).
fn expand_row_head<'a>(row: &[Option<&'a ast::Pattern>], out: &mut Vec<Vec<Option<&'a ast::Pattern>>>) {
    if let Some(ast::Pattern::Or(or)) = row.first().copied().flatten() {
        for alternative in &or.alternatives {
            let mut alternative_row = row.to_vec();
            alternative_row[0] = Some(alternative);
            expand_row_head(&alternative_row, out);
        }
    } else {
        out.push(row.to_vec());
    }
}

/// Resolves the variant index named by a path's last segment against an enum type.
fn path_variant(meta: &impl MetaContainer, type_id: TypeId, path: &ast::Path) -> Option<usize> {
    let variant_name = &path.segments.last()?.name;
    meta.type_by_id(type_id).as_enum().and_then(|enum_| enum_.variant_index(variant_name)).map(|index| index as usize)
}

/// Renders a constructor (with its already-rendered sub-witnesses) as a pattern string for error reporting.
fn ctor_to_string(meta: &impl MetaContainer, type_id: TypeId, ctor: &MatchCtor, sub_witness: &[String]) -> String {
    match ctor {
        MatchCtor::Bool(value) => value.to_string(),
        MatchCtor::Variant(index) => {
            let enum_ = meta.type_by_id(type_id).as_enum();
            let type_name = meta.type_name(type_id);
            match enum_.and_then(|enum_| enum_.variants.get(*index)) {
                Some((name, EnumVariant::Data(_))) => format!("{}::{}({})", type_name, name, sub_witness.join(", ")),
                Some((name, _)) => format!("{}::{}", type_name, name),
                None => "_".to_string(),
            }
        },
        MatchCtor::Struct => {
            let type_name = meta.type_name(type_id);
            match meta.type_by_id(type_id).as_struct() {
                Some(struct_) => {
                    let fields = struct_.fields.keys().zip(sub_witness).map(|(name, pattern)| format!("{}: {}", name, pattern)).collect::<Vec<_>>().join(", ");
                    format!("{} {{ {} }}", type_name, fields)
                },
                None => "_".to_string(),
            }
        },
    }
}

/// Whether a pattern matches unconditionally (and therefore acts as a catch-all in its column).
fn is_wildcard(pattern: &ast::Pattern) -> bool {
    matches!(pattern, ast::Pattern::Wildcard(_) | ast::Pattern::Binding(_))
}
