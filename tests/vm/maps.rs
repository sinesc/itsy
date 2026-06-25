use crate::util::*;

#[test]
fn map_literal_string_keys() {
    let result = run(stringify!(
        let m = [ "a" => 1u64, "b" => 2u64, "c" => 3u64 ];
        ret_u64(m["a"]);
        ret_u64(m["b"]);
        ret_u64(m["c"]);
    ));
    assert_all(&result, &[ 1u64, 2, 3 ]);
}

#[test]
fn map_literal_duplicate_keys_last_wins() {
    // duplicate keys in a literal resolve to the last value (matching Rust), with a single live entry
    let result = run(stringify!(
        let m = [ "a" => 1u64, "b" => 2u64, "a" => 3u64 ];
        ret_u64(m["a"]);
        ret_u64(m["b"]);
        ret_u64(m.len());
    ));
    assert_all(&result, &[ 3u64, 2, 2 ]);
}

#[test]
fn map_literal_duplicate_string_values_last_wins() {
    // exercises the replaced-value drop path for reference values during literal construction (no leak)
    let result = run(stringify!(
        let m = [ "k" => "first", "k" => "second" ];
        ret_string(m["k"]);
    ));
    assert_all(&result, &[ "second".to_string() ]);
}

#[test]
fn map_index_update_existing() {
    let result = run(stringify!(
        let m = [ "a" => 1u64, "b" => 2u64 ];
        m["a"] = 42u64;
        ret_u64(m["a"]);
        ret_u64(m["b"]);
    ));
    assert_all(&result, &[ 42u64, 2 ]);
}

#[test]
fn map_index_insert_new() {
    let result = run(stringify!(
        let m = [ "a" => 1u64 ];
        m["b"] = 7u64;
        m["c"] = 8u64;
        ret_u64(m["a"]);
        ret_u64(m["b"]);
        ret_u64(m["c"]);
    ));
    assert_all(&result, &[ 1u64, 7, 8 ]);
}

#[test]
fn map_empty_then_insert() {
    let result = run(stringify!(
        let m = [ => ];
        m["k"] = 5u64;
        m["j"] = 6u64;
        ret_u64(m["k"]);
        ret_u64(m["j"]);
    ));
    assert_all(&result, &[ 5u64, 6 ]);
}

#[test]
fn map_primitive_keys() {
    let result = run(stringify!(
        let m = [ 1u64 => 10u64, 2u64 => 20u64, 3u64 => 30u64 ];
        ret_u64(m[1u64]);
        ret_u64(m[2u64]);
        ret_u64(m[3u64]);
    ));
    assert_all(&result, &[ 10u64, 20, 30 ]);
}

#[test]
fn map_string_values() {
    let result = run(stringify!(
        let m = [ 1u64 => "one", 2u64 => "two" ];
        ret_string(m[1u64]);
        ret_string(m[2u64]);
    ));
    assert_all(&result, &[ "one".to_string(), "two".to_string() ]);
}

#[test]
fn map_struct_keys() {
    let result = run(stringify!(
        struct Point { x: i32, y: i32 }
        fn main() {
            let m = [ Point { x: 1, y: 2 } => 100u64, Point { x: 3, y: 4 } => 200u64 ];
            ret_u64(m[Point { x: 1, y: 2 }]);
            ret_u64(m[Point { x: 3, y: 4 }]);
        }
    ));
    assert_all(&result, &[ 100u64, 200 ]);
}

#[test]
fn map_struct_values() {
    let result = run(stringify!(
        struct Point { x: i32, y: i32 }
        fn main() {
            let m = [ "origin" => Point { x: 0, y: 0 }, "unit" => Point { x: 1, y: 1 } ];
            let o = m["origin"]; ret_i32(o.x); ret_i32(o.y);
            let u = m["unit"]; ret_i32(u.x); ret_i32(u.y);
        }
    ));
    assert_all(&result, &[ 0i32, 0, 1, 1 ]);
}

#[test]
fn map_reassigned_value_releases_old() {
    // exercises the replace path (old value dropped, new value retained) without leaking the heap
    let result = run(stringify!(
        let m = [ "a" => "first" ];
        m["a"] = "second";
        m["a"] = "third";
        ret_string(m["a"]);
    ));
    assert_all(&result, &[ "third".to_string() ]);
}

#[test]
fn map_get_missing_key_returns_none() {
    // the fallible `.get` returns None for an absent key (whereas indexing traps; see below)
    let result = run(stringify!(
        let m = [ "a" => 1u64 ];
        let v = m.get("missing"); match v { Some(val) => ret_u64(val), None => ret_u64(0) };
    ));
    assert_all(&result, &[ 0u64 ]);
}

#[test]
#[should_panic(expected = "Map key not found")]
fn map_index_missing_key_traps() {
    // indexing a map with an absent key traps (mirroring out-of-bounds array indexing); use `.get` for
    // a fallible lookup that returns an Option instead
    run(stringify!(
        let m = [ "a" => 1u64 ];
        ret_u64(m["missing"]);
    ));
}

#[test]
fn map_len() {
    let result = run(stringify!(
        let m = [ "a" => 1u64, "b" => 2u64, "c" => 3u64 ];
        ret_u64(m.len());
        let e = [ => ];
        e["x"] = 1u64;
        ret_u64(e.len());
    ));
    assert_all(&result, &[ 3u64, 1 ]);
}

#[test]
fn map_method_insert_get() {
    let result = run(stringify!(
        let m = [ => ];
        m.insert("a", 1u64);
        m.insert("b", 2u64);
        m.insert("a", 42u64); // update existing
        let v = m.get("a"); match v { Some(val) => ret_u64(val), None => ret_u64(0) };
        let v = m.get("b"); match v { Some(val) => ret_u64(val), None => ret_u64(0) };
        ret_u64(m.len());
    ));
    assert_all(&result, &[ 42u64, 2, 2 ]);
}

#[test]
fn map_contains_key() {
    let result = run(stringify!(
        let m = [ "a" => 1u64, "b" => 2u64 ];
        ret_bool(m.contains_key("a"));
        ret_bool(m.contains_key("b"));
        ret_bool(m.contains_key("c"));
        m.remove("a");
        ret_bool(m.contains_key("a"));
        m.insert("c", 3u64);
        ret_bool(m.contains_key("c"));
    ));
    assert_all(&result, &[ true, true, false, false, true ]);
}

#[test]
fn map_contains_key_primitive() {
    let result = run(stringify!(
        let m = [ 1u64 => "a", 2u64 => "b" ];
        ret_bool(m.contains_key(1u64));
        ret_bool(m.contains_key(3u64));
    ));
    assert_all(&result, &[ true, false ]);
}

#[test]
fn map_remove() {
    let result = run(stringify!(
        let m = [ "a" => 1u64, "b" => 2u64, "c" => 3u64 ];
        m.remove("b");
        ret_u64(m.len());
        ret_u64(m["a"]);
        ret_u64(m["c"]);
        m.remove("does-not-exist"); // no-op
        ret_u64(m.len());
    ));
    assert_all(&result, &[ 2u64, 1, 3, 2 ]);
}

#[test]
fn map_remove_then_reinsert() {
    let result = run(stringify!(
        let m = [ "a" => 1u64, "b" => 2u64 ];
        m.remove("a");
        m["a"] = 7u64; // re-insert into a tombstoned slot
        ret_u64(m["a"]);
        ret_u64(m["b"]);
        ret_u64(m.len());
    ));
    assert_all(&result, &[ 7u64, 2, 2 ]);
}

#[test]
fn map_clear() {
    let result = run(stringify!(
        let m = [ "a" => 1u64, "b" => 2u64 ];
        m.clear();
        ret_u64(m.len());
        m["x"] = 9u64; // usable after clear
        ret_u64(m.len());
        ret_u64(m["x"]);
    ));
    assert_all(&result, &[ 0u64, 1, 9 ]);
}

#[test]
fn map_clear_string_values_no_leak() {
    // clearing must drop the boxed reference values without leaking the heap
    let result = run(stringify!(
        let m = [ "a" => "first", "b" => "second" ];
        m.clear();
        ret_u64(m.len());
    ));
    assert_all(&result, &[ 0u64 ]);
}

#[test]
fn map_extend() {
    let result = run(stringify!(
        let a = [ "a" => 1u64, "b" => 2u64 ];
        let b = [ "b" => 20u64, "c" => 30u64 ]; // "b" collides, "c" is new
        a.extend(b);
        ret_u64(a.len());
        ret_u64(a["a"]);
        ret_u64(a["b"]); // overwritten
        ret_u64(a["c"]);
        ret_u64(b.len()); // source is unchanged
    ));
    assert_all(&result, &[ 3u64, 1, 20, 30, 2 ]);
}

#[test]
fn map_extend_empty_source() {
    let result = run(stringify!(
        let a = [ "a" => 1u64 ];
        let b = [ => ];
        a.extend(b);
        ret_u64(a.len());
        ret_u64(a["a"]);
    ));
    assert_all(&result, &[ 1u64, 1 ]);
}

#[test]
fn map_extend_into_empty() {
    let result = run(stringify!(
        let a = [ => ];
        a["seed"] = 0u64; // pins the element types
        a.remove("seed"); // ...leaving an empty target
        let b = [ "x" => 5u64, "y" => 6u64 ];
        a.extend(b);
        ret_u64(a.len());
        ret_u64(a["x"]);
        ret_u64(a["y"]);
    ));
    assert_all(&result, &[ 2u64, 5, 6 ]);
}

#[test]
fn map_extend_string_values_no_leak() {
    // sharing boxed reference values into the target must keep refcounts balanced
    let result = run(stringify!(
        let a = [ "a" => "first", "b" => "second" ];
        let b = [ "b" => "SECOND", "c" => "third" ];
        a.extend(b);
        ret_u64(a.len());
        ret_str(a["b"]);
        ret_str(a["c"]);
        ret_u64(b.len());
    ));
    assert_all!(&result, [ 3u64, String::from("SECOND"), String::from("third"), 2u64 ]);
}

#[test]
fn map_extend_primitive_keys() {
    let result = run(stringify!(
        let a = [ 1u64 => "one", 2u64 => "two" ];
        let b = [ 2u64 => "TWO", 3u64 => "three" ];
        a.extend(b);
        ret_u64(a.len());
        ret_str(a[2u64]);
        ret_str(a[3u64]);
    ));
    assert_all!(&result, [ 3u64, String::from("TWO"), String::from("three") ]);
}

#[test]
fn map_extend_self_is_noop() {
    let result = run(stringify!(
        let m = [ "a" => "first", "b" => "second" ];
        m.extend(m);
        ret_u64(m.len());
        ret_str(m["a"]);
        ret_str(m["b"]);
    ));
    assert_all!(&result, [ 2u64, String::from("first"), String::from("second") ]);
}

#[test]
fn map_keys() {
    let result = run(stringify!(
        let m = [ "a" => 1u64, "b" => 2u64, "c" => 3u64 ];
        let ks = m.keys();
        for k in ks {
            ret_str(k);
        }
    ));
    assert_all(&result, &[ "a".to_string(), "b".to_string(), "c".to_string() ]);
}

#[test]
fn map_values() {
    let result = run(stringify!(
        let m = [ "a" => 1u64, "b" => 2u64, "c" => 3u64 ];
        let vs = m.values();
        for v in vs {
            ret_u64(v);
        }
    ));
    assert_all(&result, &[ 1u64, 2, 3 ]);
}

#[test]
fn map_keys_primitive() {
    let result = run(stringify!(
        let m = [ 10u64 => 1u64, 20u64 => 2u64 ];
        let ks = m.keys();
        for k in ks {
            ret_u64(k);
        }
    ));
    assert_all(&result, &[ 10u64, 20 ]);
}

#[test]
fn map_values_string() {
    let result = run(stringify!(
        let m = [ 1u64 => "one", 2u64 => "two" ];
        let vs = m.values();
        for v in vs {
            ret_str(v);
        }
    ));
    assert_all(&result, &[ "one".to_string(), "two".to_string() ]);
}

#[test]
fn for_in_map_values() {
    // the single-binding form iterates values (mirroring arrays)
    let result = run(stringify!(
        let m = [ "a" => 1u64, "b" => 2u64, "c" => 3u64 ];
        for v in m {
            ret_u64(v);
        }
    ));
    assert_all(&result, &[ 1u64, 2, 3 ]);
}

#[test]
fn for_in_map_string_values() {
    let result = run(stringify!(
        let m = [ 1u64 => "one", 2u64 => "two", 3u64 => "three" ];
        for v in m {
            ret_str(v);
        }
    ));
    assert_all(&result, &[ "one".to_string(), "two".to_string(), "three".to_string() ]);
}

#[test]
fn for_in_map_string_keys() {
    // `key, _` iterates keys alone
    let result = run(stringify!(
        let m = [ "a" => 1u64, "b" => 2u64, "c" => 3u64 ];
        for k, _ in m {
            ret_str(k);
        }
    ));
    assert_all(&result, &[ "a".to_string(), "b".to_string(), "c".to_string() ]);
}

#[test]
fn for_in_map_primitive_keys() {
    let result = run(stringify!(
        let m = [ 10u64 => 100u64, 20u64 => 200u64, 30u64 => 300u64 ];
        for k, _ in m {
            ret_u64(k);
        }
    ));
    assert_all(&result, &[ 10u64, 20, 30 ]);
}

#[test]
fn for_in_map_lookup_value() {
    // iterate keys and look up the corresponding value through the map being iterated
    let result = run(stringify!(
        let m = [ 1u64 => 100u64, 2u64 => 200u64, 3u64 => 300u64 ];
        for k, _ in m {
            ret_u64(m[k]);
        }
    ));
    assert_all(&result, &[ 100u64, 200, 300 ]);
}

#[test]
fn for_in_map_ignore_key() {
    // `_, value` iterates values, ignoring keys
    let result = run(stringify!(
        let m = [ "a" => 1u64, "b" => 2u64, "c" => 3u64 ];
        for _, v in m {
            ret_u64(v);
        }
    ));
    assert_all(&result, &[ 1u64, 2, 3 ]);
}

#[test]
fn for_in_map_key_value() {
    // two-binding form binds both the key and the looked-up value
    let result = run(stringify!(
        let m = [ "a" => 1u64, "b" => 2u64, "c" => 3u64 ];
        for k, v in m {
            ret_str("{k}={v}");
        }
    ));
    assert_all(&result, &[ "a=1".to_string(), "b=2".to_string(), "c=3".to_string() ]);
}

#[test]
fn for_in_map_key_value_primitive() {
    let result = run(stringify!(
        let m = [ 10u64 => 100u64, 20u64 => 200u64 ];
        for k, v in m {
            ret_u64(k + v);
        }
    ));
    assert_all(&result, &[ 110u64, 220 ]);
}

#[test]
fn for_in_map_key_value_mutate_during_iteration() {
    // iterating a clone lets the body mutate the map without disturbing iteration; the key and value
    // bindings reflect the snapshot taken at loop entry
    let result = run(stringify!(
        let m = [ 1u64 => 10u64, 2u64 => 20u64, 3u64 => 30u64 ];
        for k, v in m {
            m.insert(k, v + 1u64);
            ret_u64(m[k]);
        }
    ));
    assert_all(&result, &[ 11u64, 21, 31 ]);
}

#[test]
fn for_in_map_value_remove_unvisited_during_iteration() {
    // Iterating a clone lets the body remove a not-yet-visited entry from the original without
    // disturbing iteration: the full snapshot is still visited, and the removed entry's references
    // stay alive (held by the retained clone) until the loop ends.
    let result = run(stringify!(
        let m = [ "a" => "A", "b" => "B", "c" => "C", "d" => "D" ];
        for v in m {
            ret_str(v);
            if m.contains_key("c") {
                m.remove("c");
            }
        }
    ));
    assert_all(&result, &[ "A".to_string(), "B".to_string(), "C".to_string(), "D".to_string() ]);
}

#[test]
fn for_in_map_key_value_remove_unvisited_during_iteration() {
    // as above for the two-binding form, with reference-typed keys and values
    let result = run(stringify!(
        let m = [ "a" => "A", "b" => "B", "c" => "C", "d" => "D" ];
        for k, v in m {
            ret_str("{k}={v}");
            if m.contains_key("c") {
                m.remove("c");
            }
        }
    ));
    assert_all(&result, &[ "a=A".to_string(), "b=B".to_string(), "c=C".to_string(), "d=D".to_string() ]);
}

#[test]
fn for_in_map_skips_removed() {
    let result = run(stringify!(
        let m = [ "a" => 1u64, "b" => 2u64, "c" => 3u64 ];
        m.remove("b");
        for k, _ in m {
            ret_str(k);
        }
    ));
    assert_all(&result, &[ "a".to_string(), "c".to_string() ]);
}

#[test]
fn for_in_map_empty() {
    let result = run(stringify!(
        let m = [ "a" => 1u64 ];
        m.remove("a");
        for k, _ in m {
            ret_str(k);
        }
        ret_u64(m.len());
    ));
    assert_all(&result, &[ 0u64 ]);
}

#[test]
fn for_in_map_struct_keys() {
    let result = run(stringify!(
        struct Point { x: i32, y: i32 }
        fn main() {
            let m = [ Point { x: 1, y: 2 } => 10u64, Point { x: 3, y: 4 } => 20u64 ];
            // iterate struct keys, using each as a lookup key and reading its fields
            for k, _ in m {
                ret_i32(k.x + k.y);
            }
        }
    ));
    assert_all(&result, &[ 3i32, 7 ]);
}

#[test]
fn map_grows_many_primitive_keys() {
    // insert well past the initial bucket count to force multiple table growths, then read all back
    let result = run(stringify!(
        let m = [ => ];
        for i in 0u64..100u64 {
            m[i] = i * 10u64;
        }
        ret_u64(m.len());
        ret_u64(m[0u64]);
        ret_u64(m[42u64]);
        ret_u64(m[99u64]);
    ));
    assert_all(&result, &[ 100u64, 0, 420, 990 ]);
}

#[test]
fn map_grows_string_keys() {
    let result = run(stringify!(
        let m = [ => ];
        for i in 0u64..50u64 {
            m["key{i}"] = i;
        }
        ret_u64(m.len());
        ret_u64(m["key0"]);
        ret_u64(m["key25"]);
        ret_u64(m["key49"]);
    ));
    assert_all(&result, &[ 50u64, 0, 25, 49 ]);
}

#[test]
fn map_insert_remove_churn() {
    // repeatedly insert and remove to exercise tombstoning and bucket rebuilds
    let result = run(stringify!(
        let m = [ => ];
        for i in 0u64..30u64 {
            m[i] = i;
        }
        for i in 0u64..30u64 {
            if i % 2u64 == 0u64 {
                m.remove(i);
            }
        }
        ret_u64(m.len());
        ret_u64(m[1u64]);
        ret_u64(m[29u64]);
        // re-insert some removed keys
        m[0u64] = 1000u64;
        m[2u64] = 2000u64;
        ret_u64(m.len());
        ret_u64(m[0u64]);
        ret_u64(m[2u64]);
    ));
    assert_all(&result, &[ 15u64, 1, 29, 17, 1000, 2000 ]);
}

#[test]
fn map_compaction_after_many_removes() {
    // removing most entries triggers entry-region compaction; remaining lookups and insertion-ordered
    // iteration must stay correct after entries are renumbered
    let result = run(stringify!(
        let m = [ => ];
        for i in 0u64..20u64 {
            m[i] = i * 100u64;
        }
        for i in 0u64..15u64 {
            m.remove(i);
        }
        ret_u64(m.len());
        ret_u64(m[15u64]);
        ret_u64(m[19u64]);
        for k, _ in m {
            ret_u64(k);
        }
    ));
    assert_all(&result, &[ 5u64, 1500, 1900, 15, 16, 17, 18, 19 ]);
}

#[test]
fn map_compaction_string_keys_no_leak() {
    // compaction moves boxed reference keys/values verbatim; ensure churn with reference keys stays
    // correct and leak-free
    let result = run(stringify!(
        let m = [ => ];
        for i in 0u64..20u64 {
            m["k{i}"] = i;
        }
        for i in 0u64..15u64 {
            m.remove("k{i}");
        }
        ret_u64(m.len());
        ret_u64(m["k15"]);
        ret_u64(m["k19"]);
    ));
    assert_all(&result, &[ 5u64, 15, 19 ]);
}

#[test]
fn map_remove_all_then_reuse() {
    // removing every entry compacts to empty (n_entries >= len*2 with len == 0); the map stays usable
    let result = run(stringify!(
        let m = [ "a" => 1u64, "b" => 2u64, "c" => 3u64 ];
        m.remove("a");
        m.remove("b");
        m.remove("c");
        ret_u64(m.len());
        m["x"] = 9u64;
        ret_u64(m["x"]);
        ret_u64(m.len());
    ));
    assert_all(&result, &[ 0u64, 9, 1 ]);
}

#[test]
fn map_grown_then_iterated() {
    // after growth, insertion order is still preserved by iteration over the entries region
    let result = run(stringify!(
        let m = [ => ];
        for i in 0u64..12u64 {
            m[i] = i;
        }
        let mut sum = 0u64;
        for k, _ in m {
            sum += k;
        }
        ret_u64(sum);
        ret_u64(m.len());
    ));
    assert_all(&result, &[ 66u64, 12 ]);
}

#[test]
fn map_enum_keys() {
    let result = run(stringify!(
        enum Color { Red, Green, Blue }
        fn main() {
            let m = [ Color::Red => 1u64, Color::Green => 2u64, Color::Blue => 3u64 ];
            ret_u64(m[Color::Red]);
            ret_u64(m[Color::Green]);
            ret_u64(m[Color::Blue]);
            m[Color::Green] = 20u64;
            ret_u64(m[Color::Green]);
            ret_u64(m.len());
        }
    ));
    assert_all(&result, &[ 1u64, 2, 3, 20, 3 ]);
}

#[test]
fn for_in_map_remove_during_iteration() {
    // removing the current key during iteration is supported (the loop walks a key snapshot)
    let result = run(stringify!(
        let m = [ 1u64 => 1u64, 2u64 => 2u64, 3u64 => 3u64, 4u64 => 4u64 ];
        for k, _ in m {
            if k % 2u64 == 0u64 {
                m.remove(k);
            }
        }
        ret_u64(m.len());
        ret_u64(m[1u64]);
        ret_u64(m[3u64]);
    ));
    assert_all(&result, &[ 2u64, 1, 3 ]);
}

#[test]
fn for_in_map_remove_all_during_iteration_string_keys() {
    // removing every (reference) key during iteration must stay leak-free: the snapshot keeps each key
    // alive for the duration of its iteration even after it is dropped from the map
    let result = run(stringify!(
        let m = [ "a" => 1u64, "b" => 2u64, "c" => 3u64 ];
        for k, _ in m {
            m.remove(k);
        }
        ret_u64(m.len());
    ));
    assert_all(&result, &[ 0u64 ]);
}

#[test]
fn for_in_map_insert_during_iteration_is_snapshot() {
    // inserting during iteration (which relocates the map's storage) is safe and does not extend the
    // iteration: only the keys present at loop entry are visited
    let result = run(stringify!(
        let m = [ 1u64 => 10u64, 2u64 => 20u64 ];
        let mut count = 0u64;
        for k, _ in m {
            m[k + 100u64] = 0u64;
            count += 1u64;
        }
        ret_u64(count);
        ret_u64(m.len());
    ));
    assert_all(&result, &[ 2u64, 4 ]);
}

#[test]
fn for_in_map_break_continue() {
    let result = run(stringify!(
        let m = [ 1u64 => 10u64, 2u64 => 20u64, 3u64 => 30u64, 4u64 => 40u64 ];
        for k, _ in m {
            if k == 2u64 {
                continue;
            }
            if k == 4u64 {
                break;
            }
            ret_u64(k);
        }
    ));
    assert_all(&result, &[ 1u64, 3 ]);
}
