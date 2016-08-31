use syntax::ext::base::ExtCtxt;
use syntax::ast::{Item as RsItem, Arm, Ident};
use syntax::parse;
use syntax::ptr::P;
use std::collections::HashSet;

use super::ast::*;

pub fn gen_decl(cx: &mut ExtCtxt, (sort_id, items): Decl, sorts: &HashSet<Ident>) -> Vec<P<RsItem>> {
    let mut uses = vec![];
    for id in sorts.iter() {
        if id != &sort_id {
            let module = ident_to_lower(id);
            uses.push(quote_item!(cx, use super::$module::$id;).unwrap())
        }
    }

    let sess = parse::ParseSess::new();

    let ops_variant = {
        let variant_arms: Vec<String> =
            items.iter().map(|&(ref name, ref item)| {
                match item {
                    &None =>
                        format!("{},", name.name.as_str()),
                    &Some(ref item) =>
                        format!("{}({}),", name.name.as_str(), item.to_ops_enum_string(&sort_id))
                }
            }).collect();
        let ops_variant = format!(
            "#[derive(Debug, Clone)] pub enum Ops {{ {} }}",
            variant_arms.join("\n"));

        parse::parse_item_from_source_str("".to_string(), ops_variant, vec![], &sess)
            .unwrap().unwrap()
    };

    let main_variant = {
        let mut variant_arms: Vec<String> =
            items.iter().map(|&(ref name, ref item)| {
                match item {
                    &None =>
                        format!("{},", name.name.as_str()),
                    &Some(ref item) =>
                        format!("{}({}),", name.name.as_str(), item.to_enum_string(&sort_id))
                }
            }).collect();
        variant_arms.insert(0, "Var(Var),".to_string());
        let main_variant = format!(
            "#[derive(Debug, Clone)] pub enum View {{ {} }}",
            variant_arms.join("\n"));

        parse::parse_item_from_source_str("".to_string(), main_variant, vec![], &sess)
            .unwrap().unwrap()
    };

    let oper_bind = {
        let arms: Vec<Arm> = items.iter().map(|&(ref name, ref item)| {
            match item {
                &Some(ref item) => item.to_ops_arm(cx, name, sorts, true),
                &None => quote_arm!(cx, Ops::$name => { Ops::$name })
            }
        }).collect();

        quote_item!(
            cx,
            pub fn oper_bind(x: Var, i: i32, term: Ops) -> Ops {
                match term {
                    $arms
                }
            }).unwrap()
    };

    let oper_unbind = {
        let arms: Vec<Arm> = items.iter().map(|&(ref name, ref item)| {
            match item {
                &Some(ref item) => item.to_ops_arm(cx, name, sorts, false),
                &None => quote_arm!(cx, Ops::$name => { Ops::$name })
            }
        }).collect();

        quote_item!(
            cx,
            pub fn oper_unbind(x: Var, i: i32, term: Ops) -> Ops {
                match term {
                    $arms
                }
            }).unwrap()
    };

    let alias = quote_item!(
        cx,
        pub type $sort_id = Abt<Ops>;
        ).unwrap();

    let view_in = {
        let mut arms: Vec<Arm> = items.iter().map(|&(ref name, ref item)| {
            match item {
                &Some(ref item) => item.to_view_in_arm(cx, sorts, name),
                &None => quote_arm!(cx, View::$name => { AbtView::Oper(Ops::$name) })
            }
        }).collect();

        arms.insert(0, quote_arm!(cx, View::Var(x) => { AbtView::Var(x) }));

        quote_item!(
            cx,
            fn view_in(vars: Vec<Var>, term: View) -> AbtView<Ops> {
                match term {
                    $arms
                }
            }).unwrap()
    };

    let oper_view_out = {
        let arms: Vec<Arm> = items.iter().map(|&(ref name, ref item)| {
            match item {
                &Some(ref item) => item.to_view_out_arm(cx, sorts, name),
                &None => quote_arm!(cx, Ops::$name => { View::$name })
            }
        }).collect();

        quote_item!(
            cx,
            fn oper_view_out(vars: Vec<Var>, term: Ops) -> View {
                match term {
                    $arms
                }
            }).unwrap()
    };

    let out = quote_item!(
        cx,
        pub fn out(term: $sort_id) -> View {
            match Abt::out(box oper_unbind, term) {
                AbtView::Var(x) => View::Var(x),
                AbtView::Oper(t) => oper_view_out(vec![], t),
                _ => panic!("Invalid out")
            }
        }).unwrap();

    let into = quote_item!(
        cx,
        pub fn into(v: View) -> $sort_id {
            Abt::into(box oper_bind, view_in(vec![], v))
        }).unwrap();

    let subst = {
        let mut arms: Vec<Arm> = items.iter().map(|&(ref name, ref item)| {
            match item {
                &Some(ref item) => item.to_subst_arm(cx, &sorts, name),
                &None => quote_arm!(cx, View::$name => { into(View::$name) })
            }
        }).collect();

        arms.insert(0, quote_arm!(cx, View::Var(var) => {
            if var == x {
                t
            } else {
                into(View::Var(var))
            }
        }));

        quote_item!(
            cx,
            pub fn subst(t: $sort_id, x: Var, term: $sort_id) -> $sort_id {
                match out(term) {
                    $arms
                }
            }).unwrap()
    };

    let sort_id_lower = ident_to_lower(&sort_id);
    let module = quote_item!(
        cx,
        pub mod $sort_id_lower {
            use rabbot::abt::{Abt, View as AbtView};
            use rabbot::var::Var;
            $uses
                $ops_variant
                $main_variant
                $alias
                $oper_bind
                $oper_unbind
                $view_in
                $oper_view_out
                $out
                $into
                $subst
        }).unwrap();

    vec![module]
}

pub fn gen_decls(cx: &mut ExtCtxt, ast: Vec<Decl>) -> Vec<P<RsItem>> {
    let mut sorts = HashSet::new();
    for &(ref id, _) in ast.iter() {
        sorts.insert(id.clone());
    }

    ast.into_iter().flat_map(|ast| gen_decl(cx, ast, &sorts)).collect()
}
