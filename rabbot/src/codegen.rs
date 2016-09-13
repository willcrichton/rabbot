use syntax::ext::base::ExtCtxt;
use syntax::ast::{Item as RsItem, Arm, Ident};
use syntax::parse;
use syntax::ptr::P;
use std::collections::HashSet;

use super::ast::*;

pub fn gen_sort(cx: &mut ExtCtxt, decl: Decl, sorts: &HashSet<Ident>, global_uses: &Vec<String>)
                -> Vec<P<RsItem>>
{
    let (sort_id, items) =
        if let Decl::Sort(sort_id, items) = decl { (sort_id, items) }
    else { unreachable!() };

    let sess = parse::ParseSess::new();

    let mut uses = vec![quote_item!(cx, use std::collections::HashSet;).unwrap()];
    for id in sorts.iter() {
        if id != &sort_id {
            let module = ident_to_lower(id);
            uses.push(quote_item!(cx, use super::$module::$id;).unwrap())
        }
    }

    for path in global_uses.iter() {
        let tt =
            parse::parse_tts_from_source_str("".to_string(), path.clone(), vec![], &sess)
            .unwrap();
        uses.push(quote_item!(cx, use $tt;).unwrap());
    }

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

    let free_vars_helper = {
        let mut arms: Vec<Arm> = items.iter().map(|&(ref name, ref item)| {
            match item {
                &Some(ref item) => item.to_free_vars_arm(cx, sorts, name),
                &None => quote_arm!(cx, View::$name => { HashSet::new() })
            }
        }).collect();

        arms.insert(0, quote_arm!(cx, View::Var(var) => {
            let mut hs = HashSet::new();
            if !bound.contains(&var) { hs.insert(var); }
            hs
        }));

        quote_item!(
            cx,
            fn free_vars_helper(t: $sort_id, bound: HashSet<Var>) -> HashSet<Var> {
                match out(t) {
                    $arms
                }
            }).unwrap()
    };

    let free_vars = quote_item!(
        cx,
        pub fn free_vars(t: $sort_id) -> HashSet<Var> {
            free_vars_helper(t, HashSet::new())
        }).unwrap();

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
                $free_vars_helper
                $free_vars
        }).unwrap();

    vec![module]
}

pub fn gen_decls(cx: &mut ExtCtxt, ast: Vec<Decl>) -> Vec<P<RsItem>> {
    let (uses, sorts): (Vec<Decl>, Vec<Decl>) =
        ast.into_iter().partition(|node| match node {
            &Decl::Use(_) => true,
            _ => false
        });


    let mut uses = uses.into_iter().map(|node| {
        if let Decl::Use(path) = node {
            path
        } else {
            unreachable!()
        }
    }).collect::<Vec<String>>();

    let mut sort_ids = HashSet::new();
    for node in sorts.iter() {
        if let &Decl::Sort(ref id, _) = node {
            sort_ids.insert(id.clone());
        } else {
            unreachable!()
        }
    }


    sorts.into_iter().flat_map(|ast| gen_sort(cx, ast, &sort_ids, &uses)).collect()
}
