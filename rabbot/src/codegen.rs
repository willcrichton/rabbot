use syntax::ext::base::ExtCtxt;
use syntax::ast::{Item as RsItem, Arm, Ident};
use syntax::parse::token::str_to_ident;
use syntax::parse;
use syntax::ptr::P;
use std::collections::HashSet;

use super::ast::*;

pub fn gen_sort(cx: &mut ExtCtxt, decl: Decl, sorts: &HashSet<Ident>, global_uses: &Vec<String>)
                -> Vec<P<RsItem>>
{
    let (sort_id, mut items, meta) =
        if let Decl::Sort(sort_id, items, meta) = decl { (sort_id, items, meta) }
    else { unreachable!() };

    items.insert(0, (str_to_ident("Var"), Some(Type::Ident(sort_id.clone()))));

    let sess = parse::ParseSess::new();

    let mut uses = vec![quote_item!(cx, use std::collections::HashSet;).unwrap()];
    for id in sorts.iter() {
        let module = ident_to_lower(id);
        uses.push(quote_item!(cx, use super::$module;).unwrap());
        if id != &sort_id {
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
            "#[derive(Debug, Clone)] pub enum Op {{ {} }}",
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
        variant_arms.insert(0, "Var_(Var),".to_string());
        let main_variant = format!(
            "#[derive(Debug, Clone)] pub enum View {{ {} }}",
            variant_arms.join("\n"));

        parse::parse_item_from_source_str("".to_string(), main_variant, vec![], &sess)
            .unwrap().unwrap()
    };


    let st = &mut State {
        sess: &sess,
        sorts: sorts.clone(),
        meta: meta.clone(),
        sort_id: sort_id.clone(),
    };

    let oper_bind = {
        let arms: Vec<Arm> = items.iter().map(|&(ref name, ref item)| {
            match item {
                &Some(ref item) => item.to_ops_arm(cx, name, st, true),
                &None => {
                    let node = st.build_node(quote_expr!(cx, Op::$name));
                    quote_arm!(cx, Op::$name => { $node })
                }
            }
        }).collect();

        quote_item!(
            cx,
            pub fn oper_bind(x: Var, i: i32, node: Meta<Op>) -> Meta<Op> {
                match node.val {
                    $arms
                }
            }).unwrap()
    };

    let oper_unbind = {
        let arms: Vec<Arm> = items.iter().map(|&(ref name, ref item)| {
            match item {
                &Some(ref item) => item.to_ops_arm(cx, name, st, false),
                &None => {
                    let node = st.build_node(quote_expr!(cx, Op::$name));
                    quote_arm!(cx, Op::$name => { $node })
                }
            }
        }).collect();

        quote_item!(
            cx,
            pub fn oper_unbind(x: Var, i: i32, node: Meta<Op>) -> Meta<Op> {
                match node.val {
                    $arms
                }
            }).unwrap()
    };

    let meta_def = {
        let arms = meta.iter().map(|&(ref key, ref value)| {
            format!("pub {}: {}", key.name.as_str(), value.name.as_str())
        }).collect::<Vec<String>>().join(",\n");

        let node = format!("#[derive(Clone, Default)] pub struct Meta<T> {{ pub val: T, \n {} }}", arms);

        parse::parse_item_from_source_str("".to_string(), node, vec![], &sess)
            .unwrap().unwrap()
    };

    let alias = quote_item!(
        cx,
        pub type $sort_id = Abt<Meta<Op>>;
        ).unwrap();

    let view_in = {
        let mut arms: Vec<Arm> = items.iter().map(|&(ref name, ref item)| {
            match item {
                &Some(ref item) => item.to_view_in_arm(cx, st, name),
                &None => {
                    let node = st.build_node(quote_expr!(cx, Op::$name));
                    quote_arm!(cx, View::$name => { AbtView::Oper($node) })
                }
            }
        }).collect();

        arms.insert(0, {
            quote_arm!(cx, View::Var_(x) => { AbtView::Var(x) })
        });

        quote_item!(
            cx,
            fn view_in(vars: Vec<Var>, node: Meta<View>) -> AbtView<Meta<Op>> {
                match node.val {
                    $arms
                }
            }).unwrap()
    };

    let oper_view_out = {
        let arms: Vec<Arm> = items.iter().map(|&(ref name, ref item)| {
            match item {
                &Some(ref item) => item.to_view_out_arm(cx, st, name),
                &None => {
                    let node = st.build_node(quote_expr!(cx, View::$name));
                    quote_arm!(cx, Op::$name => { $node })
                }
            }
        }).collect();

        quote_item!(
            cx,
            fn oper_view_out(vars: Vec<Var>, node: Meta<Op>) -> Meta<View> {
                match node.val {
                    $arms
                }
            }).unwrap()
    };

    let out = quote_item!(
        cx,
        pub fn out(term: $sort_id) -> Meta<View> {
            match Abt::out(box oper_unbind, term) {
                AbtView::Var(_) => panic!("Out called on a Var"),
                AbtView::Oper(t) => oper_view_out(vec![], t),
                _ => panic!("Invalid out")
            }
        }).unwrap();

    let into = quote_item!(
        cx,
        pub fn into(v: Meta<View>) -> $sort_id {
            Abt::into(box oper_bind, view_in(vec![], v))
        }).unwrap();

    let subst = {
        let mut arms: Vec<Arm> = items.iter().map(|&(ref name, ref item)| {
            match item {
                &Some(ref item) => item.to_subst_arm(cx, st, name),
                &None => {
                    let e = st.build_node(quote_expr!(cx, View::$name));
                    quote_arm!(cx, View::$name => { into($e) })
                }
            }
        }).collect();

        let e = st.build_node(quote_expr!(cx, View::Var_(var)));
        arms.insert(0, quote_arm!(cx, View::Var_(var) => {
            if var == x {
                t
            } else {
                into($e)
            }
        }));

        quote_item!(
            cx,
            pub fn subst(t: $sort_id, x: Var, term: $sort_id) -> $sort_id {
                let node = out(term);
                match node.val {
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

        arms.insert(0, quote_arm!(cx, View::Var_(var) => { unreachable!() }));

        quote_item!(
            cx,
            fn free_vars_helper(t: $sort_id, bound: HashSet<Var>) -> HashSet<Var> {
                match out(t).val {
                    $arms
                }
            }).unwrap()
    };

    let free_vars = quote_item!(
        cx,
        pub fn free_vars(t: $sort_id) -> HashSet<Var> {
            free_vars_helper(t, HashSet::new())
        }).unwrap();

    let oper_aequiv = {
        let arms: Vec<Arm> = items.iter().map(|&(ref name, ref item)| {
            match item {
                &Some(ref item) => item.to_aequiv_arm(cx, sorts, name),
                &None => quote_arm!(cx, (Op::$name, Op::$name) => { true })
            }
        }).collect();

        quote_item!(
            cx,
            fn oper_aequiv(t1: Meta<Op>, t2: Meta<Op>) -> bool {
                match (t1.val, t2.val) {
                    $arms
                    _ => false
                }
            })
    };

    let aequiv = quote_item!(
        cx,
        pub fn aequiv(t1: $sort_id, t2: $sort_id) -> bool {
            Abt::aequiv(box oper_aequiv, t1, t2)
        });

    let var = {
        let e = st.build_node(quote_expr!(cx, View::Var_(node.val)));
        quote_item!(
            cx,
            pub fn var(node: Meta<Var>) -> View {
                View::Var(into($e))
            })
    };

    let (default1, default2) = {
        let mut default = None;
        for &(ref name, ref item) in items.iter() {
            match item {
                &None => {
                    default = Some(name.clone());
                    break;
                },
                _ => { continue; }
            }
        }
        let default = default.expect("Enum must have one arm with no fields");
        (quote_item!(
            cx,
            impl Default for Op {
                fn default() -> Op { Op::$default }
            }),
         quote_item!(
             cx,
             impl Default for View {
                 fn default() -> View { View::$default }
             }))
    };

    let sort_id_lower = ident_to_lower(&sort_id);
    let module = quote_item!(
        cx,
        #[allow(unused_variables, dead_code, unused_imports)]
        pub mod $sort_id_lower {
            use rabbot::abt::{Abt, View as AbtView};
            use rabbot::var::Var;
            use std::fmt;
            $uses
                $ops_variant
                $main_variant
                $meta_def
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
                $oper_aequiv
                $aequiv
                $var
                $default1
                $default2
                pub fn extract_var(t: $sort_id) -> Var {
                    match Abt::out(box oper_unbind, t) {
                        AbtView::Var(v) => v,
                        _ => unreachable!()
                    }
                }
            pub fn into_view(v: View) -> $sort_id {
                into(Meta { val: v, ..Default::default() })
            }
            impl<T: fmt::Debug> fmt::Debug for Meta<T> {
                fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                    self.val.fmt(f)
                }
            }
        }).unwrap();

    vec![module]
}

pub fn gen_decls(cx: &mut ExtCtxt, ast: Vec<Decl>) -> Vec<P<RsItem>> {
    let (uses, sorts): (Vec<Decl>, Vec<Decl>) =
        ast.into_iter().partition(|node| match node {
            &Decl::Use(_) => true,
            _ => false
        });

    let uses = uses.into_iter().map(|node| {
        if let Decl::Use(path) = node {
            path
        } else {
            unreachable!()
        }
    }).collect::<Vec<String>>();

    let mut sort_ids = HashSet::new();
    for node in sorts.iter() {
        if let &Decl::Sort(ref id, _, _) = node {
            sort_ids.insert(id.clone());
        } else {
            unreachable!()
        }
    }

    sorts.into_iter().flat_map(|ast| gen_sort(cx, ast, &sort_ids, &uses)).collect()
}
