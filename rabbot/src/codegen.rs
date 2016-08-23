use syntax::ext::base::ExtCtxt;
use syntax::ast::{Item as RsItem, Arm};
use syntax::parse;
use syntax::parse::token::str_to_ident;
use syntax::ptr::P;

use super::ast::*;

pub fn gen_decl(cx: &mut ExtCtxt, (decl_id, items): Decl) -> Vec<P<RsItem>> {
    let ops_id =
        str_to_ident(format!("{}{}", decl_id.name.as_str(), "Ops").as_str());
    let view_id =
        str_to_ident(format!("{}{}", decl_id.name.as_str(), "View").as_str());
    let sess = parse::ParseSess::new();

    let ops_variant = {
        let mut variant_arms: Vec<String> =
            items.iter().map(|&(ref name, ref item)| {
                match item {
                    &None =>
                        format!("{},", name.name.as_str()),
                    &Some(ref item) =>
                        format!("{}({}),", name.name.as_str(), item.to_ops_enum_string(&decl_id, &ops_id))
                }
            }).collect();
        let ops_variant = format!(
            "enum {} {{ {} }}",
            ops_id.name.as_str(),
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
                        format!("{}({}),", name.name.as_str(), item.to_enum_string(&decl_id))
                }
            }).collect();
        let main_variant = format!(
            "enum {} {{ {} }}",
            view_id.name.as_str(),
            variant_arms.join("\n"));

        parse::parse_item_from_source_str("".to_string(), main_variant, vec![], &sess)
            .unwrap().unwrap()
    };

    let oper_bind = {
        let arms: Vec<Arm> = items.iter().map(|&(ref name, ref item)| {
            match item {
                &Some(ref item) => item.to_ops_arm(cx, name),
                &None => quote_arm!(cx, $name => { $name })
            }
        }).collect();

        quote_item!(
            cx,
            fn term_oper_bind<T>(x: Var, i: i32, term: T) -> TermOps {
                use $ops_id::*;
                match term {
                    $arms
                }
            }).unwrap()
    };

    let alias = quote_item!(
        cx,
        type Term = Abt<TermOps>;
        ).unwrap();

    let view_in = {
        let arms: Vec<Arm> = items.iter().map(|&(ref name, ref item)| {
            let path = quote_path!(cx, $view_id::$name);
            match item {
                &Some(ref item) => item.to_view_arm(cx, path),
                &None => quote_arm!(cx, $path => { View::Oper($ops_id::$name) })
            }
        }).collect();

        quote_item!(
            cx,
            fn view_in(vars: Vec<Var>, term: $view_id) -> Term {
                match term {
                    $arms
                }
            }).unwrap()
    };

    vec![ops_variant, main_variant, alias, oper_bind, view_in]
}

pub fn gen_decls(cx: &mut ExtCtxt, ast: Vec<Decl>) -> Vec<P<RsItem>> {
    ast.into_iter().flat_map(|ast| gen_decl(cx, ast)).collect()
}
