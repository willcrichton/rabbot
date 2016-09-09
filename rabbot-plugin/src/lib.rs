#![feature(rustc_private, plugin_registrar)]

extern crate rustc_plugin;
extern crate rabbot;
extern crate syntax;

use rustc_plugin::Registry;
use syntax::codemap::Span;
use syntax::parse::token::{Token as RsToken};
use syntax::ext::base::{ExtCtxt, MacResult, MacEager};
use syntax::tokenstream::TokenTree;
use syntax::util::small_vector::SmallVector as Svec;

use rabbot::token::Token;
use rabbot::grammar::{parse_Decls as parse};
use rabbot::codegen;

fn tt_flatten(tt: &TokenTree) -> Vec<RsToken> {
    match tt {
        &TokenTree::Token(_, ref t) => vec![t.clone()],
        &TokenTree::Delimited(_, ref delim) => {
            let mut toks: Vec<RsToken> =
                delim.tts.iter().flat_map(tt_flatten).collect();
            toks.insert(0, RsToken::OpenDelim(delim.delim));
            toks.push(RsToken::CloseDelim(delim.delim));
            toks
        },
        _ => panic!("Unreachable")
    }
}

#[allow(unused_variables)]
pub fn expand_rabbot(cx: &mut ExtCtxt, sp: Span, args: &[TokenTree]) ->
    Box<MacResult + 'static>
{
    let tokens: Vec<Token> =
        args
        .into_iter()
        .flat_map(tt_flatten)
        .map(Token::from_rust_token)
        .collect();
    //println!("{:?}", tokens);

    let ast = parse(tokens).unwrap();
    //println!("{:?}", ast);

    MacEager::items(Svec::many(codegen::gen_decls(cx, ast)))
}

#[plugin_registrar]
pub fn plugin_registrar(reg: &mut Registry) {
    reg.register_macro("rabbot", expand_rabbot);
}
