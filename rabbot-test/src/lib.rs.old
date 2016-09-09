#![feature(plugin, box_syntax)]
#![plugin(rabbot_plugin)]

#[macro_use] extern crate rabbot;

rabbot! {
    sort Term {
        Z,
        S(Term),
        Lam(Binding<Term> . Term),
        App((Term, Term))
    }
}

use rabbot::var::Var;
use term::*;
fn interpret(t: Term) -> Term {
    match out(t) {
        v @ View::Z | v @ View::Lam(_) => into(v),
        View::S(t) => {
            bind!(View::S{v} = out(interpret(t)));
            into(View::S(v))
        },
        View::App((fun, arg)) => {
            bind!(View::Lam{(var, body)} = out(interpret(fun)));
            subst(arg, var, body)
        },
        View::Var(_) => unreachable!()
    }
}

#[test]
fn test() {
    let x = Var::new("x".to_string());
    let term = into(View::App((
        into(View::Lam((
            x.clone(), into(View::S(into(View::Var(x.clone()))))))),
        into(View::Z))));

    println!("{:?}", interpret(term)); // prints S(Z)
}
