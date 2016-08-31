#![feature(plugin, box_syntax)]
#![plugin(rabbot_plugin)]

#[macro_use] extern crate rabbot;

rabbot! {
    sort Typ {
        Nat
    }

    sort Term {
        Z,
        S(Term),
        Lam((Binding<Term>, Typ) . Term),
        App((Term, Term))
    }
}
