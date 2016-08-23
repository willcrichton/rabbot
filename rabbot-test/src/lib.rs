#![feature(plugin)]
#![plugin(rabbot_plugin)]

extern crate rabbot;

use rabbot::abt::*;
use rabbot::var::Var;


rabbot! {
    sort Term {
        Z,
        S(Term),
        Lam(Binding<Term> . Term),
        App((Term, Term))
    }
}

#[cfg(test)]
mod tests {

    #[test]
    fn it_works() {
    }
}
