#![feature(plugin, box_syntax)]
#![plugin(rabbot_plugin)]

extern crate rabbot;

rabbot! {
    sort Term {
        Z,
        S(Term),
        Lam(Binding<Term> . Term),
        App((Term, Term))
    }
}

#[test]
fn test() {
    use term::*;
    println!("{:?}", into(View::Z));
}
