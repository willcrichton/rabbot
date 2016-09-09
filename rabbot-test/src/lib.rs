#![feature(plugin, box_syntax)]
#![plugin(rabbot_plugin)]

#[macro_use] extern crate rabbot;

rabbot! {
    sort Term {
        Z,
        L(Vec<Term>)
    }
}
