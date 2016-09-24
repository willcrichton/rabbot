#![feature(plugin, box_syntax)]
#![plugin(rabbot_plugin)]

#[macro_use] extern crate rabbot;

rabbot! {
    enum Typ {
        A
    }

    enum Term {mark: i32} {
        B(Typ)
    }
}
