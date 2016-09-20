#![feature(plugin, box_syntax)]
#![plugin(rabbot_plugin)]

#[macro_use] extern crate rabbot;

rabbot! {
    enum Term {mark: i32} {
        A,
        B(i32)
    }
}
