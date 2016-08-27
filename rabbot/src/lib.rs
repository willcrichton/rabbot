#![feature(rustc_private, plugin_registrar, quote, box_syntax, box_patterns)]

extern crate rustc;
extern crate rustc_plugin;
extern crate syntax;
extern crate syntax_pos;

pub mod token;
pub mod grammar;
pub mod ast;
pub mod codegen;
pub mod abt;
pub mod var;

#[macro_export]
macro_rules! bind {
    ($variant:path { $args:tt } = $e:expr) => {
        let $args = match $e {
            $variant($args) => { $args },
            _ => unreachable!()
        };
    }
}
