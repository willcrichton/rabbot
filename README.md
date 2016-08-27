# rabbot

Rabbot generates interfaces to abstract binding trees (ABTs). It is a port of SML's [Abbot](https://github.com/robsimmons/abbot) to Rust.

## What is an ABT?

An abstract binding tree is a data structure similar to an abstract syntax tree, except that it lets you pleasantly and correctly handle the declaration and binding of symbols. To motivate this, let's consider a syntax tree for a simple pseudo-language. For the expression `2 + 1`, this would produce a syntax tree `Plus(Number(2), Number(1))`. For the expression `let x = 1 in x + 2`, this would produce `Let(Var("x"), Number(1), Plus(Var("x"), Number(2))`. For these examples, using a normal abstract syntax tree works fine.

However, we run into trouble if we introduce multiple bindings with the same name. For example, in the expression `let x = 1 in (let x = 2 in x) + x`, we now need some notion of scope analysis. Which `x` does each instance refer to? ABTs eliminate this problem by providing constructs to handle the binding and substitution of variables. The prior expression translates roughly to `Let(1, x . (Let(2, x . x) + x))` where the dot `.` denotes a binding site. See Bob Harper's [Practical Foundations for Programming Languages](https://www.cs.cmu.edu/~rwh/pfpl/2nded.pdf), chapter 1 for more information.

## What does Rabbot do?

The usual way to create an abstract syntax is tree is to define an enum with branches for each part of your syntax, e.g.

```rust
enum Expression {
    Number(i32),
    Plus(Box<Expression>, Box<Expression>),
    ...
}
```

However, ABTs work differently. At its core, an ABT is a fixed tree data structure that is parameterized by a set of operators, where each operator describes functionality like `Number` or `Plus`. See `rabbot/src/abt.rs` for the definition of the data type. This lets you easily re-use the ABT structure for different kinds of programs, but it requires more boilerplate and more code in practice to use.

To alleviate the verbosity of using ABTs, Rabbot takes a description of an abstract binding tree and generates a more concise interface for the programmer to use. For example, one can implement lambda calculus with Peano numerals and its interpreter in a few lines:

```rust
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
```

## Caveats

This is still in heavy development, so talk to me (email: [wcrichto@stanford.edu](mailto:wcrichto@stanford.edu)) if you want to use it. The code generator works as a compiler plugin, so it only works on nightly, although that's not a hard requirement.