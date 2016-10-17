use std::default::Default;
use var::Var;

#[derive(Clone, Debug)]
pub enum Oper {
    Int(i32),
    Plus,
    Let,
    Lam,
    App
}

// Oper contains a boxed T to avoid infintely-sized types.

#[derive(Clone, Debug)]
pub enum Abt<T> {
    Fv(Var),
    Bv(i32),
    Abs(String, Box<Abt<T>>),
    Oper(Box<T>),
}

impl<T> Default for Abt<T> where T: Default {
    fn default() -> Abt<T> {
        let t: T = Default::default();
        Abt::Oper(box t)
    }
}

#[derive(Clone, Debug)]
pub enum View<T> {
    Var(Var),
    Binding(Var, Abt<T>),
    Oper(T),
}

type BindingModifier<T> = Box<Fn(Var, i32, T) -> T>;

impl<T> Abt<T> {
    pub fn bind(bind_oper: BindingModifier<T>, x: Var, i: i32, t: Abt<T>) -> Abt<T> {
        match t {
            Abt::Fv(y) => if x == y { Abt::Bv(i) } else { Abt::Fv(y) },
            Abt::Abs(name, box t) => Abt::Abs(name, box Abt::bind(bind_oper, x, i + 1, t)),
            Abt::Bv(n) => Abt::Bv(n),
            Abt::Oper(f) => Abt::Oper(box bind_oper(x, i, *f))
        }
    }

    pub fn unbind(unbind_oper: BindingModifier<T>, x: Var, i: i32, t: Abt<T>) -> Abt<T> {
        use self::*;
        match t {
            Abt::Fv(y) => Abt::Fv(y),
            Abt::Abs(name, box t) => Abt::Abs(name, box Abt::unbind(unbind_oper, x, i + 1, t)),
            Abt::Bv(n) => if i == n { Abt::Fv(x) } else { Abt::Bv(n) },
            Abt::Oper(f) => Abt::Oper(box unbind_oper(x, i, *f))
        }
    }

    pub fn into(bind_oper: BindingModifier<T>, t: View<T>) -> Abt<T> {
        match t {
            View::Var(x) => Abt::Fv(x),
            View::Binding(x, t) => Abt::Abs(x.to_string(), box Abt::bind(bind_oper, x, 0, t)),
            View::Oper(f) => Abt::Oper(box f)
        }
    }

    pub fn out(unbind_oper: BindingModifier<T>, t: Abt<T>) -> View<T> {
        match t {
            Abt::Bv(_) => panic!("Out on Bv"),
            Abt::Fv(x) => View::Var(x),
            Abt::Oper(f) => View::Oper(*f),
            Abt::Abs(_, box t) => {
                let var = Var::new();
                //println!("out: new var {}", var);
                View::Binding(var.clone(), Abt::unbind(unbind_oper, var, 0, t))
            }
        }
    }

    pub fn aequiv(oper_eq: Box<Fn(T, T) -> bool>, t1: Abt<T>, t2: Abt<T>) -> bool {
        match (t1, t2) {
            (Abt::Bv(i), Abt::Bv(j)) => i == j,
            (Abt::Fv(x), Abt::Fv(y)) => x == y,
            (Abt::Abs(_, box t1), Abt::Abs(_, box t2)) => Abt::aequiv(oper_eq, t1, t2),
            (Abt::Oper(box f1), Abt::Oper(box f2)) => oper_eq(f1, f2),
            _ => false
        }
    }
}
