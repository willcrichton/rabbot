use var::Var;

#[derive(Clone, Debug)]
pub enum Oper {
    Int(i32),
    Plus,
    Let,
    Lam,
    App
}

#[derive(Clone, Debug)]
pub enum Abt<T> {
    Fv(Var),
    Bv(i32),
    Abs(String, Box<Abt<T>>),
    Oper(T),
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
            Abt::Oper(f) => Abt::Oper(bind_oper(x, i, f))
        }
    }

    pub fn unbind(unbind_oper: BindingModifier<T>, x: Var, i: i32, t: Abt<T>) -> Abt<T> {
        use self::*;
        match t {
            Abt::Fv(y) => Abt::Fv(y),
            Abt::Abs(name, box t) => Abt::Abs(name, box Abt::unbind(unbind_oper, x, i + 1, t)),
            Abt::Bv(n) => if i == n { Abt::Fv(x) } else { Abt::Bv(n) },
            Abt::Oper(f) => Abt::Oper(unbind_oper(x, i, f))
        }
    }

    pub fn into(bind_oper: BindingModifier<T>, t: View<T>) -> Abt<T> {
        match t {
            View::Var(x) => Abt::Fv(x),
            View::Binding(x, t) => Abt::Abs(x.to_string(), box Abt::bind(bind_oper, x, 0, t)),
            View::Oper(f) => Abt::Oper(f)
        }
    }

    pub fn out(unbind_oper: BindingModifier<T>, t: Abt<T>) -> View<T> {
        match t {
            Abt::Bv(_) => panic!("Bv in out"),
            Abt::Fv(x) => View::Var(x),
            Abt::Oper(f) => View::Oper(f),
            Abt::Abs(name, box t) => {
                let var = Var::new(name);
                View::Binding(var.clone(), Abt::unbind(unbind_oper, var, 0, t))
            }
        }
    }
}
