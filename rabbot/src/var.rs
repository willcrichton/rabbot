use std::cell::RefCell;
use std::fmt;

#[derive(Eq, PartialEq, Clone, Hash)]
pub struct Var {
    name: String
}

thread_local! {
    static COUNT: RefCell<i32> = RefCell::new(0);
}

impl Var {
    pub fn from_string(name: String) -> Var {
        Var {
            name: name
        }
    }

    pub fn new() -> Var {
        let name =
            COUNT.with(|c| {
                let s = format!("t{}", *c.borrow());
                *c.borrow_mut() += 1;
                s
            });
        Var::from_string(name)
    }

    pub fn to_string(&self) -> String {
        self.name.clone()
    }
}

impl fmt::Display for Var {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.name.fmt(f)
    }
}

impl fmt::Debug for Var {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.name.fmt(f)
    }
}
