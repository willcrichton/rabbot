use syntax::ast::Ident;
use syntax::ast::{Expr, Arm, Pat, Path};
use syntax::ptr::P;
use syntax::ext::base::ExtCtxt;
use syntax::parse::token::{str_to_ident, Token as RsToken};
use syntax::tokenstream::TokenTree;
use syntax::ext::quote::rt::ToTokens;
use syntax_pos::DUMMY_SP;

pub fn token_separate<T: ToTokens>(ecx: &ExtCtxt, things: &[T],
                                   token: RsToken) -> Vec<TokenTree> {
    let mut output: Vec<TokenTree> = vec![];
    for (i, thing) in things.iter().enumerate() {
        output.extend(thing.to_tokens(ecx));
        if i < things.len() - 1 {
            output.push(TokenTree::Token(DUMMY_SP, token.clone()));
        }
    }
    output
}

struct IdGenerator {
    count: i32
}

impl IdGenerator {
    pub fn new() -> IdGenerator {
        IdGenerator {
            count: 0
        }
    }

    pub fn gen(&mut self) -> Ident {
        let new_id = str_to_ident(format!("var{}", self.count).as_str());
        self.count += 1;
        new_id
    }
}

pub struct Sort {
    base_id: Ident
}

impl Sort {
    pub fn new(base_id: Ident) -> Sort {
        Sort {
            base_id: base_id
        }
    }

    pub fn base_id(&self) -> Ident {
        self.base_id.clone()
    }

    pub fn base_id_lower(&self) -> Ident {
        str_to_ident(self.base_id.name.as_str().to_lowercase().as_str())
    }

    pub fn ops_id(&self) -> Ident {
        str_to_ident(format!("{}{}", self.base_id.name.as_str(), "Ops").as_str())
    }

    pub fn view_id(&self) -> Ident {
        str_to_ident(format!("{}{}", self.base_id.name.as_str(), "View").as_str())
    }
}

pub type Decl = (Ident, Vec<Item>);

pub type Item = (Ident, Option<Type>);

#[derive(Debug, Clone, Hash)]
pub enum Type {
    Ident(Ident),
    Prod(Vec<Type>),
    Var(Box<Type>, Box<Type>),
    Bind(Ident),
}

impl Type {
    pub fn to_ops_arm(&self, cx: &mut ExtCtxt, name: &Ident, bind: bool) -> Arm {
        let (pat, e) = self.to_ops_arm_helper(cx, &mut IdGenerator::new(), bind);
        quote_arm!(cx, Ops::$name($pat) => { Ops::$name($e) })
    }

    fn to_ops_arm_helper(&self, cx: &mut ExtCtxt, generator: &mut IdGenerator, bind: bool)
                         -> (P<Pat>, P<Expr>)
    {
        match self {
            &Type::Ident(ref id) => {
                let new_id = generator.gen();
                (quote_pat!(cx, $new_id),
                 if bind {
                     quote_expr!(cx, (Abt::bind(box term_oper_bind, x.clone(), i, $new_id)))
                 } else {
                     quote_expr!(cx, (Abt::unbind(box term_oper_unbind, x.clone(), i, $new_id)))
                 })
            },
            &Type::Prod(ref tys) => {
                let (pats, exprs): (Vec<P<Pat>>, Vec<P<Expr>>) =
                    tys.iter().map(|ty| ty.to_ops_arm_helper(cx, generator, bind)).unzip();
                let pats = token_separate(cx, &pats, RsToken::Comma);
                let exprs = token_separate(cx, &exprs, RsToken::Comma);
                (quote_pat!(cx, ($pats)), quote_expr!(cx, ($exprs)))
            },
            &Type::Var(box ref l, box ref r) => {
                Type::Prod(vec![l.clone(), r.clone()]).to_ops_arm_helper(cx, generator, bind)
            },
            &Type::Bind(ref id) => {
                let new_id = generator.gen();
                (quote_pat!(cx, $new_id), quote_expr!(cx, $new_id))
            },
        }
    }

    pub fn to_view_in_arm(&self, cx: &mut ExtCtxt, sort: &Sort, name: &Ident) -> Arm {
        let view_id = sort.view_id();
        let (pat, e) = self.to_view_in_arm_helper(cx, &mut IdGenerator::new());
        quote_arm!(cx, View::$name($pat) => { AbtView::Oper(Ops::$name($e)) })
    }

    fn to_view_in_arm_helper(&self, cx: &mut ExtCtxt, generator: &mut IdGenerator)
                          -> (P<Pat>, P<Expr>)
    {
        match self {
            &Type::Ident(ref id) => {
                let new_id = generator.gen();
                (quote_pat!(cx, $new_id),
                 quote_expr!(cx, {
                     let mut abt = $new_id;
                     for var in vars.iter() {
                         abt = Abt::into(box term_oper_bind, AbtView::Binding(var.clone(), abt));
                     }
                     abt
                 }))
            },
            &Type::Prod(ref tys) => {
                let (pats, exprs): (Vec<P<Pat>>, Vec<P<Expr>>) =
                    tys.iter().map(|ty| ty.to_view_in_arm_helper(cx, generator)).unzip();
                let pats = token_separate(cx, &pats, RsToken::Comma);
                let exprs = token_separate(cx, &exprs, RsToken::Comma);
                (quote_pat!(cx, ($pats)), quote_expr!(cx, ($exprs)))
            },
            &Type::Var(box ref l, box ref r) => {
                let (lpat, lexpr) = l.to_view_in_arm_helper(cx, generator);
                let (rpat, rexpr) = r.to_view_in_arm_helper(cx, generator);
                (quote_pat!(cx, ($lpat, $rpat)),
                 quote_expr!(cx, {
                     let (t, mut vars1) = $lexpr;
                     let mut vars = vars.clone();
                     vars.append(&mut vars1);
                     (t, $rexpr)
                 }))
            },
            &Type::Bind(ref id) => {
                let new_id = generator.gen();
                (quote_pat!(cx, $new_id),
                 quote_expr!(cx, {
                     ($new_id.to_string(), vec![$new_id])
                 }))
            }
        }
    }

    pub fn to_view_out_arm(&self, cx: &mut ExtCtxt, sort: &Sort, name: &Ident) -> Arm {
        let view_id = sort.view_id();
        let (pat, e) = self.to_view_out_arm_helper(cx, &mut IdGenerator::new());
        quote_arm!(cx, Ops::$name($pat) => { View::$name($e) })
    }

    fn to_view_out_arm_helper(
        &self, cx: &mut ExtCtxt, generator: &mut IdGenerator)
                              -> (P<Pat>, P<Expr>)
    {
        match self {
            &Type::Ident(ref id) => {
                let new_id = generator.gen();
                (quote_pat!(cx, $new_id),
                 quote_expr!(cx, {
                     let mut abt = $new_id;
                     for var in vars.iter() {
                         let unbound = Abt::unbind(box term_oper_unbind, var.clone(), -1, abt);
                         abt = match Abt::out(box term_oper_unbind, unbound) {
                             AbtView::Binding(_, abt) => abt,
                             _ => unreachable!()
                         };
                     }
                     abt
                 }))
            },
            &Type::Prod(ref tys) => {
                let (pats, exprs): (Vec<P<Pat>>, Vec<P<Expr>>) =
                    tys.iter().map(|ty| ty.to_view_out_arm_helper(cx, generator)).unzip();
                let pats = token_separate(cx, &pats, RsToken::Comma);
                let exprs = token_separate(cx, &exprs, RsToken::Comma);
                (quote_pat!(cx, ($pats)), quote_expr!(cx, ($exprs)))
            },
            &Type::Var(box ref l, box ref r) => {
                let (lpat, lexpr) = l.to_view_out_arm_helper(cx, generator);
                let (rpat, rexpr) = r.to_view_out_arm_helper(cx, generator);
                (quote_pat!(cx, ($lpat, $rpat)),
                 quote_expr!(cx, {
                     let (t, mut vars1) = $lexpr;
                     let mut vars = vars.clone();
                     vars.append(&mut vars1);
                     (t, $rexpr)
                 }))
            },
            &Type::Bind(ref id) => {
                let new_id = generator.gen();
                (quote_pat!(cx, $new_id),
                 quote_expr!(cx, {
                     let x = Var::new($new_id);
                     (x.clone(), vec![x])
                 }))
            }
        }
    }

    pub fn to_enum_string(&self, base_id: &Ident) -> String {
        match self {
            &Type::Ident(ref id) => id.name.as_str().to_string(),
            &Type::Prod(ref tys) => {
                let strs: Vec<String> =
                    tys.iter().map(|ty| ty.to_enum_string(base_id)).collect();
                format!("({})", strs.join(", "))
            },
            &Type::Var(box ref l, box ref r) =>
                Type::Prod(vec![l.clone(), r.clone()]).to_enum_string(base_id),
            &Type::Bind(_) => "Var".to_string()
        }
    }

    pub fn to_ops_enum_string(&self, sort: &Sort) -> String {
        let base_id = sort.base_id();
        match self {
            &Type::Ident(ref id) => id.name.as_str().to_string(),
            &Type::Prod(ref tys) => {
                let strs: Vec<String> =
                    tys.iter().map(|ty| ty.to_ops_enum_string(sort)).collect();
                format!("({})", strs.join(", "))
            },
            &Type::Var(box ref l, box ref r) =>
                Type::Prod(vec![l.clone(), r.clone()]).to_ops_enum_string(sort),
            &Type::Bind(_) => "String".to_string()
        }
    }
}
