use syntax::ast::Ident;
use syntax::ast::{Expr, Arm, Pat, Path};
use syntax::ptr::P;
use syntax::ext::base::ExtCtxt;
use syntax::parse::token::{str_to_ident, Token as RsToken};
use syntax::tokenstream::TokenTree;
use syntax::ext::quote::rt::ToTokens;
use syntax_pos::DUMMY_SP;
use std::collections::HashMap;

pub type Decl = (Ident, Vec<Item>);

pub type Item = (Ident, Option<Type>);

#[derive(Debug, Clone, Hash)]
pub enum Type {
    Ident(Ident),
    Prod(Vec<Type>),
    Var(Box<Type>, Box<Type>),
    Bind(Ident),
}

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

impl Type {
    pub fn to_ops_arm(&self, cx: &mut ExtCtxt, name: &Ident) -> Arm {
        let (pat, e) = self.to_ops_arm_helper(cx, &mut IdGenerator::new());
        quote_arm!(cx, $name($pat) => { $name($e) })
    }

    fn to_ops_arm_helper(&self, cx: &mut ExtCtxt, generator: &mut IdGenerator)
                         -> (P<Pat>, P<Expr>)
    {
        match self {
            &Type::Ident(ref id) => {
                let new_id = generator.gen();
                (quote_pat!(cx, $new_id),
                 quote_expr!(cx, ($new_id.bind(term_oper_bind, x, i))))
            },
            &Type::Prod(ref tys) => {
                let (pats, exprs): (Vec<P<Pat>>, Vec<P<Expr>>) =
                    tys.iter().map(|ty| ty.to_ops_arm_helper(cx, generator)).unzip();
                let pats = token_separate(cx, &pats, RsToken::Comma);
                let exprs = token_separate(cx, &exprs, RsToken::Comma);
                (quote_pat!(cx, ($pats)), quote_expr!(cx, ($exprs)))
            },
            &Type::Var(box ref l, box ref r) => {
                Type::Prod(vec![l.clone(), r.clone()]).to_ops_arm_helper(cx, generator)
            },
            &Type::Bind(ref id) => {
                let new_id = generator.gen();
                (quote_pat!(cx, $new_id), quote_expr!(cx, $new_id))
            },
        }
    }

    pub fn to_view_arm(&self, cx: &mut ExtCtxt, path: Path) -> Arm {
        let (pat, e) = self.to_view_arm_helper(cx, &mut IdGenerator::new());
        quote_arm!(cx, $path($pat) => { $e })
    }

    fn to_view_arm_helper(&self, cx: &mut ExtCtxt, generator: &mut IdGenerator)
                          -> (P<Pat>, P<Expr>)
    {
        match self {
            &Type::Ident(ref id) => {
                let new_id = generator.gen();
                (quote_pat!(cx, $new_id),
                 quote_expr!(cx, {
                     let mut abt = $new_id;
                     for var in vars {
                         abt = Abt::into(term_oper_bind, Abt::Binding(var, abt));
                     }
                     abt
                 }))
            },
            &Type::Prod(ref tys) => {
                let (pats, exprs): (Vec<P<Pat>>, Vec<P<Expr>>) =
                    tys.iter().map(|ty| ty.to_view_arm_helper(cx, generator)).unzip();
                let pats = token_separate(cx, &pats, RsToken::Comma);
                let exprs = token_separate(cx, &exprs, RsToken::Comma);
                (quote_pat!(cx, ($pats)), quote_expr!(cx, ($exprs)))
            },
            &Type::Var(box ref l, box ref r) => {
                let (lpat, lexpr) = l.to_view_arm_helper(cx, generator);
                let (rpat, rexpr) = r.to_view_arm_helper(cx, generator);
                (quote_pat!(cx, ($lpat, $rpat)),
                 quote_expr!(cx, {
                     let (t, vars1) = $lexpr;
                     let mut vars = vars.clone();
                     vars.append(vars1);
                     $rexpr
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

    pub fn to_enum_string(&self, decl_id: &Ident) -> String {
        match self {
            &Type::Ident(ref id) => id.name.as_str().to_string(),
            &Type::Prod(ref tys) => {
                let strs: Vec<String> =
                    tys.iter().map(|ty| ty.to_enum_string(decl_id)).collect();
                format!("({})", strs.join(", "))
            },
            &Type::Var(box ref l, box ref r) =>
                Type::Prod(vec![l.clone(), r.clone()]).to_enum_string(decl_id),
            &Type::Bind(_) => "Var".to_string()
        }
    }

    pub fn to_ops_enum_string(&self, decl_id: &Ident, ops_id: &Ident) -> String {
        match self {
            &Type::Ident(ref id) =>
                if decl_id == id {
                    format!("Abt<{}>", ops_id.name.as_str())
                } else {
                    id.name.as_str().to_string()
                },
            &Type::Prod(ref tys) => {
                let strs: Vec<String> =
                    tys.iter().map(|ty| ty.to_ops_enum_string(decl_id, ops_id)).collect();
                format!("({})", strs.join(", "))
            },
            &Type::Var(box ref l, box ref r) =>
                Type::Prod(vec![l.clone(), r.clone()]).to_ops_enum_string(decl_id, ops_id),
            &Type::Bind(_) => "String".to_string()
        }
    }
}
