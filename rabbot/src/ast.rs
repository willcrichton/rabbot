use syntax::ast::Ident;
use syntax::ast::{Expr, Arm, Pat, Path};
use syntax::ptr::P;
use syntax::ext::base::ExtCtxt;
use syntax::parse::token::{str_to_ident, Token as RsToken};
use syntax::tokenstream::TokenTree;
use syntax::ext::quote::rt::ToTokens;
use syntax::parse;
use syntax::print::pprust;
use syntax_pos::DUMMY_SP;
use std::collections::HashSet;

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

pub fn ident_to_lower(id: &Ident) -> Ident {
    str_to_ident(id.name.as_str().to_lowercase().as_str())
}

pub type Metadata = Vec<(Ident, Ident)>;

pub enum Decl {
    Sort(Ident, Vec<Item>, Metadata),
    Use(String),
}

pub type Item = (Ident, Option<Type>);

#[derive(Debug, Clone, Hash)]
pub enum Type {
    Ident(Ident),
    Prod(Vec<Type>),
    Var(Box<Type>, Box<Type>),
    Bind(Ident),
    Vec(Box<Type>),
}

pub struct State<'a> {
    pub sess: &'a parse::ParseSess,
    pub sorts: HashSet<Ident>,
    pub meta: Metadata,
}

impl<'a> State<'a> {
    pub fn build_node(&mut self, cx: &mut ExtCtxt, val: P<Expr>) -> P<Expr> {
        let exprs = self.meta.iter().map(|&(ref key, ref value)| {
            let key = key.name.as_str();
            format!("{}: node.{}", key, key)
        }).collect::<Vec<String>>().join(",");
        let val = pprust::expr_to_string(&val.unwrap());
        let node = format!("Meta {{ {}, val: {} }}", exprs, val);
        parse::parse_expr_from_source_str("".to_string(), node, vec![], self.sess)
            .unwrap()
    }
}

impl Type {
    pub fn to_ops_arm(&self, cx: &mut ExtCtxt, name: &Ident,
                          st: &mut State, bind: bool)
                      -> Arm
    {
        let (pat, e) = self.to_ops_arm_helper(cx, &mut IdGenerator::new(), st, bind);
        let e = quote_expr!(cx, Op::$name($e));
        let node = st.build_node(cx, e);
        quote_arm!(cx, Op::$name($pat) => { $node })
    }

    fn to_ops_arm_helper(&self, cx: &mut ExtCtxt, generator: &mut IdGenerator,
                         st: &mut State, bind: bool)
                         -> (P<Pat>, P<Expr>)
    {
        match self {
            &Type::Ident(ref id) => {
                let new_id = generator.gen();
                (quote_pat!(cx, $new_id),
                 if st.sorts.contains(id) {
                     let lower = ident_to_lower(id);
                     if bind {
                         quote_expr!(cx, (Abt::bind(box super::$lower::oper_bind, x.clone(), i, $new_id)))
                     } else {
                         quote_expr!(cx, (Abt::unbind(box super::$lower::oper_unbind, x.clone(), i, $new_id)))
                     }
                 } else {
                     quote_expr!(cx, $new_id)
                 })
            },
            &Type::Prod(ref tys) => {
                let (pats, exprs): (Vec<P<Pat>>, Vec<P<Expr>>) =
                    tys.iter().map(|ty| ty.to_ops_arm_helper(cx, generator, st, bind)).unzip();
                let pats = token_separate(cx, &pats, RsToken::Comma);
                let exprs = token_separate(cx, &exprs, RsToken::Comma);
                (quote_pat!(cx, ($pats)), quote_expr!(cx, ($exprs)))
            },
            &Type::Vec(ref ty) => {
                let (pat, expr) = ty.to_ops_arm_helper(cx, generator, st, bind);
                let new_id = generator.gen();
                (quote_pat!(cx, $new_id),
                 quote_expr!(cx, {
                     $new_id.into_iter().map(|$pat| { $expr }).collect()
                 }))
            },
            &Type::Var(box ref l, box ref r) => {
                Type::Prod(vec![l.clone(), r.clone()]).to_ops_arm_helper(cx, generator, st, bind)
            },
            &Type::Bind(ref id) => {
                let new_id = generator.gen();
                (quote_pat!(cx, $new_id), quote_expr!(cx, $new_id))
            },
        }
    }

    pub fn to_view_in_arm(&self, cx: &mut ExtCtxt, st: &mut State, name: &Ident)
                          -> Arm
    {
        let (pat, e) = self.to_view_in_arm_helper(cx, &mut IdGenerator::new(), st);
        let e = quote_expr!(cx, Op::$name({
            let (t, _): (_, Vec<Var>) = $e;
            t
        }));
        let node = st.build_node(cx, e);
        quote_arm!(cx, View::$name($pat) => { AbtView::Oper($node) })
    }

    fn to_view_in_arm_helper(&self, cx: &mut ExtCtxt, generator: &mut IdGenerator,
                             st: &mut State)
                          -> (P<Pat>, P<Expr>)
    {
        match self {
            &Type::Ident(ref id) => {
                let new_id = generator.gen();
                (quote_pat!(cx, $new_id),
                 if st.sorts.contains(id) {
                     let lower = ident_to_lower(id);
                     quote_expr!(cx, {
                         let mut abt = $new_id;
                         for var in vars.iter() {
                             abt = Abt::into(box super::$lower::oper_bind, AbtView::Binding(var.clone(), abt));
                         }
                         (abt, vec![])
                     })
                 } else {
                     quote_expr!(cx, ($new_id, vec![]))
                })
            },
            &Type::Prod(ref tys) => {
                let (pats, exprs): (Vec<P<Pat>>, Vec<P<Expr>>) =
                    tys.iter().map(|ty| ty.to_view_in_arm_helper(cx, generator, st)).unzip();
                let new_vars = generator.gen();
                let exprs: Vec<P<Expr>> = exprs.into_iter().map(|expr| {
                    let t_id = generator.gen();
                    let vars_id = generator.gen();
                    quote_expr!(cx, {
                        let ($t_id, mut $vars_id) = $expr;
                        $new_vars.append(&mut $vars_id);
                        $t_id
                    })
                }).collect();
                let pats = token_separate(cx, &pats, RsToken::Comma);
                let exprs = token_separate(cx, &exprs, RsToken::Comma);
                (quote_pat!(cx, ($pats)),
                 quote_expr!(cx, {
                     let mut $new_vars = vec![];
                     (($exprs), $new_vars)
                 }))
            },
            &Type::Vec(ref ty) => {
                let (pat, expr) = ty.to_view_in_arm_helper(cx, generator, st);
                let new_id = generator.gen();
                (quote_pat!(cx, $new_id),
                 quote_expr!(cx, {
                     let (ts, vars): (Vec<Abt<Op>>, Vec<Vec<Var>>) =
                         $new_id.into_iter().map(|$pat| {
                             $expr
                         }).unzip();
                     (ts, vars.into_iter().flat_map(|x| x).collect())
                 }))
            },
            &Type::Var(box ref l, box ref r) => {
                let (lpat, lexpr) = l.to_view_in_arm_helper(cx, generator, st);
                let (rpat, rexpr) = r.to_view_in_arm_helper(cx, generator, st);
                (quote_pat!(cx, ($lpat, $rpat)),
                 quote_expr!(cx, {
                     let (t0, vars) = $lexpr;
                     let (t1, vars0) = $rexpr;
                     ((t0, t1), vars0)
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

    pub fn to_view_out_arm(&self, cx: &mut ExtCtxt, st: &mut State, name: &Ident)
                          -> Arm
    {
        let (pat, e) = self.to_view_out_arm_helper(cx, &mut IdGenerator::new(), st);
        let e = quote_expr!(cx, View::$name({
            let (t, _): (_, Vec<Var>) = $e;
            t
        }));
        let node = st.build_node(cx, e);
        quote_arm!(cx, Op::$name($pat) => { $node })
    }

    fn to_view_out_arm_helper(&self, cx: &mut ExtCtxt, generator: &mut IdGenerator,
                             st: &mut State)
                          -> (P<Pat>, P<Expr>)
    {
        match self {
            &Type::Ident(ref id) => {
                let new_id = generator.gen();
                (quote_pat!(cx, $new_id),
                 if st.sorts.contains(id) {
                     let lower = ident_to_lower(id);
                     quote_expr!(cx, {
                         let mut abt = $new_id;
                         for var in vars.iter() {
                             let unbound = Abt::unbind(box super::$lower::oper_unbind, var.clone(), -1, abt);
                             abt = match Abt::out(box super::$lower::oper_unbind, unbound) {
                                 AbtView::Binding(_, abt) => abt,
                                 _ => unreachable!()
                             };
                         }
                         (abt, vec![])
                     })
                 } else {
                     quote_expr!(cx, ($new_id, vec![]))
                })
            },
            &Type::Prod(ref tys) => {
                let (pats, exprs): (Vec<P<Pat>>, Vec<P<Expr>>) =
                    tys.iter().map(|ty| ty.to_view_out_arm_helper(cx, generator, st)).unzip();
                let new_vars = generator.gen();
                let exprs: Vec<P<Expr>> = exprs.into_iter().map(|expr| {
                    let t_id = generator.gen();
                    let vars_id = generator.gen();
                    quote_expr!(cx, {
                        let ($t_id, mut $vars_id) = $expr;
                        $new_vars.append(&mut $vars_id);
                        $t_id
                    })
                }).collect();
                let pats = token_separate(cx, &pats, RsToken::Comma);
                let exprs = token_separate(cx, &exprs, RsToken::Comma);
                (quote_pat!(cx, ($pats)),
                 quote_expr!(cx, {
                     let mut $new_vars = vec![];
                     (($exprs), $new_vars)
                 }))
            },
            &Type::Vec(ref ty) => {
                let (pat, expr) = ty.to_view_out_arm_helper(cx, generator, st);
                let new_id = generator.gen();
                (quote_pat!(cx, $new_id),
                 quote_expr!(cx, {
                     let (ts, vars): (Vec<Abt<Op>>, Vec<Vec<Var>>) =
                         $new_id.into_iter().map(|$pat| {
                             $expr
                         }).unzip();
                     (ts, vars.into_iter().flat_map(|x| x).collect())
                 }))
            },
            &Type::Var(box ref l, box ref r) => {
                let (lpat, lexpr) = l.to_view_out_arm_helper(cx, generator, st);
                let (rpat, rexpr) = r.to_view_out_arm_helper(cx, generator, st);
                (quote_pat!(cx, ($lpat, $rpat)),
                 quote_expr!(cx, {
                     let (t0, vars) = $lexpr;
                     let (t1, vars0) = $rexpr;
                     ((t0, t1), vars0)
                 }))
            },
            &Type::Bind(ref id) => {
                let new_id = generator.gen();
                (quote_pat!(cx, $new_id),
                 quote_expr!(cx, {
                     let x = Var::from_string($new_id);
                     (x.clone(), vec![x])
                 }))
            }
        }
    }

    pub fn to_subst_arm(&self, cx: &mut ExtCtxt, st: &mut State, name: &Ident) -> Arm
    {
        let (pat, e) = self.to_subst_arm_helper(cx, &mut IdGenerator::new(), st);
        let e = quote_expr!(cx, View::$name($e));
        let node = st.build_node(cx, e);
        quote_arm!(cx, View::$name($pat) => { into($node) })
    }

    fn to_subst_arm_helper(&self, cx: &mut ExtCtxt, generator: &mut IdGenerator,
                           st: &mut State)
                           -> (P<Pat>, P<Expr>)
    {
        match self {
            &Type::Ident(ref id) => {
                let new_id = generator.gen();
                (quote_pat!(cx, $new_id),
                 if st.sorts.contains(id) {
                     quote_expr!(cx, subst(t.clone(), x.clone(), $new_id))
                 } else {
                     quote_expr!(cx, $new_id)
                 })
            },
            &Type::Prod(ref tys) => {
                let (pats, exprs): (Vec<P<Pat>>, Vec<P<Expr>>) =
                    tys.iter().map(|ty| ty.to_subst_arm_helper(cx, generator, st)).unzip();
                let pats = token_separate(cx, &pats, RsToken::Comma);
                let exprs = token_separate(cx, &exprs, RsToken::Comma);
                (quote_pat!(cx, ($pats)), quote_expr!(cx, ($exprs)))
            },
            &Type::Vec(ref ty) => {
                let (pat, expr) = ty.to_subst_arm_helper(cx, generator, st);
                let new_id = generator.gen();
                (quote_pat!(cx, $new_id),
                 quote_expr!(cx, {
                     $new_id.into_iter().map(|$pat| $expr).collect()
                 }))
            },
            &Type::Var(_, box ref r) => {
                let (rpat, rexpr) = r.to_subst_arm_helper(cx, generator, st);
                (quote_pat!(cx, (l, $rpat)),
                 quote_expr!(cx, (l, $rexpr)))
            },
            &Type::Bind(_) => {
                let new_id = generator.gen();
                (quote_pat!(cx, $new_id), quote_expr!(cx, $new_id))
            }
        }
    }

    pub fn to_free_vars_arm(&self, cx: &mut ExtCtxt, sorts: &HashSet<Ident>,
                            name: &Ident)
                            -> Arm
    {
        let (pat, e) = self.to_free_vars_arm_helper(cx, &mut IdGenerator::new(), sorts);
        quote_arm!(cx, View::$name($pat) => { $e })
    }

    fn to_free_vars_arm_helper(&self,
                               cx: &mut ExtCtxt,
                               generator: &mut IdGenerator,
                               sorts: &HashSet<Ident>)
                               -> (P<Pat>, P<Expr>)
    {
        match self {
            &Type::Ident(ref id) => {
                let new_id = generator.gen();
                (quote_pat!(cx, $new_id),
                 if sorts.contains(id) {
                     quote_expr!(cx, free_vars($new_id))
                 } else {
                     quote_expr!(cx, HashSet::new())
                 })
            },
            &Type::Prod(ref tys) => {
                let (pats, exprs): (Vec<P<Pat>>, Vec<P<Expr>>) =
                    tys.iter().map(|ty| ty.to_free_vars_arm_helper(cx, generator, sorts)).unzip();
                let new_vars = generator.gen();
                let exprs: Vec<P<Expr>> = exprs.into_iter().map(|expr| {
                    quote_expr!(cx, {
                        $new_vars.extend(($expr).into_iter());
                    })
                }).collect();
                let pats = token_separate(cx, &pats, RsToken::Comma);
                let exprs = token_separate(cx, &exprs, RsToken::Comma);
                (quote_pat!(cx, ($pats)),
                 quote_expr!(cx, {
                     let mut $new_vars = HashSet::new();
                     ($exprs);
                     $new_vars
                 }))
            },
            &Type::Vec(ref ty) => {
                let (pat, expr) = ty.to_free_vars_arm_helper(cx, generator,sorts);
                let new_id = generator.gen();
                (quote_pat!(cx, $new_id),
                 quote_expr!(cx, {
                     let mut hs = HashSet::new();
                     for $pat in $new_id.into_iter() {
                         hs.extend($expr);
                     }
                     hs
                 }))
            },
            &Type::Var(_, box ref r) => {
                let (rpat, rexpr) = r.to_free_vars_arm_helper(cx, generator, sorts);
                (quote_pat!(cx, (_, $rpat)),
                 quote_expr!(cx, $rexpr))
            },
            &Type::Bind(_) => {
                (quote_pat!(cx, _), quote_expr!(cx, HashSet::new()))
            }
        }
    }

    pub fn to_enum_string(&self, sort_id: &Ident) -> String {
        match self {
            &Type::Ident(ref id) => id.name.as_str().to_string(),
            &Type::Prod(ref tys) => {
                let strs: Vec<String> =
                    tys.iter().map(|ty| ty.to_enum_string(sort_id)).collect();
                format!("({})", strs.join(", "))
            },
            &Type::Vec(ref ty) => format!("Vec<{}>", ty.to_enum_string(sort_id)),
            &Type::Var(box ref l, box ref r) =>
                Type::Prod(vec![l.clone(), r.clone()]).to_enum_string(sort_id),
            &Type::Bind(_) => "Var".to_string()
        }
    }

    pub fn to_ops_enum_string(&self, sort_id: &Ident) -> String {
        match self {
            &Type::Ident(ref id) => id.name.as_str().to_string(),
            &Type::Prod(ref tys) => {
                let strs: Vec<String> =
                    tys.iter().map(|ty| ty.to_ops_enum_string(sort_id)).collect();
                format!("({})", strs.join(", "))
            },
            &Type::Vec(ref ty) => format!("Vec<{}>", ty.to_enum_string(sort_id)),
            &Type::Var(box ref l, box ref r) =>
                Type::Prod(vec![l.clone(), r.clone()]).to_ops_enum_string(sort_id),
            &Type::Bind(_) => "String".to_string()
        }
    }
}
