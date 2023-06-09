#![feature(box_syntax, box_patterns, if_let_guard, let_chains)]

pub mod parser;

use color_eyre::{
    eyre::{eyre, ContextCompat},
    Result,
};
use im_rc::HashMap;
use std::{
    fmt,
    ops::{Deref, DerefMut},
};

type Level = u8;

type Name = String;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expr {
    U(Option<Level>),
    Nat,
    Zero,
    Succ(Box<Expr>),
    NatLit(u64),
    Var(Name),
    Pi(String, Box<Expr>, Box<Expr>),
    The(Box<Expr>, Box<Expr>),
    Lam(String, Box<Expr>),
    App(Box<Expr>, Box<Expr>),
    Plus(Box<Expr>, Box<Expr>),
}

#[derive(Clone, Debug)]
pub enum Value {
    Neutral(Box<Ty>, Neutral),
    U(u8),
    Nat,
    NatLit(u64),
    Pi(Box<Ty>, Closure),
    Lam(Closure),
}

#[derive(Clone, Debug)]
pub struct Env(HashMap<Name, Value>);

impl Env {
    pub fn new() -> Self {
        Self(HashMap::new())
    }

    fn extend(&self, x: Name, v: Value) -> Result<Self> {
        if self.contains_key(&x) {
            return Err(eyre!("{x} already exists"));
        }

        Ok(Self(self.update(x, v)))
    }
}

impl From<Ctx> for Env {
    fn from(value: Ctx) -> Self {
        Self(
            value
                .0
                .into_iter()
                .map(|(x, (t, o))| {
                    let v = match o {
                        None => Value::Neutral(box t, Neutral::Var(x.clone())),
                        Some(v) => v,
                    };

                    (x, v)
                })
                .collect(),
        )
    }
}

impl Deref for Env {
    type Target = HashMap<Name, Value>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Clone)]
pub struct Ctx(HashMap<Name, (Ty, Option<Value>)>);

impl Deref for Ctx {
    type Target = HashMap<Name, (Ty, Option<Value>)>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Ctx {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl Ctx {
    pub fn new() -> Self {
        Self(HashMap::new())
    }

    fn lookup_type(&self, x: &Name) -> Result<&Ty> {
        let (t, _v) = self
            .get(x)
            .wrap_err_with(|| eyre!("Unbound variable: {x}"))?;
        Ok(t)
    }

    fn extend(&self, x: Name, t: Ty) -> Result<Self> {
        if self.contains_key(&x) {
            return Err(eyre!("Variable already exists: {x}"));
        }

        Ok(Self(self.update(x, (t, None))))
    }
}

impl Expr {
    pub fn check(&self, t: &Ty, ctx: Ctx) -> Result<()> {
        match self {
            Self::U(o) => {
                let i = o.unwrap_or_default();

                let Ty::U(j) = t else {
                    return Err(eyre!("error"));
                };

                if i >= *j {
                    return Err(eyre!("error"));
                }

                Ok(())
            }
            Self::Nat => {
                let Ty::U(_i) = t else {
                    return Err(eyre!("error"));
                };

                Ok(())
            }
            Self::Succ(n) => {
                t.is_nat(ctx.clone())?;
                n.check(t, ctx)
            }
            Self::Zero | Self::NatLit(_) => t.is_nat(ctx),
            Self::Pi(x, e_1, e_2) => {
                let Ty::U(_i) = t else {
                    return Err(eyre!("error"));
                };

                e_1.check(t, ctx.clone())?;

                if ctx.contains_key(x) {
                    return Err(eyre!("{x} already exists"));
                }

                let env: Env = ctx.clone().into();

                let t_1: Ty = e_1.eval(env)?;

                let ctx_1 = ctx.extend(x.to_owned(), t_1)?;
                e_2.check(t, ctx_1)
            }
            Self::Lam(x, body) => {
                let (t_1, closure) = t.is_pi(ctx.clone())?;
                let t_2 =
                    closure.eval(Value::Neutral(box t_1.clone(), Neutral::Var(x.to_owned())))?;
                let ctx_1 = ctx.extend(x.to_owned(), t_1)?;
                body.check(&t_2, ctx_1)
            }
            _ => {
                let t_1 = self.synth(ctx.clone())?;
                t_1.convert(t, Value::U(u8::MAX), ctx)
            }
        }
    }

    pub fn eval(&self, env: Env) -> Result<Value> {
        match self {
            Self::U(o) => Ok(Value::U(o.unwrap_or(0))),
            Self::Nat => Ok(Value::Nat),
            Self::Zero => Ok(Value::NatLit(0)),
            Self::Succ(e) => {
                let value = e.eval(env)?;

                match value {
                    Value::Neutral(ty, n) => Ok(Value::Neutral(ty, Neutral::Succ(box n))),
                    Value::NatLit(l) => Ok(Value::NatLit(l + 1)),
                    _ => unreachable!(),
                }
            }
            Self::NatLit(n) => Ok(Value::NatLit(*n)),
            Self::Var(x) => {
                let v = env
                    .get(x)
                    .cloned()
                    .wrap_err_with(|| eyre!("Missing value for {x}"))?;

                Ok(v)
            }
            Self::Pi(x, dom, box ran) => {
                let t = dom.eval(env.clone())?;

                Ok(Value::Pi(
                    box t,
                    Closure {
                        env,
                        name: x.to_owned(),
                        body: ran.to_owned(),
                    },
                ))
            }
            Self::The(_t, e) => e.eval(env),
            Self::Lam(x, box body) => Ok(Value::Lam(Closure {
                env,
                name: x.to_owned(),
                body: body.to_owned(),
            })),
            Self::App(rator, rand) => rator.eval(env.clone())?.do_apply(rand.eval(env)?),
            Self::Plus(e_1, e_2) => {
                let v_1 = e_1.eval(env.clone())?;
                let v_2 = e_2.eval(env)?;

                match (v_1, v_2) {
                    (Value::NatLit(l_1), Value::NatLit(l_2)) => Ok(Value::NatLit(l_1 + l_2)),
                    (Value::Neutral(box t, n_1), v_2) => Ok(Value::Neutral(
                        box t.clone(),
                        Neutral::Plus(box n_1, box Normal { ty: t, value: v_2 }),
                    )),
                    (v_1, Value::Neutral(box t, n_2)) => Ok(Value::Neutral(
                        box t.clone(),
                        Neutral::Plus(box n_2, box Normal { ty: t, value: v_1 }),
                    )),
                    _ => unreachable!(),
                }
            }
        }
    }

    pub fn normalize(self, ctx: Ctx) -> Result<Normal> {
        let ty = self.synth(ctx.clone())?;
        let value = self.eval(ctx.into())?;

        Ok(Normal { ty, value })
    }

    pub fn synth(&self, ctx: Ctx) -> Result<Ty> {
        match self {
            Self::Zero => Ok(Ty::Nat),
            Self::Succ(e) => {
                let ty = Ty::Nat;
                e.check(&ty, ctx)?;
                Ok(ty)
            }
            Self::NatLit(_n) => Ok(Ty::Nat),
            Self::Var(x) => ctx.lookup_type(x).cloned(),
            Self::The(ty, expr) => {
                ty.check(&Ty::U(u8::MAX), ctx.clone())?;
                let env: Env = ctx.clone().into();
                let ty_1 = ty.eval(env)?;
                expr.check(&ty_1, ctx)?;
                Ok(ty_1)
            }
            Self::App(rator, rand) => {
                let ty = rator.synth(ctx.clone())?;
                let (ty_1, closure) = ty.is_pi(ctx.clone())?;
                rand.check(&ty_1, ctx.clone())?;
                let env: Env = ctx.into();
                closure.eval(rand.eval(env)?)
            }
            Self::Plus(e_1, e_2) => {
                let ty = Ty::Nat;
                e_1.check(&ty, ctx.clone())?;
                e_2.check(&ty, ctx)?;
                Ok(ty)
            }
            _ => Err(eyre!("failed to synthesize type")),
        }
    }

    // fn substitute(self, e: &Self, x: &String) -> Expr {
    //     match self {
    //         Self::U(_) | Self::Nat | Self::Zero | Self::NatLit(_) => self,
    //         Self::Succ(e_1) => Self::Succ(box e_1.substitute(e, x)),
    //         Self::Var(ref y) => {
    //             if y == x {
    //                 e.to_owned()
    //             } else {
    //                 self
    //             }
    //         }
    //         Self::Pi(y, e_1, e_2) => {
    //             Self::Pi(y, box e_1.substitute(e, x), box e_2.substitute(e, x))
    //         }
    //         Self::The(e_1, e_2) => Self::The(box e_1.substitute(e, x), box e_2.substitute(e, x)),
    //         Self::Lam(y, e_1) => Self::Lam(y, box e_1.substitute(e, x)),
    //         Self::App(e_1, e_2) => Self::App(box e_1.substitute(e, x), box e_2.substitute(e, x)),
    //         Self::Plus(e_1, e_2) => Self::Plus(box e_1.substitute(e, x), box e_2.substitute(e, x)),
    //     }
    // }

    fn alpha_equiv(&self, other: &Self) -> bool {
        self.alpha_equiv_helper(other, 0, HashMap::new(), HashMap::new())
    }

    fn alpha_equiv_helper(
        &self,
        other: &Self,
        i: usize,
        ns_1: HashMap<Name, usize>,
        ns_2: HashMap<Name, usize>,
    ) -> bool {
        match (self, other) {
            (Self::U(i), Self::U(j)) => i == j,
            (Self::Nat, Self::Nat) => true,
            (Self::Zero, Self::Zero) => true,
            (Self::Succ(e_1), Self::Succ(e_2)) => e_1.alpha_equiv_helper(e_2, i, ns_1, ns_2),
            (Self::NatLit(n_1), Self::NatLit(n_2)) => n_1 == n_2,
            (Self::Var(x), Self::Var(y)) => match (ns_1.get(x), ns_2.get(y)) {
                (None, None) => x == y,
                (Some(i), Some(j)) => i == j,
                _ => false,
            },
            (Self::Pi(x, a_1, r_1), Self::Pi(y, a_2, r_2)) => {
                a_1.alpha_equiv_helper(a_2, i, ns_1.clone(), ns_2.clone())
                    && r_1.alpha_equiv_helper(
                        r_2,
                        i + 1,
                        ns_1.update(x.to_owned(), i),
                        ns_2.update(y.to_owned(), i),
                    )
            }
            (Self::The(t_1, e_1), Self::The(t_2, e_2)) => {
                t_1.alpha_equiv_helper(t_2, i, ns_1.clone(), ns_2.clone())
                    && e_1.alpha_equiv_helper(e_2, i, ns_1, ns_2)
            }
            (Self::Lam(x, body_1), Self::Lam(y, body_2)) => body_1.alpha_equiv_helper(
                body_2,
                i + 1,
                ns_1.update(x.to_owned(), i),
                ns_2.update(y.to_owned(), i),
            ),
            (Self::App(rator_1, rand_1), Self::App(rator_2, rand_2)) => {
                rator_1.alpha_equiv_helper(rator_2, i, ns_1.clone(), ns_2.clone())
                    && rand_1.alpha_equiv_helper(rand_2, i, ns_1.clone(), ns_2.clone())
            }
            (Self::Plus(a_1, b_1), Self::Plus(a_2, b_2)) => {
                a_1.alpha_equiv_helper(a_2, i, ns_1.clone(), ns_2.clone())
                    && b_1.alpha_equiv_helper(b_2, i, ns_1, ns_2)
            }
            _ => false,
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::U(None) => 'U'.fmt(f),
            Self::U(Some(i)) => write!(f, "U({i})"),
            Self::Nat => "Nat".fmt(f),
            Self::Zero => "zero".fmt(f),
            Self::Succ(e) => write!(f, "succ({e})"),
            Self::NatLit(n) => n.fmt(f),
            Self::Var(x) => x.fmt(f),
            Self::Pi(x, e_1, e_2) => write!(f, "Pi({x} : {e_1}. {e_2})"),
            Self::The(e_1, e_2) => write!(f, "the({e_1}, {e_2})"),
            Self::Lam(x, e) => write!(f, "lam({x}. {e})"),
            Self::App(e_1, e_2) => write!(f, "{e_1}({e_2})"),
            Self::Plus(e_1, e_2) => write!(f, "{e_1} + {e_2}"),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Neutral {
    Var(Name),
    App(Box<Neutral>, Box<Normal>),
    Plus(Box<Neutral>, Box<Normal>),
    Succ(Box<Neutral>),
}

impl Neutral {
    fn read_back(&self, ctx: Ctx) -> Expr {
        match self {
            Self::Var(x) => Expr::Var(x.to_owned()),
            Self::App(neu, arg) => {
                Expr::App(box neu.read_back(ctx.clone()), box arg.read_back(ctx))
            }
            Self::Plus(neu, norm) => {
                Expr::Plus(box neu.read_back(ctx.clone()), box norm.read_back(ctx))
            }
            Self::Succ(neu) => Expr::Succ(box neu.read_back(ctx)),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Closure {
    env: Env,
    name: Name,
    body: Expr,
}

impl Closure {
    fn eval(&self, v: Value) -> Result<Value> {
        let env = self.env.extend(self.name.clone(), v)?;
        self.body.eval(env)
    }
}

impl Value {
    fn unexpected<T>(&self, msg: &str, ctx: Ctx) -> Result<T> {
        let e = self.read_back_typed(&Value::U(u8::MAX), ctx);
        Err(eyre!("{msg}: {e}"))
    }

    fn is_pi(&self, ctx: Ctx) -> Result<(Ty, Closure)> {
        let Self::Pi(box ty, closure) = self else {
            return self.unexpected("Not a Pi type", ctx);
        };

        Ok((ty.to_owned(), closure.to_owned()))
    }

    fn is_nat(&self, ctx: Ctx) -> Result<()> {
        let Self::Nat = self else {
            return self.unexpected("Not Nat", ctx);
        };

        Ok(())
    }

    fn do_apply(&self, arg: Self) -> Result<Value> {
        match self {
            Self::Lam(closure) => closure.eval(arg),
            Self::Neutral(box Ty::Pi(box dom, ran), neu) => {
                let t = ran.eval(arg.to_owned())?;

                Ok(Self::Neutral(
                    box t,
                    Neutral::App(
                        box neu.to_owned(),
                        box Normal {
                            ty: dom.to_owned(),
                            value: arg,
                        },
                    ),
                ))
            }
            _ => Err(eyre!("error")),
        }
    }
}

pub type Ty = Value;

#[derive(Clone, Debug)]
pub struct Normal {
    pub ty: Ty,
    pub value: Value,
}

impl Normal {
    pub fn read_back(&self, ctx: Ctx) -> Expr {
        self.value.read_back_typed(&self.ty, ctx)
    }
}

impl Value {
    pub fn read_back_typed(&self, ty: &Ty, ctx: Ctx) -> Expr {
        match (self, ty) {
            (Self::Neutral(_t, neutral), _) => neutral.read_back(ctx),
            (Self::U(i), Ty::U(j)) if i < j => Expr::U(if *i == 0 { None } else { Some(*i) }),
            (Self::Nat, Ty::U(_i)) => Expr::Nat,
            (Self::NatLit(n), Ty::Nat) => Expr::NatLit(*n),
            (Self::Pi(t, closure), Ty::U(_i)) => Expr::Pi(
                closure.name.clone(),
                box t.read_back_typed(ty, ctx),
                box closure.body.clone(),
            ),
            (Self::Lam(closure), Ty::Pi(_ty, _closure)) => {
                Expr::Lam(closure.name.clone(), box closure.body.clone())
            }
            _ => unreachable!(),
        }
    }

    fn convert(&self, other: &Self, t: Ty, ctx: Ctx) -> Result<()> {
        let e_1 = self.read_back_typed(&t, ctx.clone());
        let e_2 = other.read_back_typed(&t, ctx);

        if !e_1.alpha_equiv(&e_2) {
            return Err(eyre!("{e_1} is not the same type as {e_2}"));
        }

        Ok(())
    }
}
