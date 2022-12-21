#![feature(box_syntax, box_patterns, if_let_guard, let_chains)]

pub mod parser;

use color_eyre::{
    eyre::{eyre, ContextCompat, Error},
    Result,
};
use im_rc::HashMap;
use std::fmt;

#[derive(Clone, Debug)]
pub enum Expr {
    U(Option<u8>),
    Nat,
    Zero,
    Succ(Box<Expr>),
    NatLit(u64),
    Var(String),
    Pi(String, Box<Expr>, Box<Expr>),
    The(Box<Expr>, Box<Expr>),
    Lam(String, Box<Expr>),
    App(Box<Expr>, Box<Expr>),
    Plus(Box<Expr>, Box<Expr>),
}

impl Expr {
    fn check(&self, ty: &Ty, ctx: HashMap<String, Ty>) -> Result<()> {
        match self {
            Self::U(o) => match ty {
                Ty::U(i) if o.map(|j| j < *i).unwrap_or_else(|| *i != 0) => Ok(()),
                _ => Err(eyre!("expected a {ty}, found {self}")),
            },
            Self::Nat => match ty {
                Ty::U(_i) => Ok(()),
                _ => Err(eyre!("expected a {ty}, found {self}")),
            },
            Self::Succ(e) => match ty {
                Ty::Nat => e.check(ty, ctx),
                _ => Err(eyre!("expected a {ty}, found {self}")),
            },
            Self::Zero | Self::NatLit(_) => match ty {
                Ty::Nat => Ok(()),
                _ => Err(eyre!("expected a {ty}, found {self}")),
            },
            // Self::Var(x) => {
            //     let ty_1 = ctx.get(x).wrap_err_with(|| eyre!("{x} not found"))?;
            //
            //     if ty_1 != ty {
            //         return Err(eyre!("type mismatch: {ty_1} != {ty}"));
            //     }
            //
            //     Ok(())
            // }
            Self::Pi(x, e_1, e_2) => match ty {
                Ty::U(i) => {
                    let ty_1 = e_1.try_into_type(*i)?;

                    if ctx.contains_key(x) {
                        return Err(eyre!("{x} already exists"));
                    }

                    let ctx_1 = ctx.update(x.to_owned(), ty_1);
                    e_2.check(&Ty::U(*i), ctx_1)
                }
                _ => Err(eyre!("expected a {ty}, found {self}")),
            },
            // Self::The(box e_1, e_2) => {
            //     let ty_1: Ty = e_1.to_owned().try_into()?;
            //
            //     if ty_1 != *ty {
            //         return Err(eyre!("type mismatch: {ty_1} != {ty}"));
            //     }
            //
            //     e_2.check(ty, ctx)
            // }
            Self::Lam(x, e) => match ty {
                Ty::Pi(y, box ty_1, ty_2) if y == x => {
                    if ctx.contains_key(x) {
                        return Err(eyre!("{x} already exists"));
                    }

                    let ctx_1 = ctx.update(x.to_owned(), ty_1.to_owned());
                    e.check(ty_2, ctx_1)
                }
                _ => Err(eyre!("expected a {ty}, found {self}")),
            },
            // Self::App(e_1, e_2) => {
            //     let ty_1 = e_1.synth(ctx.clone())?;
            //
            //     match ty_1 {
            //         Ty::Pi(_x, ty_2, ty_3) => {
            //             e_2.check(&ty_2, ctx)?;
            //
            //             if *ty_3 != *ty {
            //                 return Err(eyre!("type mismatch: {ty_3} != {ty}"));
            //             }
            //
            //             Ok(())
            //         }
            //         _ => Err(eyre!("expected a function, found {e_1}")),
            //     }
            // }
            // Self::Plus(e_1, e_2) => match ty {
            //     Ty::Nat => {
            //         e_1.check(ty, ctx.clone())?;
            //         e_2.check(ty, ctx)?;
            //         Ok(())
            //     }
            //     _ => Err(eyre!("expected a {ty}, found {self}")),
            // },
            _ => {
                let ty_1 = self.synth(ctx)?;

                if ty_1 != *ty {
                    return Err(eyre!("type mismatch: {ty_1} != {ty}"));
                }

                Ok(())
            }
        }
    }

    fn try_into_type(&self, i: u8) -> Result<Ty> {
        match self {
            Self::U(o) if let j = o.unwrap_or(0) && j < i => Ok(Ty::U(j)),
            Self::Nat => Ok(Ty::Nat),
            Self::Pi(x, e_1, e_2) => {
                let ty_1 = e_1.try_into_type(i)?;
                let ty_2 = e_2.try_into_type(i)?;
                Ok(Ty::Pi(x.to_owned(), box ty_1, box ty_2))
            }
            _ => Err(eyre!("not a term of U({i})")),
        }
    }

    fn eval(self, env: HashMap<String, Value>) -> Result<Value> {
        match self {
            Self::U(o) => Ok(Value::U(o.unwrap_or(0))),
            Self::Nat => Ok(Value::Nat),
            Self::Zero => Ok(Value::NatLit(0)),
            Self::Succ(e) => {
                let value = e.eval(env)?;

                match value {
                    Value::Neutral(n) => Ok(Value::Neutral(Neutral::Succ(box n))),
                    Value::NatLit(l) => Ok(Value::NatLit(l + 1)),
                    _ => unreachable!(),
                }
            }
            Self::NatLit(n) => Ok(Value::NatLit(n)),
            Self::Var(x) => Ok(env
                .get(&x)
                .cloned()
                .unwrap_or_else(|| Value::Neutral(Neutral::Var(x)))),
            Self::Pi(x, e_1, e_2) => {
                let mut env_1 = env.clone();
                let v_1 = e_1.eval(env)?;
                env_1.insert(x.clone(), v_1.clone());
                let v_2 = e_2.eval(env_1)?;
                Ok(Value::Pi(x, box v_1, box v_2))
            }
            Self::The(_e_1, e_2) => e_2.eval(env),
            Self::Lam(x, e) => {
                let v = e.eval(env)?;
                Ok(Value::Lam(x, box v))
            }
            Self::App(e_1, e_2) => {
                let value = e_1.eval(env.clone())?;

                match value {
                    Value::Neutral(n) => {
                        let value_1 = e_2.eval(env)?;
                        Ok(Value::Neutral(Neutral::App(box n, box value_1)))
                    }
                    Value::Lam(x, v) => v.read_back().substitute(&e_2, &x).eval(env),
                    _ => unreachable!(),
                }
            }
            Self::Plus(e_1, e_2) => {
                let v_1 = e_1.eval(env.clone())?;
                let v_2 = e_2.eval(env)?;

                match (v_1, v_2) {
                    (Value::NatLit(l_1), Value::NatLit(l_2)) => Ok(Value::NatLit(l_1 + l_2)),
                    (Value::Neutral(n_1), v_2) => {
                        Ok(Value::Neutral(Neutral::Plus(box n_1, box v_2)))
                    }
                    (v_1, Value::Neutral(n_2)) => {
                        Ok(Value::Neutral(Neutral::Plus(box n_2, box v_1)))
                    }
                    _ => unreachable!(),
                }
            }
        }
    }

    pub fn normalize(self) -> Result<Self> {
        let value = self.eval(HashMap::new())?;
        Ok(value.read_back())
    }

    pub fn synth(&self, ctx: HashMap<String, Ty>) -> Result<Ty> {
        match self {
            Self::Zero => Ok(Ty::Nat),
            Self::Succ(e) => {
                let ty = Ty::Nat;
                e.check(&ty, ctx)?;
                Ok(ty)
            }
            Self::NatLit(_n) => Ok(Ty::Nat),
            Self::Var(x) => ctx.get(x).cloned().wrap_err_with(|| eyre!("{x} not found")),
            Self::The(box e_1, e_2) => {
                let ty: Ty = e_1.to_owned().try_into()?;
                e_2.check(&ty, ctx)?;
                Ok(ty)
            }
            Self::App(e_1, e_2) => {
                let ty_1 = e_1.synth(ctx.clone())?;

                match ty_1 {
                    Ty::Pi(_x, ty_2, ty_3) => {
                        e_2.check(&ty_2, ctx)?;
                        Ok(*ty_3)
                    }
                    _ => Err(eyre!("expected a function, found {e_1}")),
                }
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

    fn substitute(self, e: &Self, x: &String) -> Expr {
        match self {
            Self::U(_) | Self::Nat | Self::Zero | Self::NatLit(_) => self,
            Self::Succ(e_1) => Self::Succ(box e_1.substitute(e, x)),
            Self::Var(ref y) => {
                if y == x {
                    e.to_owned()
                } else {
                    self
                }
            }
            Self::Pi(y, e_1, e_2) => {
                Self::Pi(y, box e_1.substitute(e, x), box e_2.substitute(e, x))
            }
            Self::The(e_1, e_2) => Self::The(box e_1.substitute(e, x), box e_2.substitute(e, x)),
            Self::Lam(y, e_1) => Self::Lam(y, box e_1.substitute(e, x)),
            Self::App(e_1, e_2) => Self::App(box e_1.substitute(e, x), box e_2.substitute(e, x)),
            Self::Plus(e_1, e_2) => Self::Plus(box e_1.substitute(e, x), box e_2.substitute(e, x)),
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::U(None) => write!(f, "u"),
            Self::U(Some(i)) => write!(f, "u({i})"),
            Self::Nat => write!(f, "nat"),
            Self::Zero => write!(f, "zero"),
            Self::Succ(e) => write!(f, "succ({e})"),
            Self::NatLit(n) => write!(f, "{n}"),
            Self::Var(x) => write!(f, "{x}"),
            Self::Pi(x, e_1, e_2) => write!(f, "pi({x}, {e_1}, {e_2})"),
            Self::The(e_1, e_2) => write!(f, "the({e_1}, {e_2})"),
            Self::Lam(x, e) => write!(f, "lam({x}, {e})"),
            Self::App(e_1, e_2) => write!(f, "{e_1} {e_2}"),
            Self::Plus(e_1, e_2) => write!(f, "{e_1} + {e_2}"),
        }
    }
}

#[derive(Clone)]
enum Neutral {
    Succ(Box<Neutral>),
    Var(String),
    App(Box<Neutral>, Box<Value>),
    Plus(Box<Neutral>, Box<Value>),
}

impl Neutral {
    fn read_back(self) -> Expr {
        match self {
            Self::Succ(n) => Expr::Succ(box n.read_back()),
            Self::Var(x) => Expr::Var(x),
            Self::App(n, v) => Expr::App(box n.read_back(), box v.read_back()),
            Self::Plus(n, v) => Expr::Plus(box n.read_back(), box v.read_back()),
        }
    }
}

#[derive(Clone)]
enum Value {
    Neutral(Neutral),
    U(u8),
    Nat,
    NatLit(u64),
    Pi(String, Box<Value>, Box<Value>),
    Lam(String, Box<Value>),
}

impl Value {
    fn read_back(self) -> Expr {
        match self {
            Self::Neutral(neutral) => neutral.read_back(),
            Self::U(i) => Expr::U(Some(i)),
            Self::Nat => Expr::Nat,
            Self::NatLit(n) => Expr::NatLit(n),
            Self::Pi(x, v_1, v_2) => Expr::Pi(x, box v_1.read_back(), box v_2.read_back()),
            Self::Lam(x, v) => Expr::Lam(x, box v.read_back()),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Ty {
    U(u8),
    Nat,
    Pi(String, Box<Ty>, Box<Ty>),
}

impl TryFrom<Expr> for Ty {
    type Error = Error;

    fn try_from(value: Expr) -> Result<Self, Self::Error> {
        match value {
            Expr::U(o) => Ok(Ty::U(o.unwrap_or(0))),
            Expr::Nat => Ok(Ty::Nat),
            Expr::Pi(x, box e_1, box e_2) => {
                let ty_1: Ty = e_1.try_into()?;
                let ty_2: Ty = e_2.try_into()?;
                Ok(Ty::Pi(x, box ty_1, box ty_2))
            }
            _ => Err(eyre!("not a type")),
        }
    }
}

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::U(i) => write!(f, "U({i})"),
            Self::Nat => write!(f, "Nat"),
            Self::Pi(x, ty_1, ty_2) => write!(f, "Pi({x}, {ty_1}, {ty_2})"),
        }
    }
}
