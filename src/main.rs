use core::fmt;
use std::{ops, str::FromStr};

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, space0},
    combinator::map,
    multi::fold_many0,
    number::complete::double,
    sequence::{delimited, pair, separated_pair},
    Finish, IResult,
};

fn main() {
    let expr = "2 ** 2 - 2^2".parse::<Expr>().unwrap();
    dbg!(&expr);
    println!("{} = {}", expr, expr.eval());
}

#[derive(Debug, Clone)]
enum Expr {
    Val(f64),
    BinOp {
        kind: BinOpKind,
        l: Box<Expr>,
        r: Box<Expr>,
    },
    Parens(Box<Expr>),
}
#[derive(Debug, Clone, Copy)]
enum BinOpKind {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
}

impl Expr {
    fn eval<'a>(&'a self) -> f64 {
        use Expr::*;
        match self {
            Val(v) => *v,
            BinOp { kind, l, r } => Expr::eval_bin(kind, l, r),
            Parens(expr) => expr.eval(),
        }
    }

    fn eval_bin<'a>(kind: &'a BinOpKind, l: &'a Box<Expr>, r: &'a Box<Expr>) -> f64 {
        use BinOpKind::*;
        match kind {
            Add => l.eval() + r.eval(),
            Sub => l.eval() - r.eval(),
            Mul => l.eval() * r.eval(),
            Div => l.eval() / r.eval(),
            Pow => l.eval().powf(r.eval()),
        }
    }
}

impl FromStr for Expr {
    //TODO figure out how to convert nom::Err<&str> to nom::Err<String>
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        expr(s).finish().map(|res| res.1).map_err(|_| ())
    }
}

impl fmt::Display for BinOpKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use BinOpKind::*;
        match self {
            Add => write!(f, "+"),
            Sub => write!(f, "-"),
            Mul => write!(f, "*"),
            Div => write!(f, "/"),
            Pow => write!(f, "^"),
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Val(v) => write!(f, "{}", v),
            Expr::BinOp { kind, l, r } => write!(f, "{} {} {}", l, kind, r),
            Expr::Parens(expr) => write!(f, "({})", expr),
        }
    }
}

impl ops::Mul for Expr {
    type Output = Expr;

    fn mul(self, rhs: Expr) -> Expr {
        Expr::BinOp {
            kind: BinOpKind::Mul,
            l: Box::new(self),
            r: Box::new(rhs),
        }
    }
}

impl ops::Div for Expr {
    type Output = Expr;

    fn div(self, rhs: Expr) -> Expr {
        Expr::BinOp {
            kind: BinOpKind::Div,
            l: Box::new(self),
            r: Box::new(rhs),
        }
    }
}

impl ops::Add for Expr {
    type Output = Expr;

    fn add(self, rhs: Expr) -> Expr {
        Expr::BinOp {
            kind: BinOpKind::Add,
            l: Box::new(self),
            r: Box::new(rhs),
        }
    }
}

impl ops::Sub for Expr {
    type Output = Expr;

    fn sub(self, rhs: Expr) -> Expr {
        Expr::BinOp {
            kind: BinOpKind::Sub,
            l: Box::new(self),
            r: Box::new(rhs),
        }
    }
}

fn parens(i: &str) -> IResult<&str, Expr> {
    let p = delimited(space0, delimited(char('('), expr, char(')')), space0)(i)?;

    Ok((p.0, Expr::Parens(Box::new(p.1))))
}

fn val(i: &str) -> IResult<&str, Expr> {
    let p = alt((
        map(delimited(space0, double, space0), |v| Expr::Val(v)),
        parens,
    ))(i)?;

    Ok((p.0, p.1))
}

fn exponent(i: &str) -> IResult<&str, Expr> {
    let p = alt((
        map(
            separated_pair(val, alt((tag("**"), tag("^"))), val),
            |(l, r)| Expr::BinOp {
                kind: BinOpKind::Pow,
                l: Box::new(l),
                r: Box::new(r),
            },
        ),
        val,
    ))(i)?;

    Ok((p.0, p.1))
}

fn term(i: &str) -> IResult<&str, Expr> {
    let (i, init) = exponent(i)?;

    fold_many0(
        pair(alt((char('*'), char('/'))), exponent),
        move || init.clone(),
        |acc, (op, val): (char, Expr)| {
            if op == '*' {
                acc * val
            } else {
                acc / val
            }
        },
    )(i)
}

fn expr(i: &str) -> IResult<&str, Expr> {
    let (i, init) = term(i)?;

    fold_many0(
        pair(alt((char('+'), char('-'))), term),
        move || init.clone(),
        |acc, (op, val): (char, Expr)| {
            if op == '+' {
                acc + val
            } else {
                acc - val
            }
        },
    )(i)
}
