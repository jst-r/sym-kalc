#[derive(Debug, Clone)]
pub enum Expr {
    Val(f64),
    BinOp {
        kind: BinOpKind,
        l: Box<Expr>,
        r: Box<Expr>,
    },
    Parens(Box<Expr>),
}
#[derive(Debug, Clone, Copy)]
pub enum BinOpKind {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
}

impl Expr {
    pub fn eval<'a>(&'a self) -> f64 {
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