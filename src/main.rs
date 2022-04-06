use sym_kalc::ast::Expr;

fn main() {
    let expr = "2 ** 2 + 2^2".parse::<Expr>().unwrap();
    dbg!(&expr);
    println!("{} = {}", expr, expr.eval());
}