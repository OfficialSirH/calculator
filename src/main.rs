pub mod calculator;

fn main() {
    let input: String = std::env::args().nth(1).expect("No input string provided");
    let expression = calculator::expr(&input);
    println!("Expression: {:?}", expression.to_string());
    println!("Evaluation: {}", expression.eval());
}
