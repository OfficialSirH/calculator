use std::ops::Range;
use std::{fmt, vec};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Token {
    Op(Operator),
    Literal(f64),
    Invalid(char),
    Eof,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Operator {
    Plus,
    Minus,
    Mul,
    Div,
    Comp,
    Fact,
    ParensOpen,
    ParensClose,
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Operator::Plus => write!(f, "+"),
            Operator::Minus => write!(f, "-"),
            Operator::Mul => write!(f, "*"),
            Operator::Div => write!(f, "/"),
            Operator::Comp => write!(f, "."),
            Operator::Fact => write!(f, "!"),
            Operator::ParensOpen => write!(f, "("),
            Operator::ParensClose => write!(f, ")"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TokenResult {
    token: Token,
    range: Range<i32>,
}

pub struct Lexer {
    pub tokens: Vec<TokenResult>,
}

impl Lexer {
    pub fn new(input: &str) -> Lexer {
        let mut tokens = Vec::new();
        let mut char_indices = input.char_indices().peekable();

        while let Some((pos, ch)) = char_indices.next() {
            let mut range = Range {
                start: pos as i32,
                end: pos as i32,
            };

            let token_result = TokenResult {
                token: match ch {
                    '+' => Token::Op(Operator::Plus),
                    '-' => Token::Op(Operator::Minus),
                    '*' => Token::Op(Operator::Mul),
                    '/' => Token::Op(Operator::Div),
                    '.' => Token::Op(Operator::Comp),
                    '!' => Token::Op(Operator::Fact),
                    ')' => Token::Op(Operator::ParensClose),
                    '(' => Token::Op(Operator::ParensOpen),
                    '0'..='9' => {
                        let mut reached_decimal_point = false;

                        // let s: String = char_indices
                        //     .by_ref()
                        //     .take_while(|(_pos, c)| {
                        //         if *c == '.' && !reached_decimal_point {
                        //             reached_decimal_point = true;
                        //             return true;
                        //         }
                        //         c.is_digit(10)
                        //     })
                        //     .map(|(_pos, c)| {
                        //         range.end = _pos as i32;
                        //         c
                        //     })
                        //     .collect();
                        // let s = format!("{}{}", ch, s);
                        // NOTE: due to take_while consuming the false value, I have to make this work around for take_while
                        let mut value = vec![ch];
                        'consume_value: loop {
                            match char_indices.peek() {
                                Some((pos, char)) => {
                                    if *char == '.' && !reached_decimal_point {
                                        reached_decimal_point = true;
                                        range.end = *pos as i32;
                                        value.push(
                                            char_indices
                                                .by_ref()
                                                .take(1)
                                                .last()
                                                .expect("How the fuck")
                                                .1,
                                        );
                                        continue 'consume_value;
                                    }
                                    if char.is_ascii_digit() {
                                        range.end = *pos as i32;
                                        value.push(
                                            char_indices
                                                .by_ref()
                                                .take(1)
                                                .last()
                                                .expect("How the fuck")
                                                .1,
                                        );
                                        continue 'consume_value;
                                    }
                                    break 'consume_value;
                                }
                                None => break 'consume_value,
                            }
                        }

                        Token::Literal(
                            value
                                .iter()
                                .map(|c| c.to_string())
                                .collect::<Vec<String>>()
                                .join("")
                                .parse::<f64>()
                                .unwrap(),
                        )
                    }
                    ' ' | '\n' => continue,
                    _ => Token::Invalid(ch),
                },
                range,
            };
            tokens.push(token_result);
        }

        tokens.reverse();
        Lexer { tokens }
    }

    pub fn next(&mut self) -> TokenResult {
        self.tokens.pop().unwrap_or(TokenResult {
            range: 0..0,
            token: Token::Eof,
        })
    }

    pub fn peek(&mut self) -> TokenResult {
        self.tokens
            .last()
            .unwrap_or(
                &TokenResult {
                    range: 0..0,
                    token: Token::Eof,
                }
                .clone(),
            )
            .clone()
    }
}

#[derive(Debug)]
pub enum S {
    Literal(f64),
    Cons(Operator, Vec<S>),
}

impl fmt::Display for S {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            S::Literal(i) => write!(f, "{}", i),
            S::Cons(head, rest) => {
                write!(f, "({}", head)?;
                for s in rest {
                    write!(f, " {}", s)?;
                }
                write!(f, ")")
            }
        }
    }
}

pub fn expr(input: &str) -> S {
    let mut lexer = Lexer::new(input);
    expr_bp(&mut lexer, 0)
}

fn expr_bp(lexer: &mut Lexer, min_bp: u8) -> S {
    let mut lhs = match lexer.next().token {
        Token::Literal(it) => S::Literal(it),
        Token::Op(Operator::ParensOpen) => {
            let lhs = expr_bp(lexer, 0);
            assert_eq!(lexer.next().token, Token::Op(Operator::ParensClose));
            lhs
        }
        Token::Op(op) => {
            let ((), r_bp) = prefix_binding_power(op);
            let rhs = expr_bp(lexer, r_bp);
            S::Cons(op, vec![rhs])
        }
        t => panic!("Bad token: {:?}", t),
    };

    loop {
        let op = match lexer.peek().token {
            Token::Eof => break,
            Token::Op(op) => op,
            t => panic!("Bad token: {:?}", t),
        };

        if let Some((l_bp, ())) = postfix_binding_power(op) {
            if l_bp < min_bp {
                break;
            }
            lexer.next();

            lhs = S::Cons(op, vec![lhs]);
            continue;
        }

        if let Some((l_bp, r_bp)) = infix_binding_power(op) {
            if l_bp < min_bp {
                break;
            }

            lexer.next();
            let rhs = expr_bp(lexer, r_bp);

            lhs = S::Cons(op, vec![lhs, rhs]);
            continue;
        }

        break;
    }

    lhs
}

fn prefix_binding_power(op: Operator) -> ((), u8) {
    match op {
        Operator::Plus | Operator::Minus => ((), 5),
        _ => panic!("Bad op: {:?}", op),
    }
}

fn postfix_binding_power(op: Operator) -> Option<(u8, ())> {
    let res = match op {
        Operator::Fact => (7, ()),
        _ => return None,
    };
    Some(res)
}

fn infix_binding_power(op: Operator) -> Option<(u8, u8)> {
    let res = match op {
        Operator::Plus | Operator::Minus => (1, 2),
        Operator::Mul | Operator::Div => (3, 4),
        Operator::Comp => (10, 9),
        _ => return None,
    };
    Some(res)
}

impl S {
    pub fn eval(&self) -> f64 {
        match self {
            S::Literal(i) => *i,
            S::Cons(op, values) => {
                if values.len() == 1 {
                    return evaluate_operations(
                        *op,
                        (unsafe { values.last().unwrap_unchecked().eval() }, None),
                    );
                }
                if let [lhs, rhs] = &values[0..=1] {
                    return evaluate_operations(*op, (lhs.eval(), Some(rhs.eval())));
                }
                panic!("This shouldn't even be possible")
            }
        }
    }
}

fn evaluate_operations(op: Operator, (lhs, rhs): (f64, Option<f64>)) -> f64 {
    match op {
        Operator::Plus if rhs.is_none() => lhs,
        Operator::Plus => lhs + unsafe { rhs.unwrap_unchecked() },
        Operator::Minus if rhs.is_none() => -lhs,
        Operator::Minus => lhs - unsafe { rhs.unwrap_unchecked() },
        // The Composite Function operator seems to be meant for more language-like shit but without the aspects the
        // parser article gave, it's basically just a multiply but in a right associative way.
        Operator::Mul | Operator::Comp => lhs * unsafe { rhs.unwrap_unchecked() },
        Operator::Div => lhs / unsafe { rhs.unwrap_unchecked() },
        Operator::Fact => {
            let mut accumulation = 1.0;
            let mut iterations = 1.0;
            loop {
                iterations += 1.0;
                accumulation *= iterations;

                if iterations >= lhs {
                    break;
                }
            }
            accumulation
        }
        op => panic!("Non-evaluable operator: {op}"),
    }
}

#[test]
fn testing_full_expr() {
    let s = expr("1 + 2 * 3");
    assert_eq!(s.to_string(), "(+ 1 (* 2 3))")
}

#[test]
fn testing_simple_expr() {
    let s = expr("1");
    assert_eq!(s.to_string(), "1")
}

#[test]
fn testing_right_associative_operator() {
    let s = expr("8 . 3 . 1");
    assert_eq!(s.to_string(), "(. 8 (. 3 1))");

    let s = expr(" 1 + 2 + 9 . 8 . 7 * 3 * 4");
    assert_eq!(s.to_string(), "(+ (+ 1 2) (* (* (. 9 (. 8 7)) 3) 4))");
}

#[test]
fn testing_prefix_binding() {
    let s = expr("--1 * 2");
    assert_eq!(s.to_string(), "(* (- (- 1)) 2)");

    let s = expr("--4 . 8");
    assert_eq!(s.to_string(), "(- (- (. 4 8)))");
}

#[test]
fn testing_postfix_binding() {
    let s = expr("-9!");
    assert_eq!(s.to_string(), "(- (! 9))");

    let s = expr("5 . 8 !");
    assert_eq!(s.to_string(), "(! (. 5 8))");
}

#[test]
fn testing_parentheses() {
    let s = expr("5 * 55345 / (3 + 44) - 5");
    assert_eq!(s.to_string(), "(- (/ (* 5 55345) (+ 3 44)) 5)");
}

#[test]
fn testing_evaluation() {
    let s = expr("99 * (54 + 89) / (341 - 511) * -23");
    assert_eq!(s.eval().floor(), 1915.0);

    let s = expr("5!");
    assert_eq!(s.eval(), 120.0);
}
