use lexer::{Keyword, Operator, Token, Type};
use std::{iter::Peekable, process::exit, vec::IntoIter};

#[derive(Debug, PartialEq)]
pub enum Node {
    Null,
    String(String),
    Number(i32),
    Boolean(bool),
    NegationBang(Box<Node>),
    NegationMinus(Box<Node>),
    Add(Box<Node>, Box<Node>),
    Subtract(Box<Node>, Box<Node>),
    Multiply(Box<Node>, Box<Node>),
    Divide(Box<Node>, Box<Node>),
    Greater(Box<Node>, Box<Node>),
    GreaterEq(Box<Node>, Box<Node>),
    Less(Box<Node>, Box<Node>),
    LessEq(Box<Node>, Box<Node>),
    Equals(Box<Node>, Box<Node>),
    NotEquals(Box<Node>, Box<Node>),
}

fn make_literal(token_type: Type) -> Node {
    match token_type {
        Type::String(s) => Node::String(s),
        Type::Number(n) => Node::Number(n),
        Type::Keyword(k) => match k {
            Keyword::True | Keyword::False => Node::Boolean(k == Keyword::True),
            Keyword::Null => Node::Null,
            _ => Node::Number(0),
        },
        _ => Node::Number(0),
    }
}

fn make_unary(token_type: Type, child: Node) -> Node {
    match token_type {
        Type::Operator(op) => match op {
            Operator::Bang => Node::NegationBang(Box::new(child)),
            Operator::Minus => Node::NegationMinus(Box::new(child)),
            _ => Node::Number(0), // TODO: error
        },
        _ => Node::Number(0), // TODO: Error
    }
}

fn make_binary(token_type: Type, lhs: Node, rhs: Node) -> Node {
    match token_type {
        Type::Operator(op) => match op {
            Operator::Plus => Node::Add(Box::new(lhs), Box::new(rhs)),
            Operator::Minus => Node::Subtract(Box::new(lhs), Box::new(rhs)),
            Operator::Star => Node::Multiply(Box::new(lhs), Box::new(rhs)),
            Operator::Slash => Node::Divide(Box::new(lhs), Box::new(rhs)),
            Operator::Greater => Node::Greater(Box::new(lhs), Box::new(rhs)),
            Operator::GreaterEqual => Node::GreaterEq(Box::new(lhs), Box::new(rhs)),
            Operator::Less => Node::Less(Box::new(lhs), Box::new(rhs)),
            Operator::LessEqual => Node::LessEq(Box::new(lhs), Box::new(rhs)),
            Operator::DoubleEquals => Node::Equals(Box::new(lhs), Box::new(rhs)),
            Operator::NotEquals => Node::NotEquals(Box::new(lhs), Box::new(rhs)),
            _ => Node::Number(0), // TODO: error
        },
        _ => Node::Number(0), // TODO: Error
    }
}

fn parse_literal(tokens: &mut Peekable<IntoIter<Token>>) -> Node {
    if let Some(token) = tokens.next_if(|t| {
        matches!(
            t.token_type,
            Type::String(_)
                | Type::Number(_)
                | Type::Keyword(Keyword::True)
                | Type::Keyword(Keyword::False)
                | Type::Keyword(Keyword::Null)
                | Type::LeftParen
        )
    }) {
        match token.token_type {
            Type::LeftParen => {
                let inner = parse_expr(tokens);
                if let Some(_) = tokens.next_if(|t| matches!(t.token_type, Type::RightParen)) {
                    return inner;
                } else {
                    exit(-1); // TODO: Error
                }
            }
            _ => return make_literal(token.token_type),
        }
    } else {
        exit(-1); // TODO: error
    }
}

fn parse_unary(tokens: &mut Peekable<IntoIter<Token>>) -> Node {
    if let Some(token) = tokens.next_if(|t| {
        matches!(
            t.token_type,
            Type::Operator(Operator::Bang) | Type::Operator(Operator::Minus)
        )
    }) {
        let rhs = parse_unary(tokens);
        return make_unary(token.token_type, rhs);
    } else {
        return parse_literal(tokens);
    }
}

fn parse_bin_multdiv(tokens: &mut Peekable<IntoIter<Token>>) -> Node {
    let mut lhs = parse_unary(tokens);
    while let Some(token) = tokens.next_if(|t| {
        matches!(
            t.token_type,
            Type::Operator(Operator::Star) | Type::Operator(Operator::Slash)
        )
    }) {
        let rhs = parse_unary(tokens);
        lhs = make_binary(token.token_type, lhs, rhs);
    }
    return lhs;
}

fn parse_bin_addsub(tokens: &mut Peekable<IntoIter<Token>>) -> Node {
    let mut lhs = parse_bin_multdiv(tokens);
    while let Some(token) = tokens.next_if(|t| {
        matches!(
            t.token_type,
            Type::Operator(Operator::Plus) | Type::Operator(Operator::Minus)
        )
    }) {
        let rhs = parse_bin_multdiv(tokens);
        lhs = make_binary(token.token_type, lhs, rhs);
    }
    return lhs;
}

fn parse_bin_comp(tokens: &mut Peekable<IntoIter<Token>>) -> Node {
    let mut lhs = parse_bin_addsub(tokens);
    while let Some(token) = tokens.next_if(|t| {
        matches!(
            t.token_type,
            Type::Operator(Operator::Greater)
                | Type::Operator(Operator::GreaterEqual)
                | Type::Operator(Operator::Less)
                | Type::Operator(Operator::LessEqual)
        )
    }) {
        let rhs = parse_bin_addsub(tokens);
        lhs = make_binary(token.token_type, lhs, rhs);
    }
    return lhs;
}

fn parse_bin_eq(tokens: &mut Peekable<IntoIter<Token>>) -> Node {
    let mut lhs = parse_bin_comp(tokens);
    while let Some(token) = tokens.next_if(|t| {
        matches!(
            t.token_type,
            Type::Operator(Operator::DoubleEquals) | Type::Operator(Operator::NotEquals)
        )
    }) {
        let rhs = parse_bin_comp(tokens);
        lhs = make_binary(token.token_type, lhs, rhs);
    }
    return lhs;
}

pub fn parse_expr(tokens: &mut Peekable<IntoIter<Token>>) -> Node {
    return parse_bin_eq(tokens);
}

#[cfg(test)]
mod tests {
    use crate::{Keyword, Node, Operator, Token, Type, parse_expr};

    #[test]
    fn test_literals() {
        assert_eq!(
            parse_expr(&mut vec![Token::new(Type::Number(3))].into_iter().peekable()),
            Node::Number(3)
        );
        assert_eq!(
            parse_expr(
                &mut vec![Token::new(Type::Keyword(Keyword::True))]
                    .into_iter()
                    .peekable()
            ),
            Node::Boolean(true)
        );
        assert_eq!(
            parse_expr(
                &mut vec![Token::new(Type::Keyword(Keyword::False))]
                    .into_iter()
                    .peekable()
            ),
            Node::Boolean(false)
        );
        assert_eq!(
            parse_expr(
                &mut vec![Token::new(Type::String(String::from("meow")))]
                    .into_iter()
                    .peekable()
            ),
            Node::String(String::from("meow"))
        );
    }

    #[test]
    fn test_unary() {
        assert_eq!(
            parse_expr(
                &mut vec![
                    Token::new(Type::Operator(Operator::Minus)),
                    Token::new(Type::Number(3))
                ]
                .into_iter()
                .peekable()
            ),
            Node::NegationMinus(Box::new(Node::Number(3)))
        );
        assert_eq!(
            parse_expr(
                &mut vec![
                    Token::new(Type::Operator(Operator::Bang)),
                    Token::new(Type::Keyword(Keyword::False))
                ]
                .into_iter()
                .peekable()
            ),
            Node::NegationBang(Box::new(Node::Boolean(false)))
        );
        assert_eq!(
            parse_expr(
                &mut vec![
                    Token::new(Type::Operator(Operator::Minus)),
                    Token::new(Type::Operator(Operator::Minus)),
                    Token::new(Type::Number(3))
                ]
                .into_iter()
                .peekable()
            ),
            Node::NegationMinus(Box::new(Node::NegationMinus(Box::new(Node::Number(3)))))
        );
        assert_eq!(
            parse_expr(
                &mut vec![
                    Token::new(Type::Operator(Operator::Bang)),
                    Token::new(Type::Operator(Operator::Bang)),
                    Token::new(Type::Keyword(Keyword::True))
                ]
                .into_iter()
                .peekable()
            ),
            Node::NegationBang(Box::new(Node::NegationBang(Box::new(Node::Boolean(true)))))
        );
    }

    #[test]
    fn test_binary() {
        assert_eq!(
            parse_expr(
                &mut vec![
                    Token::new(Type::Number(4)),
                    Token::new(Type::Operator(Operator::Star)),
                    Token::new(Type::Number(4))
                ]
                .into_iter()
                .peekable()
            ),
            Node::Multiply(Box::new(Node::Number(4)), Box::new(Node::Number(4)))
        );
        assert_eq!(
            parse_expr(
                &mut vec![
                    Token::new(Type::Number(8)),
                    Token::new(Type::Operator(Operator::Star)),
                    Token::new(Type::Number(8)),
                    Token::new(Type::Operator(Operator::Star)),
                    Token::new(Type::Number(4)),
                    Token::new(Type::Operator(Operator::Star)),
                    Token::new(Type::Number(4))
                ]
                .into_iter()
                .peekable()
            ),
            Node::Multiply(
                Box::new(Node::Multiply(
                    Box::new(Node::Multiply(
                        Box::new(Node::Number(8)),
                        Box::new(Node::Number(8))
                    )),
                    Box::new(Node::Number(4))
                )),
                Box::new(Node::Number(4))
            )
        );
        assert_eq!(
            parse_expr(
                &mut vec![
                    Token::new(Type::Number(6)),
                    Token::new(Type::Operator(Operator::Slash)),
                    Token::new(Type::Number(3)),
                    Token::new(Type::Operator(Operator::Plus)),
                    Token::new(Type::Number(1))
                ]
                .into_iter()
                .peekable()
            ),
            Node::Add(
                Box::new(Node::Divide(
                    Box::new(Node::Number(6)),
                    Box::new(Node::Number(3))
                )),
                Box::new(Node::Number(1)),
            )
        );
        assert_eq!(
            parse_expr(
                &mut vec![
                    Token::new(Type::Number(5)),
                    Token::new(Type::Operator(Operator::Star)),
                    Token::new(Type::Number(3)),
                    Token::new(Type::Operator(Operator::Plus)),
                    Token::new(Type::Number(3)),
                    Token::new(Type::Operator(Operator::Star)),
                    Token::new(Type::Number(3)),
                ]
                .into_iter()
                .peekable()
            ),
            Node::Add(
                Box::new(Node::Multiply(
                    Box::new(Node::Number(5)),
                    Box::new(Node::Number(3))
                )),
                Box::new(Node::Multiply(
                    Box::new(Node::Number(3)),
                    Box::new(Node::Number(3))
                ))
            )
        );
        assert_eq!(
            parse_expr(
                &mut vec![
                    Token::new(Type::Number(5)),
                    Token::new(Type::Operator(Operator::Plus)),
                    Token::new(Type::Operator(Operator::Minus)),
                    Token::new(Type::Number(4))
                ]
                .into_iter()
                .peekable()
            ),
            Node::Add(
                Box::new(Node::Number(5)),
                Box::new(Node::NegationMinus(Box::new(Node::Number(4))))
            )
        );
    }

    #[test]
    fn test() {
        let mut data = vec![
            Token::new(Type::Number(5)),
            Token::new(Type::Operator(Operator::Star)),
            Token::new(Type::LeftParen),
            Token::new(Type::Number(3)),
            Token::new(Type::Operator(Operator::Plus)),
            Token::new(Type::Number(3)),
            Token::new(Type::RightParen),
            Token::new(Type::Operator(Operator::Star)),
            Token::new(Type::Number(3)),
        ]
        .into_iter()
        .peekable();
        dbg!(parse_expr(&mut data));
    }
}
