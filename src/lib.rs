use lexer::{Keyword, Operator, Token, Type};
use std::{iter::Peekable, process::exit, vec::IntoIter};

#[derive(Debug, PartialEq)]
pub enum Node {
    Null,
    String(String),
    Number(u32),
    Boolean(bool),
    NegationBang(Box<Node>),
    NegationMinus(Box<Node>), // Grouping(Node),
    Multiply(Box<Node>, Box<Node>),
    Divide(Box<Node>, Box<Node>),
}

fn make_literal(token_type: Type) -> Node {
    match token_type {
        Type::String(s) => Node::String(s),
        Type::Number(n) => Node::Number(n),
        Type::Keyword(k) => match k {
            Keyword::True | Keyword::False => Node::Boolean(k == Keyword::True),
            Keyword::Null => Node::Null,
            _ => Node::Number(0),
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

fn parse_literal(tokens: &mut Peekable<IntoIter<Token>>) -> Node {
    if let Some(token) = tokens.next_if(|t| {
        matches!(
            t.token_type,
            Type::String(_)
                | Type::Number(_)
                | Type::Keyword(Keyword::True)
                | Type::Keyword(Keyword::False)
                | Type::Keyword(Keyword::Null)
        )
    }) {
        return make_literal(token.token_type);
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

fn parse_expr(tokens: &mut Peekable<IntoIter<Token>>) -> Node {
    return parse_bin_multdiv(tokens);
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
}
