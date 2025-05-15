use lexer::{Keyword, Operator, Token, Type};
use std::{iter::Peekable, mem, process::exit, vec::IntoIter};

#[derive(Debug, PartialEq)]
pub enum Node {
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
        Type::Keyword(k) if k == Keyword::True || k == Keyword::False => {
            Node::Boolean(k == Keyword::True)
        }
        _ => Node::Number(0),
    }
}

fn parse_literal(tokens: &mut Peekable<IntoIter<Token>>) -> Node {
    if let Some(token) = tokens.next_if(|t| {
        t.is_types(vec![
            Type::String(String::new()),
            Type::Number(0),
            Type::Keyword(Keyword::True),
            Type::Keyword(Keyword::False),
        ])
    }) {
        return make_literal(token.token_type);
    } else {
        exit(-1); // TODO: error
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
}
