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
