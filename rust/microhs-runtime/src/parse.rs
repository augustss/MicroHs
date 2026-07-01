use std::collections::HashMap;
use std::fmt;

use crate::runtime::{Node, NodeId, Program};

const COMB_VERSION: &[u8] = b"v8.4\n";

#[derive(Debug)]
pub enum ParseError {
    Version,
    Eof,
    Expected { expected: u8, got: Option<u8> },
    InvalidNumber,
    InvalidUtf8,
    StackUnderflow,
    StackJunk,
    DuplicateLabel(usize),
    DanglingLabel(usize),
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Version => write!(f, "unsupported combinator file version"),
            Self::Eof => write!(f, "unexpected end of file"),
            Self::Expected { expected, got } => match got {
                Some(got) => write!(
                    f,
                    "expected {:?}, got {:?}",
                    *expected as char, *got as char
                ),
                None => write!(f, "expected {:?}, got EOF", *expected as char),
            },
            Self::InvalidNumber => write!(f, "invalid number"),
            Self::InvalidUtf8 => write!(f, "invalid utf-8 in token"),
            Self::StackUnderflow => write!(f, "combinator parse stack underflow"),
            Self::StackJunk => write!(f, "combinator parse stack left extra values"),
            Self::DuplicateLabel(label) => write!(f, "duplicate shared label {label}"),
            Self::DanglingLabel(label) => write!(f, "dangling shared label {label}"),
        }
    }
}

impl std::error::Error for ParseError {}

pub fn parse_program(input: &[u8]) -> Result<Program, ParseError> {
    Parser::new(input).parse_top()
}

struct Parser<'a> {
    input: &'a [u8],
    pos: usize,
    nodes: Vec<Node>,
    stack: Vec<NodeId>,
    labels: HashMap<usize, NodeId>,
}

impl<'a> Parser<'a> {
    fn new(input: &'a [u8]) -> Self {
        Self {
            input,
            pos: 0,
            nodes: Vec::new(),
            stack: Vec::new(),
            labels: HashMap::new(),
        }
    }

    fn parse_top(mut self) -> Result<Program, ParseError> {
        if !self.input.starts_with(COMB_VERSION) {
            return Err(ParseError::Version);
        }
        self.pos = COMB_VERSION.len();
        self.gobble(b'\r');
        let _num_labels = self.parse_usize()?;
        self.expect(b'\n')?;
        self.gobble(b'\r');
        let root = self.parse_expr()?;
        for (label, id) in &self.labels {
            if matches!(self.nodes[id.0], Node::Indir(None)) {
                return Err(ParseError::DanglingLabel(*label));
            }
        }
        Ok(Program::new(self.nodes, root, self.labels))
    }

    fn parse_expr(&mut self) -> Result<NodeId, ParseError> {
        loop {
            let c = self.next_non_space()?;
            match c {
                b'@' => {
                    let x = self.pop()?;
                    let y = self.pop()?;
                    let app = self.push(Node::App(y, x));
                    self.stack.push(app);
                }
                b'}' => {
                    let root = self.pop()?;
                    if !self.stack.is_empty() {
                        return Err(ParseError::StackJunk);
                    }
                    return Ok(root);
                }
                b'%' => {
                    self.expect(b'"')?;
                    let digits = self.parse_string()?;
                    let node = self.push(Node::BigInt(digits));
                    self.stack.push(node);
                }
                b'&' => {
                    let is32 = self.gobble(b'&');
                    let token = self.token_after_prefix();
                    let text = std::str::from_utf8(&token).map_err(|_| ParseError::InvalidUtf8)?;
                    let node = if is32 {
                        Node::Float32(text.parse().map_err(|_| ParseError::InvalidNumber)?)
                    } else {
                        Node::Float64(text.parse().map_err(|_| ParseError::InvalidNumber)?)
                    };
                    let id = self.push(node);
                    self.stack.push(id);
                }
                b'#' => {
                    let node = if self.gobble(b'#') {
                        Node::Int64(self.parse_i64()?)
                    } else {
                        Node::Int(self.parse_i64()?)
                    };
                    let id = self.push(node);
                    self.stack.push(id);
                }
                b'[' => {
                    let size = self.parse_usize()?;
                    self.expect(b']')?;
                    if self.stack.len() < size {
                        return Err(ParseError::StackUnderflow);
                    }
                    let start = self.stack.len() - size;
                    let items = self.stack.drain(start..).collect();
                    let id = self.push(Node::Array(items));
                    self.stack.push(id);
                }
                b'_' => {
                    let label = self.parse_usize()?;
                    let id = self.ref_label(label);
                    self.stack.push(id);
                }
                b':' => {
                    let label = self.parse_usize()?;
                    self.expect(b' ')?;
                    let top = *self.stack.last().ok_or(ParseError::StackUnderflow)?;
                    self.define_label(label, top)?;
                }
                b'"' => {
                    let bytes = self.parse_string()?;
                    let id = self.push(Node::Bytes(bytes));
                    self.stack.push(id);
                }
                b'$' => {
                    let len = self.parse_usize()?;
                    self.expect(b' ')?;
                    if self.input.len().saturating_sub(self.pos) < len {
                        return Err(ParseError::Eof);
                    }
                    let bytes = self.input[self.pos..self.pos + len].to_vec();
                    self.pos += len;
                    let id = self.push(Node::Bytes(bytes));
                    self.stack.push(id);
                }
                b'!' => {
                    self.expect(b'"')?;
                    let name = self.parse_string()?;
                    let id = self.push(Node::Tick(name));
                    self.stack.push(id);
                }
                b'^' => {
                    let name = self.token_after_prefix_string()?;
                    let id = self.push(Node::Ffi(name));
                    self.stack.push(id);
                }
                b'~' => {
                    let tags = self.token_after_prefix_string()?;
                    self.expect(b'"')?;
                    let body = self.parse_string()?;
                    let id = self.push(Node::JsCall { tags, body });
                    self.stack.push(id);
                }
                b'`' => {
                    let tags = self.token_after_prefix_string()?;
                    let id = self.push(Node::JsWrap { tags });
                    self.stack.push(id);
                }
                b';' => {
                    let name = self.token_after_prefix_string()?;
                    let id = self.push(Node::FunPtr(name));
                    self.stack.push(id);
                }
                _ => {
                    let name = self.token_string(c)?;
                    let id = self.push(Node::Prim(name));
                    self.stack.push(id);
                }
            }
        }
    }

    fn next_non_space(&mut self) -> Result<u8, ParseError> {
        loop {
            let c = self.get()?;
            if !matches!(c, b' ' | b'\n' | b'\r') {
                return Ok(c);
            }
        }
    }

    fn parse_string(&mut self) -> Result<Vec<u8>, ParseError> {
        let mut bytes = Vec::new();
        loop {
            let mut c = self.get()?;
            if c == b'"' {
                return Ok(bytes);
            }
            match c {
                b'\\' => {
                    c = self.get()?;
                    c = match c {
                        b'?' => 0x7f,
                        b'_' => 0xff,
                        other => other,
                    };
                }
                b'^' => {
                    c = self.get()?;
                    c = if c < 0x40 {
                        c & 0x1f
                    } else {
                        (c & 0x1f) | 0x80
                    };
                }
                b'|' => {
                    c = self.get()? | 0x80;
                }
                _ => {}
            }
            bytes.push(c);
        }
    }

    fn token_string(&mut self, first: u8) -> Result<String, ParseError> {
        String::from_utf8(self.token(first)).map_err(|_| ParseError::InvalidUtf8)
    }

    fn token(&mut self, first: u8) -> Vec<u8> {
        let mut token = vec![first];
        token.extend(self.token_after_prefix());
        token
    }

    fn token_after_prefix_string(&mut self) -> Result<String, ParseError> {
        String::from_utf8(self.token_after_prefix()).map_err(|_| ParseError::InvalidUtf8)
    }

    fn token_after_prefix(&mut self) -> Vec<u8> {
        let mut token = Vec::new();
        while let Some(c) = self.peek() {
            if matches!(c, b' ' | b'\n') {
                self.pos += 1;
                break;
            }
            token.push(c);
            self.pos += 1;
        }
        token
    }

    fn parse_i64(&mut self) -> Result<i64, ParseError> {
        let sign = if self.gobble(b'-') { -1 } else { 1 };
        let mut saw_digit = false;
        let mut value: i64 = 0;
        while let Some(c) = self.peek() {
            if !c.is_ascii_digit() {
                break;
            }
            saw_digit = true;
            self.pos += 1;
            value = value
                .checked_mul(10)
                .and_then(|v| v.checked_add((c - b'0') as i64))
                .ok_or(ParseError::InvalidNumber)?;
        }
        if !saw_digit {
            return Err(ParseError::InvalidNumber);
        }
        Ok(value * sign)
    }

    fn parse_usize(&mut self) -> Result<usize, ParseError> {
        let value = self.parse_i64()?;
        usize::try_from(value).map_err(|_| ParseError::InvalidNumber)
    }

    fn ref_label(&mut self, label: usize) -> NodeId {
        if let Some(id) = self.labels.get(&label) {
            *id
        } else {
            let id = self.push(Node::Indir(None));
            self.labels.insert(label, id);
            id
        }
    }

    fn define_label(&mut self, label: usize, target: NodeId) -> Result<(), ParseError> {
        if let Some(id) = self.labels.get(&label).copied() {
            match &mut self.nodes[id.0] {
                Node::Indir(slot @ None) => {
                    *slot = Some(target);
                    Ok(())
                }
                _ => Err(ParseError::DuplicateLabel(label)),
            }
        } else {
            self.labels.insert(label, target);
            Ok(())
        }
    }

    fn push(&mut self, node: Node) -> NodeId {
        let id = NodeId(self.nodes.len());
        self.nodes.push(node);
        id
    }

    fn pop(&mut self) -> Result<NodeId, ParseError> {
        self.stack.pop().ok_or(ParseError::StackUnderflow)
    }

    fn expect(&mut self, expected: u8) -> Result<(), ParseError> {
        let got = self.get().ok();
        if got == Some(expected) {
            Ok(())
        } else {
            Err(ParseError::Expected { expected, got })
        }
    }

    fn gobble(&mut self, expected: u8) -> bool {
        if self.peek() == Some(expected) {
            self.pos += 1;
            true
        } else {
            false
        }
    }

    fn get(&mut self) -> Result<u8, ParseError> {
        let c = self.peek().ok_or(ParseError::Eof)?;
        self.pos += 1;
        Ok(c)
    }

    fn peek(&self) -> Option<u8> {
        self.input.get(self.pos).copied()
    }
}
