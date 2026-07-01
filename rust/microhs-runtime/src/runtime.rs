use std::collections::HashMap;
use std::fmt;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct NodeId(pub usize);

#[derive(Clone, Debug)]
pub enum Node {
    App(NodeId, NodeId),
    Indir(Option<NodeId>),
    Prim(String),
    Int(i64),
    Int64(i64),
    Float64(f64),
    Float32(f32),
    BigInt(Vec<u8>),
    Bytes(Vec<u8>),
    Array(Vec<NodeId>),
    Ffi(String),
    JsCall { tags: String, body: Vec<u8> },
    JsWrap { tags: String },
    FunPtr(String),
    Tick(Vec<u8>),
}

#[derive(Debug)]
pub enum EvalError {
    StepLimit { limit: usize },
    DanglingIndirection(NodeId),
    ExpectedInt(NodeId),
    DivideByZero,
    Overflow,
    InvalidShift(i64),
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::StepLimit { limit } => write!(f, "reduction step limit reached ({limit})"),
            Self::DanglingIndirection(id) => write!(f, "dangling shared reference at node {id:?}"),
            Self::ExpectedInt(id) => write!(f, "expected Int at node {id:?}"),
            Self::DivideByZero => write!(f, "integer division by zero"),
            Self::Overflow => write!(f, "integer overflow"),
            Self::InvalidShift(n) => write!(f, "invalid shift amount {n}"),
        }
    }
}

impl std::error::Error for EvalError {}

#[derive(Clone, Debug)]
pub struct Program {
    nodes: Vec<Node>,
    root: NodeId,
    labels: HashMap<usize, NodeId>,
}

impl Program {
    pub fn new(nodes: Vec<Node>, root: NodeId, labels: HashMap<usize, NodeId>) -> Self {
        Self {
            nodes,
            root,
            labels,
        }
    }

    pub fn root(&self) -> NodeId {
        self.root
    }

    pub fn nodes(&self) -> &[Node] {
        &self.nodes
    }

    pub fn label(&self, label: usize) -> Option<NodeId> {
        self.labels.get(&label).copied()
    }

    pub fn push_node(&mut self, node: Node) -> NodeId {
        let id = NodeId(self.nodes.len());
        self.nodes.push(node);
        id
    }

    pub fn resolve(&self, mut id: NodeId) -> Result<NodeId, EvalError> {
        loop {
            match self.nodes.get(id.0) {
                Some(Node::Indir(Some(next))) => id = *next,
                Some(Node::Indir(None)) => return Err(EvalError::DanglingIndirection(id)),
                Some(_) => return Ok(id),
                None => return Err(EvalError::DanglingIndirection(id)),
            }
        }
    }

    pub fn reduce_whnf(&mut self, limit: usize) -> Result<(NodeId, usize), EvalError> {
        let mut root = self.root;
        for steps in 0..limit {
            let Some(next) = self.step(root)? else {
                self.root = root;
                return Ok((root, steps));
            };
            root = next;
        }
        Err(EvalError::StepLimit { limit })
    }

    fn step(&mut self, root: NodeId) -> Result<Option<NodeId>, EvalError> {
        let (head, args) = self.spine(root)?;
        let Node::Prim(name) = self.nodes[head.0].clone() else {
            return Ok(None);
        };

        let rewrite = match name.as_str() {
            "I" if !args.is_empty() => Some((1, args[0])),
            "K" if args.len() >= 2 => Some((2, args[0])),
            "A" if args.len() >= 2 => Some((2, args[1])),
            "U" if args.len() >= 2 => {
                let n = self.app(args[1], args[0]);
                Some((2, n))
            }
            "S" if args.len() >= 3 => {
                let x = args[2];
                let left = self.app(args[0], x);
                let right = self.app(args[1], x);
                let n = self.app(left, right);
                Some((3, n))
            }
            "S'" if args.len() >= 4 => {
                let yw = self.app(args[1], args[3]);
                let zw = self.app(args[2], args[3]);
                let left = self.app(args[0], yw);
                let n = self.app(left, zw);
                Some((4, n))
            }
            "B" if args.len() >= 3 => {
                let yz = self.app(args[1], args[2]);
                let n = self.app(args[0], yz);
                Some((3, n))
            }
            "B'" if args.len() >= 4 => {
                let zw = self.app(args[2], args[3]);
                let xy = self.app(args[0], args[1]);
                let n = self.app(xy, zw);
                Some((4, n))
            }
            "B'" if args.len() >= 2 => {
                let xy = self.app(args[0], args[1]);
                let b = self.prim("B");
                let n = self.app(b, xy);
                Some((2, n))
            }
            "Z" if args.len() >= 3 => {
                let n = self.app(args[0], args[1]);
                Some((3, n))
            }
            "Z" if args.len() >= 2 => {
                let xy = self.app(args[0], args[1]);
                let k = self.prim("K");
                let n = self.app(k, xy);
                Some((2, n))
            }
            "J" if args.len() >= 3 => {
                let n = self.app(args[2], args[0]);
                Some((3, n))
            }
            "L" if args.len() >= 3 => {
                let n = self.app(args[1], args[0]);
                Some((3, n))
            }
            "KK" if args.len() >= 3 => Some((3, args[1])),
            "KA" if args.len() >= 3 => Some((3, args[2])),
            "C" if args.len() >= 3 => {
                let xz = self.app(args[0], args[2]);
                let n = self.app(xz, args[1]);
                Some((3, n))
            }
            "C'" if args.len() >= 4 => {
                let yw = self.app(args[1], args[3]);
                let xyw = self.app(args[0], yw);
                let n = self.app(xyw, args[2]);
                Some((4, n))
            }
            "P" if args.len() >= 3 => {
                let zx = self.app(args[2], args[0]);
                let n = self.app(zx, args[1]);
                Some((3, n))
            }
            "R" if args.len() >= 3 => {
                let yz = self.app(args[1], args[2]);
                let n = self.app(yz, args[0]);
                Some((3, n))
            }
            "R" if args.len() >= 2 => {
                let c = self.prim("C");
                let cy = self.app(c, args[1]);
                let n = self.app(cy, args[0]);
                Some((2, n))
            }
            "O" if args.len() >= 4 => {
                let wx = self.app(args[3], args[0]);
                let n = self.app(wx, args[1]);
                Some((4, n))
            }
            "K2" if args.len() >= 3 => Some((3, args[0])),
            "K2" if args.len() >= 2 => {
                let k = self.prim("K");
                let n = self.app(k, args[0]);
                Some((2, n))
            }
            "K3" if args.len() >= 4 => Some((4, args[0])),
            "K3" if args.len() >= 2 => {
                let k2 = self.prim("K2");
                let n = self.app(k2, args[0]);
                Some((2, n))
            }
            "K4" if args.len() >= 5 => Some((5, args[0])),
            "K4" if args.len() >= 2 => {
                let k3 = self.prim("K3");
                let n = self.app(k3, args[0]);
                Some((2, n))
            }
            "C'B" if args.len() >= 4 => {
                let yw = self.app(args[1], args[3]);
                let xz = self.app(args[0], args[2]);
                let n = self.app(xz, yw);
                Some((4, n))
            }
            "C'B" if args.len() >= 3 => {
                let xz = self.app(args[0], args[2]);
                let b = self.prim("B");
                let bxz = self.app(b, xz);
                let n = self.app(bxz, args[1]);
                Some((3, n))
            }
            "Y" if !args.is_empty() => {
                let y = self.prim("Y");
                let yy = self.app(y, args[0]);
                let n = self.app(args[0], yy);
                Some((1, n))
            }
            name if args.len() >= 2 && tag_index(name).is_some() => {
                let tag = self.push_node(Node::Int(tag_index(name).expect("checked tag") as i64));
                let ytag = self.app(args[1], tag);
                let n = self.app(ytag, args[0]);
                Some((2, n))
            }
            name if tuple_fields(name).is_some_and(|fields| args.len() > fields) => {
                let fields = tuple_fields(name).expect("checked tuple constructor");
                let mut n = args[fields];
                for arg in &args[..fields] {
                    n = self.app(n, *arg);
                }
                Some((fields + 1, n))
            }
            name if args.len() >= 2 => self.int_binop(name, &args)?,
            name if !args.is_empty() => self.int_unop(name, &args)?,
            _ => None,
        };

        let Some((used, mut node)) = rewrite else {
            return Ok(None);
        };
        for arg in &args[used..] {
            node = self.app(node, *arg);
        }
        Ok(Some(node))
    }

    fn spine(&self, root: NodeId) -> Result<(NodeId, Vec<NodeId>), EvalError> {
        let mut node = self.resolve(root)?;
        let mut args = Vec::new();
        while let Node::App(fun, arg) = self.nodes[node.0] {
            args.push(self.resolve(arg)?);
            node = self.resolve(fun)?;
        }
        args.reverse();
        Ok((node, args))
    }

    fn app(&mut self, fun: NodeId, arg: NodeId) -> NodeId {
        self.push_node(Node::App(fun, arg))
    }

    fn prim(&mut self, name: &str) -> NodeId {
        self.push_node(Node::Prim(name.to_owned()))
    }

    fn int_binop(
        &mut self,
        name: &str,
        args: &[NodeId],
    ) -> Result<Option<(usize, NodeId)>, EvalError> {
        let Some(op) = IntBinOp::from_prim(name) else {
            return Ok(None);
        };
        let x = self.eval_int(args[0])?;
        let y = self.eval_int(args[1])?;
        let node = match op.apply(x, y)? {
            IntResult::Int(n) => self.push_node(Node::Int(n)),
            IntResult::Bool(b) => self.prim(if b { "A" } else { "K" }),
        };
        Ok(Some((2, node)))
    }

    fn int_unop(
        &mut self,
        name: &str,
        args: &[NodeId],
    ) -> Result<Option<(usize, NodeId)>, EvalError> {
        let Some(op) = IntUnOp::from_prim(name) else {
            return Ok(None);
        };
        let x = self.eval_int(args[0])?;
        let node = self.push_node(Node::Int(op.apply(x)?));
        Ok(Some((1, node)))
    }

    fn eval_int(&mut self, id: NodeId) -> Result<i64, EvalError> {
        let root = self.reduce_node_whnf(id, 10_000)?;
        match self.nodes[self.resolve(root)?.0] {
            Node::Int(n) => Ok(n),
            _ => Err(EvalError::ExpectedInt(root)),
        }
    }

    fn reduce_node_whnf(&mut self, mut root: NodeId, limit: usize) -> Result<NodeId, EvalError> {
        for _ in 0..limit {
            let Some(next) = self.step(root)? else {
                return Ok(root);
            };
            root = next;
        }
        Err(EvalError::StepLimit { limit })
    }

    pub fn render(&self, root: NodeId) -> String {
        let mut out = String::new();
        self.render_into(root, 0, &mut out);
        out
    }

    fn render_into(&self, id: NodeId, depth: usize, out: &mut String) {
        if depth > 80 {
            out.push_str("...");
            return;
        }
        let Ok(id) = self.resolve(id) else {
            out.push_str("<dangling>");
            return;
        };
        match &self.nodes[id.0] {
            Node::App(fun, arg) => {
                out.push('(');
                self.render_into(*fun, depth + 1, out);
                out.push(' ');
                self.render_into(*arg, depth + 1, out);
                out.push(')');
            }
            Node::Indir(_) => out.push_str("<indir>"),
            Node::Prim(name) => out.push_str(name),
            Node::Int(n) => out.push_str(&n.to_string()),
            Node::Int64(n) => {
                out.push_str(&n.to_string());
                out.push_str("i64");
            }
            Node::Float64(n) => out.push_str(&n.to_string()),
            Node::Float32(n) => {
                out.push_str(&n.to_string());
                out.push('f');
            }
            Node::BigInt(bytes) => {
                out.push('%');
                render_bytes(bytes, out);
            }
            Node::Bytes(bytes) => render_bytes(bytes, out),
            Node::Array(items) => {
                out.push('[');
                for (idx, item) in items.iter().enumerate() {
                    if idx != 0 {
                        out.push_str(", ");
                    }
                    self.render_into(*item, depth + 1, out);
                }
                out.push(']');
            }
            Node::Ffi(name) => {
                out.push('^');
                out.push_str(name);
            }
            Node::JsCall { tags, body } => {
                out.push('~');
                out.push_str(tags);
                out.push(' ');
                render_bytes(body, out);
            }
            Node::JsWrap { tags } => {
                out.push('`');
                out.push_str(tags);
            }
            Node::FunPtr(name) => {
                out.push(';');
                out.push_str(name);
            }
            Node::Tick(name) => {
                out.push('!');
                render_bytes(name, out);
            }
        }
    }
}

enum IntResult {
    Int(i64),
    Bool(bool),
}

#[derive(Clone, Copy)]
enum IntBinOp {
    Add,
    Sub,
    Mul,
    Quot,
    Rem,
    SubR,
    UAdd,
    USub,
    UMul,
    UQuot,
    URem,
    USubR,
    And,
    Or,
    Xor,
    Shl,
    Shr,
    Ashr,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    Ult,
    Ule,
    Ugt,
    Uge,
}

impl IntBinOp {
    fn from_prim(name: &str) -> Option<Self> {
        Some(match name {
            "+" => Self::Add,
            "-" => Self::Sub,
            "*" => Self::Mul,
            "quot" => Self::Quot,
            "rem" => Self::Rem,
            "subtract" => Self::SubR,
            "u+" => Self::UAdd,
            "u-" => Self::USub,
            "u*" => Self::UMul,
            "uquot" => Self::UQuot,
            "urem" => Self::URem,
            "usubtract" => Self::USubR,
            "and" => Self::And,
            "or" => Self::Or,
            "xor" => Self::Xor,
            "shl" => Self::Shl,
            "shr" => Self::Shr,
            "ashr" => Self::Ashr,
            "==" => Self::Eq,
            "/=" => Self::Ne,
            "<" => Self::Lt,
            "<=" => Self::Le,
            ">" => Self::Gt,
            ">=" => Self::Ge,
            "u<" => Self::Ult,
            "u<=" => Self::Ule,
            "u>" => Self::Ugt,
            "u>=" => Self::Uge,
            _ => return None,
        })
    }

    fn apply(self, x: i64, y: i64) -> Result<IntResult, EvalError> {
        let xu = x as u64;
        let yu = y as u64;
        let n = match self {
            Self::Add => x.checked_add(y).ok_or(EvalError::Overflow)?,
            Self::Sub => x.checked_sub(y).ok_or(EvalError::Overflow)?,
            Self::Mul => x.checked_mul(y).ok_or(EvalError::Overflow)?,
            Self::Quot => {
                if y == 0 {
                    return Err(EvalError::DivideByZero);
                }
                x.checked_div(y).ok_or(EvalError::Overflow)?
            }
            Self::Rem => {
                if y == 0 {
                    return Err(EvalError::DivideByZero);
                }
                x.checked_rem(y).ok_or(EvalError::Overflow)?
            }
            Self::SubR => y.checked_sub(x).ok_or(EvalError::Overflow)?,
            Self::UAdd => xu.wrapping_add(yu) as i64,
            Self::USub => xu.wrapping_sub(yu) as i64,
            Self::UMul => xu.wrapping_mul(yu) as i64,
            Self::UQuot => {
                if yu == 0 {
                    return Err(EvalError::DivideByZero);
                }
                (xu / yu) as i64
            }
            Self::URem => {
                if yu == 0 {
                    return Err(EvalError::DivideByZero);
                }
                (xu % yu) as i64
            }
            Self::USubR => yu.wrapping_sub(xu) as i64,
            Self::And => (xu & yu) as i64,
            Self::Or => (xu | yu) as i64,
            Self::Xor => (xu ^ yu) as i64,
            Self::Shl => (xu.wrapping_shl(shift(y)?)) as i64,
            Self::Shr => (xu.wrapping_shr(shift(y)?)) as i64,
            Self::Ashr => x.wrapping_shr(shift(y)?),
            Self::Eq => return Ok(IntResult::Bool(xu == yu)),
            Self::Ne => return Ok(IntResult::Bool(xu != yu)),
            Self::Lt => return Ok(IntResult::Bool(x < y)),
            Self::Le => return Ok(IntResult::Bool(x <= y)),
            Self::Gt => return Ok(IntResult::Bool(x > y)),
            Self::Ge => return Ok(IntResult::Bool(x >= y)),
            Self::Ult => return Ok(IntResult::Bool(xu < yu)),
            Self::Ule => return Ok(IntResult::Bool(xu <= yu)),
            Self::Ugt => return Ok(IntResult::Bool(xu > yu)),
            Self::Uge => return Ok(IntResult::Bool(xu >= yu)),
        };
        Ok(IntResult::Int(n))
    }
}

#[derive(Clone, Copy)]
enum IntUnOp {
    Neg,
    UNeg,
    Inv,
    PopCount,
    Clz,
    Ctz,
}

impl IntUnOp {
    fn from_prim(name: &str) -> Option<Self> {
        Some(match name {
            "neg" => Self::Neg,
            "uneg" => Self::UNeg,
            "inv" => Self::Inv,
            "popcount" => Self::PopCount,
            "clz" => Self::Clz,
            "ctz" => Self::Ctz,
            _ => return None,
        })
    }

    fn apply(self, x: i64) -> Result<i64, EvalError> {
        Ok(match self {
            Self::Neg => x.checked_neg().ok_or(EvalError::Overflow)?,
            Self::UNeg => (0u64.wrapping_sub(x as u64)) as i64,
            Self::Inv => !(x as u64) as i64,
            Self::PopCount => (x as u64).count_ones() as i64,
            Self::Clz => (x as u64).leading_zeros() as i64,
            Self::Ctz => (x as u64).trailing_zeros() as i64,
        })
    }
}

fn shift(n: i64) -> Result<u32, EvalError> {
    if !(0..64).contains(&n) {
        return Err(EvalError::InvalidShift(n));
    }
    Ok(n as u32)
}

fn tag_index(name: &str) -> Option<usize> {
    let tag = name.strip_prefix("TAG")?.parse().ok()?;
    (tag <= 32).then_some(tag)
}

fn tuple_fields(name: &str) -> Option<usize> {
    let fields = name.strip_prefix('T')?.parse().ok()?;
    (3..=16).contains(&fields).then_some(fields)
}

fn render_bytes(bytes: &[u8], out: &mut String) {
    out.push('"');
    for &byte in bytes {
        match byte {
            b'\\' => out.push_str("\\\\"),
            b'"' => out.push_str("\\\""),
            b'\n' => out.push_str("\\n"),
            b'\r' => out.push_str("\\r"),
            b'\t' => out.push_str("\\t"),
            0x20..=0x7e => out.push(byte as char),
            _ => {
                out.push_str("\\x");
                out.push(nibble(byte >> 4));
                out.push(nibble(byte & 0x0f));
            }
        }
    }
    out.push('"');
}

fn nibble(n: u8) -> char {
    match n {
        0..=9 => (b'0' + n) as char,
        10..=15 => (b'a' + n - 10) as char,
        _ => unreachable!(),
    }
}

#[cfg(test)]
mod tests {
    use crate::parse_program;

    fn whnf(input: &[u8]) -> String {
        let mut program = parse_program(input).unwrap();
        let (root, _) = program.reduce_whnf(100).unwrap();
        program.render(root)
    }

    #[test]
    fn reduces_identity() {
        assert_eq!(whnf(b"v8.4\n0\nI #42 @ }"), "42");
    }

    #[test]
    fn reduces_skk_identity() {
        assert_eq!(whnf(b"v8.4\n0\nS K @ K @ #7 @ }"), "7");
    }

    #[test]
    fn reduces_optimizer_combinators() {
        assert_eq!(whnf(b"v8.4\n0\nS' K @ K @ K @ #5 @ #0 @ }"), "5");
        assert_eq!(whnf(b"v8.4\n0\nB' K @ #5 @ K @ #0 @ }"), "5");
        assert_eq!(whnf(b"v8.4\n0\nZ K @ #5 @ #0 @ #1 @ }"), "5");
        assert_eq!(whnf(b"v8.4\n0\nJ #5 @ #0 @ I @ }"), "5");
        assert_eq!(whnf(b"v8.4\n0\nL #5 @ I @ #0 @ }"), "5");
        assert_eq!(whnf(b"v8.4\n0\nKK #0 @ #5 @ #1 @ }"), "5");
        assert_eq!(whnf(b"v8.4\n0\nKA #0 @ #1 @ #5 @ }"), "5");
        assert_eq!(whnf(b"v8.4\n0\nC' A @ K @ #5 @ #0 @ }"), "5");
        assert_eq!(whnf(b"v8.4\n0\nR #0 @ K @ #5 @ }"), "5");
        assert_eq!(whnf(b"v8.4\n0\nO #5 @ #0 @ #1 @ K @ }"), "5");
        assert_eq!(whnf(b"v8.4\n0\nC'B K @ K @ #5 @ #0 @ }"), "5");
    }

    #[test]
    fn reduces_partial_arity_specializations() {
        assert_eq!(whnf(b"v8.4\n0\nB' I @ I @ #9 @ }"), "((B (I I)) 9)");
        assert_eq!(whnf(b"v8.4\n0\nZ K @ #5 @ }"), "(K (K 5))");
        assert_eq!(whnf(b"v8.4\n0\nR #1 @ #2 @ }"), "((C 2) 1)");
        assert_eq!(whnf(b"v8.4\n0\nK2 #1 @ #2 @ }"), "(K 1)");
        assert_eq!(whnf(b"v8.4\n0\nK3 #1 @ #2 @ #3 @ }"), "(K 1)");
        assert_eq!(whnf(b"v8.4\n0\nC'B K @ I @ #9 @ }"), "((B (K 9)) I)");
    }

    #[test]
    fn reduces_constructor_tags_and_tuples() {
        assert_eq!(whnf(b"v8.4\n0\nTAG3 #99 @ K @ }"), "3");
        assert_eq!(whnf(b"v8.4\n0\nTAG10 #99 @ A @ }"), "99");
        assert_eq!(whnf(b"v8.4\n0\nT3 #1 @ #2 @ #3 @ K3 @ #0 @ }"), "1");
        assert_eq!(whnf(b"v8.4\n0\nT4 #1 @ #2 @ #3 @ #4 @ K4 @ #0 @ }"), "1");
    }

    #[test]
    fn resolves_shared_labels() {
        assert_eq!(whnf(b"v8.4\n1\nA #42 :0 @ _0 @ }"), "42");
    }

    #[test]
    fn reduces_integer_arithmetic() {
        assert_eq!(whnf(b"v8.4\n0\n+ #40 @ #2 @ }"), "42");
        assert_eq!(whnf(b"v8.4\n0\nsubtract #10 @ #3 @ }"), "-7");
        assert_eq!(whnf(b"v8.4\n0\nquot #22 @ #5 @ }"), "4");
        assert_eq!(whnf(b"v8.4\n0\nrem #22 @ #5 @ }"), "2");
    }

    #[test]
    fn reduces_integer_bit_ops() {
        assert_eq!(whnf(b"v8.4\n0\nand #6 @ #3 @ }"), "2");
        assert_eq!(whnf(b"v8.4\n0\nor #4 @ #1 @ }"), "5");
        assert_eq!(whnf(b"v8.4\n0\nshl #3 @ #2 @ }"), "12");
        assert_eq!(whnf(b"v8.4\n0\npopcount #7 @ }"), "3");
    }

    #[test]
    fn reduces_integer_comparisons_to_microhs_bools() {
        assert_eq!(whnf(b"v8.4\n0\n== #2 @ #2 @ }"), "A");
        assert_eq!(whnf(b"v8.4\n0\n< #2 @ #1 @ }"), "K");
        assert_eq!(whnf(b"v8.4\n0\nu> #-1 @ #1 @ }"), "A");
    }
}
