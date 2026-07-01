use std::cmp::Ordering;
use std::collections::{HashMap, HashSet};
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
    MutableBytes { bytes: Vec<u8>, capacity: usize },
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
    ExpectedInt64(NodeId),
    ExpectedFloat64(NodeId),
    ExpectedFloat32(NodeId),
    ExpectedBytes(NodeId),
    ExpectedArray(NodeId),
    DivideByZero,
    Overflow,
    InvalidShift(i64),
    InvalidByteString,
    InvalidArray,
    Raised(NodeId),
    InvalidStablePtr,
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::StepLimit { limit } => write!(f, "reduction step limit reached ({limit})"),
            Self::DanglingIndirection(id) => write!(f, "dangling shared reference at node {id:?}"),
            Self::ExpectedInt(id) => write!(f, "expected Int at node {id:?}"),
            Self::ExpectedInt64(id) => write!(f, "expected Int64 at node {id:?}"),
            Self::ExpectedFloat64(id) => write!(f, "expected Float64 at node {id:?}"),
            Self::ExpectedFloat32(id) => write!(f, "expected Float32 at node {id:?}"),
            Self::ExpectedBytes(id) => write!(f, "expected ByteString at node {id:?}"),
            Self::ExpectedArray(id) => write!(f, "expected Array at node {id:?}"),
            Self::DivideByZero => write!(f, "integer division by zero"),
            Self::Overflow => write!(f, "integer overflow"),
            Self::InvalidShift(n) => write!(f, "invalid shift amount {n}"),
            Self::InvalidByteString => write!(f, "invalid ByteString operation"),
            Self::InvalidArray => write!(f, "invalid Array operation"),
            Self::Raised(id) => write!(f, "uncaught exception at node {id:?}"),
            Self::InvalidStablePtr => write!(f, "invalid StablePtr operation"),
        }
    }
}

impl std::error::Error for EvalError {}

#[derive(Clone, Debug)]
pub struct Program {
    nodes: Vec<Node>,
    root: NodeId,
    labels: HashMap<usize, NodeId>,
    stable_ptrs: Vec<Option<NodeId>>,
}

impl Program {
    pub fn new(nodes: Vec<Node>, root: NodeId, labels: HashMap<usize, NodeId>) -> Self {
        Self {
            nodes,
            root,
            labels,
            stable_ptrs: vec![None],
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
            let current = self.resolve(root)?;
            let Some(next) = self.step(current)? else {
                self.root = current;
                return Ok((current, steps));
            };
            self.nodes[current.0] = Node::Indir(Some(next));
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
            "I" | "ord" | "chr" if !args.is_empty() => Some((1, args[0])),
            "K" if args.len() >= 2 => Some((2, args[0])),
            "A" if args.len() >= 2 => Some((2, args[1])),
            "U" if args.len() >= 2 => {
                let n = self.app(args[1], args[0]);
                Some((2, n))
            }
            "IO.performIO" if !args.is_empty() => {
                let world = self.world();
                let k = self.prim("K");
                let action = self.app(args[0], world);
                let n = self.app(action, k);
                Some((1, n))
            }
            "IO.atomic" if args.len() >= 2 => {
                let k = self.prim("K");
                let action = self.app(args[0], args[1]);
                let result = self.app(action, k);
                let pair = self.prim("P");
                let result_pair = self.app(pair, result);
                let n = self.app(result_pair, args[1]);
                Some((2, n))
            }
            "IO.>>=" if args.len() >= 3 => {
                let action = self.app(args[0], args[2]);
                let n = self.app(action, args[1]);
                Some((3, n))
            }
            "IO.>>" if args.len() >= 2 => {
                let bind = self.prim("IO.>>=");
                let bind_action = self.app(bind, args[0]);
                let k = self.prim("K");
                let then = self.app(k, args[1]);
                let n = self.app(bind_action, then);
                Some((2, n))
            }
            "IO.return" if args.len() >= 3 => {
                let kx = self.app(args[2], args[0]);
                let n = self.app(kx, args[1]);
                Some((3, n))
            }
            "IO.lazyBind" if args.len() >= 3 => {
                let world_result = self.app(args[0], args[2]);
                let fst = self.fst();
                let snd = self.snd();
                let result = self.app(fst, world_result);
                let world = self.app(snd, world_result);
                let next = self.app(args[1], result);
                let n = self.app(next, world);
                Some((3, n))
            }
            "IO.strict" if args.len() >= 2 => {
                self.reduce_node_whnf(args[1], 10_000)?;
                let n = self.app(args[0], args[1]);
                Some((2, n))
            }
            "catch" if args.len() >= 3 => {
                let action = self.app(args[0], args[2]);
                Some((3, self.catch_result(action, args[1], args[2])?))
            }
            "catchr" if args.len() >= 3 => Some((3, self.catch_result(args[0], args[1], args[2])?)),
            "raise" if !args.is_empty() => return Err(EvalError::Raised(args[0])),
            "rnf" if args.len() >= 2 => {
                let noerr = self.eval_int(args[0])? != 0;
                self.rnf(noerr, args[1])?;
                Some((2, self.prim("I")))
            }
            "seq" if args.len() >= 2 => {
                self.reduce_node_whnf(args[0], 10_000)?;
                Some((2, args[1]))
            }
            "isint" if !args.is_empty() => {
                let root = self.reduce_node_whnf(args[0], 10_000)?;
                let n = match self.nodes[self.resolve(root)?.0] {
                    Node::Int(n) => n,
                    _ => -1,
                };
                Some((1, self.push_node(Node::Int(n))))
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
            name if args.len() >= 2 => self
                .array_op(name, &args)?
                .or(self.stable_ptr_op(name, &args)?)
                .or(self.bytes_op(name, &args)?)
                .or(self.float64_binop(name, &args)?)
                .or(self.float32_binop(name, &args)?)
                .or(self.int64_binop(name, &args)?)
                .or(self.int_binop(name, &args)?)
                .or(self.array_unop(name, &args)?)
                .or(self.bytes_unop(name, &args)?)
                .or(self.float64_unop(name, &args)?)
                .or(self.float32_unop(name, &args)?)
                .or(self.float_conversion(name, &args)?)
                .or(self.int64_unop(name, &args)?)
                .or(self.int_conversion(name, &args)?)
                .or(self.int_unop(name, &args)?),
            name if !args.is_empty() => self
                .array_unop(name, &args)?
                .or(self.stable_ptr_unop(name, &args)?)
                .or(self.bytes_unop(name, &args)?)
                .or(self.float64_unop(name, &args)?)
                .or(self.float32_unop(name, &args)?)
                .or(self.float_conversion(name, &args)?)
                .or(self.int64_unop(name, &args)?)
                .or(self.int_conversion(name, &args)?)
                .or(self.int_unop(name, &args)?),
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

    fn world(&mut self) -> NodeId {
        self.push_node(Node::Int(99_999))
    }

    fn fst(&mut self) -> NodeId {
        let u = self.prim("U");
        let k = self.prim("K");
        self.app(u, k)
    }

    fn snd(&mut self) -> NodeId {
        let u = self.prim("U");
        let a = self.prim("A");
        self.app(u, a)
    }

    fn pair(&mut self, result: NodeId, world: NodeId) -> NodeId {
        let pair = self.prim("P");
        let result_pair = self.app(pair, result);
        self.app(result_pair, world)
    }

    fn catch_result(
        &mut self,
        action: NodeId,
        handler: NodeId,
        world: NodeId,
    ) -> Result<NodeId, EvalError> {
        match self.reduce_node_whnf(action, 10_000) {
            Ok(result) => Ok(result),
            Err(EvalError::Raised(exn)) => {
                let handled = self.app(handler, exn);
                Ok(self.app(handled, world))
            }
            Err(err) => Err(err),
        }
    }

    fn ordering(&mut self, ord: Ordering) -> NodeId {
        let name = match ord {
            Ordering::Less => "K2",
            Ordering::Equal => "KK",
            Ordering::Greater => "KA",
        };
        self.prim(name)
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
            IntResult::Ordering(ord) => self.ordering(ord),
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

    fn int64_binop(
        &mut self,
        name: &str,
        args: &[NodeId],
    ) -> Result<Option<(usize, NodeId)>, EvalError> {
        let Some(op) = Int64BinOp::from_prim(name) else {
            return Ok(None);
        };
        let x = self.eval_int64(args[0])?;
        let y = if op.rhs_is_shift() {
            self.eval_int(args[1])?
        } else {
            self.eval_int64(args[1])?
        };
        let node = match op.apply(x, y)? {
            Int64Result::Int64(n) => self.push_node(Node::Int64(n)),
            Int64Result::Bool(b) => self.prim(if b { "A" } else { "K" }),
            Int64Result::Ordering(ord) => self.ordering(ord),
        };
        Ok(Some((2, node)))
    }

    fn int64_unop(
        &mut self,
        name: &str,
        args: &[NodeId],
    ) -> Result<Option<(usize, NodeId)>, EvalError> {
        let Some(op) = Int64UnOp::from_prim(name) else {
            return Ok(None);
        };
        let x = self.eval_int64(args[0])?;
        let node = match op.apply(x)? {
            Int64UnResult::Int64(n) => self.push_node(Node::Int64(n)),
            Int64UnResult::Int(n) => self.push_node(Node::Int(n)),
        };
        Ok(Some((1, node)))
    }

    fn int_conversion(
        &mut self,
        name: &str,
        args: &[NodeId],
    ) -> Result<Option<(usize, NodeId)>, EvalError> {
        let node = match name {
            "itoI" | "utoU" => {
                let n = self.eval_int(args[0])?;
                self.push_node(Node::Int64(n))
            }
            "Itoi" | "Utou" => {
                let n = self.eval_int64(args[0])?;
                self.push_node(Node::Int(n))
            }
            _ => return Ok(None),
        };
        Ok(Some((1, node)))
    }

    fn float64_binop(
        &mut self,
        name: &str,
        args: &[NodeId],
    ) -> Result<Option<(usize, NodeId)>, EvalError> {
        let Some(op) = Float64BinOp::from_prim(name) else {
            return Ok(None);
        };
        let x = self.eval_float64(args[0])?;
        let y = self.eval_float64(args[1])?;
        let node = match op.apply(x, y) {
            Float64Result::Float(n) => self.push_node(Node::Float64(n)),
            Float64Result::Bool(b) => self.prim(if b { "A" } else { "K" }),
        };
        Ok(Some((2, node)))
    }

    fn float64_unop(
        &mut self,
        name: &str,
        args: &[NodeId],
    ) -> Result<Option<(usize, NodeId)>, EvalError> {
        let Some(op) = Float64UnOp::from_prim(name) else {
            return Ok(None);
        };
        let x = self.eval_float64(args[0])?;
        let node = self.push_node(Node::Float64(op.apply(x)));
        Ok(Some((1, node)))
    }

    fn float32_binop(
        &mut self,
        name: &str,
        args: &[NodeId],
    ) -> Result<Option<(usize, NodeId)>, EvalError> {
        let Some(op) = Float32BinOp::from_prim(name) else {
            return Ok(None);
        };
        let x = self.eval_float32(args[0])?;
        let y = self.eval_float32(args[1])?;
        let node = match op.apply(x, y) {
            Float32Result::Float(n) => self.push_node(Node::Float32(n)),
            Float32Result::Bool(b) => self.prim(if b { "A" } else { "K" }),
        };
        Ok(Some((2, node)))
    }

    fn float32_unop(
        &mut self,
        name: &str,
        args: &[NodeId],
    ) -> Result<Option<(usize, NodeId)>, EvalError> {
        let Some(op) = Float32UnOp::from_prim(name) else {
            return Ok(None);
        };
        let x = self.eval_float32(args[0])?;
        let node = self.push_node(Node::Float32(op.apply(x)));
        Ok(Some((1, node)))
    }

    fn float_conversion(
        &mut self,
        name: &str,
        args: &[NodeId],
    ) -> Result<Option<(usize, NodeId)>, EvalError> {
        let node = match name {
            "itod" => {
                let n = self.eval_int(args[0])?;
                self.push_node(Node::Float64(n as f64))
            }
            "utod" => {
                let n = self.eval_int(args[0])?;
                self.push_node(Node::Float64((n as u64) as f64))
            }
            "Itod" => {
                let n = self.eval_int64(args[0])?;
                self.push_node(Node::Float64(n as f64))
            }
            "dtoi" => {
                let n = self.eval_float64(args[0])?;
                self.push_node(Node::Int(n as i64))
            }
            "itof" => {
                let n = self.eval_int(args[0])?;
                self.push_node(Node::Float32(n as f32))
            }
            "utof" => {
                let n = self.eval_int(args[0])?;
                self.push_node(Node::Float32((n as u64) as f32))
            }
            "Itof" => {
                let n = self.eval_int64(args[0])?;
                self.push_node(Node::Float32(n as f32))
            }
            "ftoi" => {
                let n = self.eval_float32(args[0])?;
                self.push_node(Node::Int(n as i64))
            }
            "dtof" => {
                let n = self.eval_float64(args[0])?;
                self.push_node(Node::Float32(n as f32))
            }
            "ftod" => {
                let n = self.eval_float32(args[0])?;
                self.push_node(Node::Float64(n as f64))
            }
            "toDbl" => {
                let n = self.eval_int64(args[0])?;
                self.push_node(Node::Float64(f64::from_bits(n as u64)))
            }
            "fromDbl" => {
                let n = self.eval_float64(args[0])?;
                self.push_node(Node::Int64(n.to_bits() as i64))
            }
            "toFlt" => {
                let n = self.eval_int(args[0])?;
                self.push_node(Node::Float32(f32::from_bits(n as u32)))
            }
            "fromFlt" => {
                let n = self.eval_float32(args[0])?;
                self.push_node(Node::Int((n.to_bits() as i32) as i64))
            }
            _ => return Ok(None),
        };
        Ok(Some((1, node)))
    }

    fn array_op(
        &mut self,
        name: &str,
        args: &[NodeId],
    ) -> Result<Option<(usize, NodeId)>, EvalError> {
        let rewrite = match name {
            "A.alloc" if args.len() >= 3 => {
                let len = int_to_usize(self.eval_int(args[0])?)?;
                let array = self.push_node(Node::Array(vec![args[1]; len]));
                Some((3, self.pair(array, args[2])))
            }
            "A.alloc" => {
                let len = int_to_usize(self.eval_int(args[0])?)?;
                Some((2, self.push_node(Node::Array(vec![args[1]; len]))))
            }
            "A.read" if args.len() >= 3 => {
                let array = self.eval_array_id(args[0])?;
                let index = int_to_usize(self.eval_int(args[1])?)?;
                let item = *self
                    .array(array)?
                    .get(index)
                    .ok_or(EvalError::InvalidArray)?;
                Some((3, self.pair(item, args[2])))
            }
            "A.read" if args.len() >= 2 => {
                let array = self.eval_array_id(args[0])?;
                let index = int_to_usize(self.eval_int(args[1])?)?;
                let item = *self
                    .array(array)?
                    .get(index)
                    .ok_or(EvalError::InvalidArray)?;
                Some((2, item))
            }
            "A.write" if args.len() >= 4 => {
                let array = self.eval_array_id(args[0])?;
                let index = int_to_usize(self.eval_int(args[1])?)?;
                let items = self.array_mut(array)?;
                let slot = items.get_mut(index).ok_or(EvalError::InvalidArray)?;
                *slot = args[2];
                let unit = self.prim("I");
                Some((4, self.pair(unit, args[3])))
            }
            "A.write" if args.len() >= 3 => {
                let array = self.eval_array_id(args[0])?;
                let index = int_to_usize(self.eval_int(args[1])?)?;
                let items = self.array_mut(array)?;
                let slot = items.get_mut(index).ok_or(EvalError::InvalidArray)?;
                *slot = args[2];
                Some((3, self.prim("I")))
            }
            "A.trunc" if args.len() >= 3 => {
                let array = self.eval_array_id(args[0])?;
                let len = int_to_usize(self.eval_int(args[1])?)?;
                let items = self.array_mut(array)?;
                if len >= items.len() {
                    return Err(EvalError::InvalidArray);
                }
                items.truncate(len);
                let unit = self.prim("I");
                Some((3, self.pair(unit, args[2])))
            }
            "A.trunc" if args.len() >= 2 => {
                let array = self.eval_array_id(args[0])?;
                let len = int_to_usize(self.eval_int(args[1])?)?;
                let items = self.array_mut(array)?;
                if len >= items.len() {
                    return Err(EvalError::InvalidArray);
                }
                items.truncate(len);
                Some((2, self.prim("I")))
            }
            "A.==" => {
                let x = self.eval_array_id(args[0])?;
                let y = self.eval_array_id(args[1])?;
                Some((2, self.prim(if x == y { "A" } else { "K" })))
            }
            _ => None,
        };
        Ok(rewrite)
    }

    fn array_unop(
        &mut self,
        name: &str,
        args: &[NodeId],
    ) -> Result<Option<(usize, NodeId)>, EvalError> {
        let node = match name {
            "A.copy" if args.len() >= 2 => {
                let array = self.eval_array_id(args[0])?;
                let items = self.array(array)?.to_vec();
                let copy = self.push_node(Node::Array(items));
                self.pair(copy, args[1])
            }
            "A.copy" => {
                let array = self.eval_array_id(args[0])?;
                let items = self.array(array)?.to_vec();
                self.push_node(Node::Array(items))
            }
            "A.size" if args.len() >= 2 => {
                let array = self.eval_array_id(args[0])?;
                let len =
                    i64::try_from(self.array(array)?.len()).map_err(|_| EvalError::Overflow)?;
                let size = self.push_node(Node::Int(len));
                self.pair(size, args[1])
            }
            "A.size" => {
                let array = self.eval_array_id(args[0])?;
                let len =
                    i64::try_from(self.array(array)?.len()).map_err(|_| EvalError::Overflow)?;
                self.push_node(Node::Int(len))
            }
            _ => return Ok(None),
        };
        let used = if matches!(name, "A.copy" | "A.size") && args.len() >= 2 {
            2
        } else {
            1
        };
        Ok(Some((used, node)))
    }

    fn stable_ptr_op(
        &mut self,
        name: &str,
        args: &[NodeId],
    ) -> Result<Option<(usize, NodeId)>, EvalError> {
        let rewrite = match name {
            "SPnew" if args.len() >= 2 => {
                let handle = self.new_stable_ptr(args[0])?;
                Some((2, self.pair(handle, args[1])))
            }
            "SPderef" if args.len() >= 2 => {
                let handle = self.stable_ptr_handle(args[0])?;
                let value = self.deref_stable_ptr(handle)?;
                Some((2, self.pair(value, args[1])))
            }
            "SPfree" if args.len() >= 2 => {
                let handle = self.stable_ptr_handle(args[0])?;
                self.free_stable_ptr(handle)?;
                let unit = self.prim("I");
                Some((2, self.pair(unit, args[1])))
            }
            _ => None,
        };
        Ok(rewrite)
    }

    fn stable_ptr_unop(
        &mut self,
        name: &str,
        args: &[NodeId],
    ) -> Result<Option<(usize, NodeId)>, EvalError> {
        let node = match name {
            "SPnew" => self.new_stable_ptr(args[0])?,
            "SPderef" => {
                let handle = self.stable_ptr_handle(args[0])?;
                self.deref_stable_ptr(handle)?
            }
            "SPfree" => {
                let handle = self.stable_ptr_handle(args[0])?;
                self.free_stable_ptr(handle)?;
                self.prim("I")
            }
            _ => return Ok(None),
        };
        Ok(Some((1, node)))
    }

    fn bytes_op(
        &mut self,
        name: &str,
        args: &[NodeId],
    ) -> Result<Option<(usize, NodeId)>, EvalError> {
        let rewrite = match name {
            "bsnew" if args.len() >= 3 => {
                let size = int_to_usize(self.eval_int(args[0])?)?;
                let capacity = int_to_usize(self.eval_int(args[1])?)?;
                let bytes = self.new_mutable_bytes(size, capacity)?;
                Some((3, self.pair(bytes, args[2])))
            }
            "bsnew" => {
                let size = int_to_usize(self.eval_int(args[0])?)?;
                let capacity = int_to_usize(self.eval_int(args[1])?)?;
                Some((2, self.new_mutable_bytes(size, capacity)?))
            }
            "bsread" if args.len() >= 3 => {
                let bytes = self.eval_bytes_id(args[0])?;
                let index = int_to_usize(self.eval_int(args[1])?)?;
                let byte = self
                    .bytes(bytes)?
                    .get(index)
                    .copied()
                    .ok_or(EvalError::InvalidByteString)?;
                let byte = self.push_node(Node::Int(byte as i64));
                Some((3, self.pair(byte, args[2])))
            }
            "bsread" => {
                let bytes = self.eval_bytes_id(args[0])?;
                let index = int_to_usize(self.eval_int(args[1])?)?;
                let byte = self
                    .bytes(bytes)?
                    .get(index)
                    .copied()
                    .ok_or(EvalError::InvalidByteString)?;
                Some((2, self.push_node(Node::Int(byte as i64))))
            }
            "bswrite" if args.len() >= 4 => {
                let bytes = self.eval_bytes_id(args[0])?;
                let index = int_to_usize(self.eval_int(args[1])?)?;
                let byte = self.eval_int(args[2])? as u8;
                let slot = self
                    .bytes_mut(bytes)?
                    .get_mut(index)
                    .ok_or(EvalError::InvalidByteString)?;
                *slot = byte;
                let unit = self.prim("I");
                Some((4, self.pair(unit, args[3])))
            }
            "bswrite" if args.len() >= 3 => {
                let bytes = self.eval_bytes_id(args[0])?;
                let index = int_to_usize(self.eval_int(args[1])?)?;
                let byte = self.eval_int(args[2])? as u8;
                let slot = self
                    .bytes_mut(bytes)?
                    .get_mut(index)
                    .ok_or(EvalError::InvalidByteString)?;
                *slot = byte;
                Some((3, self.prim("I")))
            }
            "bsfreeze" if args.len() >= 2 => {
                let bytes = self.eval_bytes_id(args[0])?;
                let bytes = self.freeze_bytes(bytes)?;
                Some((2, self.pair(bytes, args[1])))
            }
            "bsappbyte" if args.len() >= 3 => {
                let bytes = self.eval_bytes_id(args[0])?;
                let byte = self.eval_int(args[1])? as u8;
                self.append_byte(bytes, byte)?;
                let unit = self.prim("I");
                Some((3, self.pair(unit, args[2])))
            }
            "bsappbyte" => {
                let bytes = self.eval_bytes_id(args[0])?;
                let byte = self.eval_int(args[1])? as u8;
                self.append_byte(bytes, byte)?;
                Some((2, self.prim("I")))
            }
            "bsappchar" if args.len() >= 3 => {
                let bytes = self.eval_bytes_id(args[0])?;
                let encoded = modified_utf8(self.eval_int(args[1])?)?;
                self.append_bytes(bytes, &encoded)?;
                let unit = self.prim("I");
                Some((3, self.pair(unit, args[2])))
            }
            "bsappchar" => {
                let bytes = self.eval_bytes_id(args[0])?;
                let encoded = modified_utf8(self.eval_int(args[1])?)?;
                self.append_bytes(bytes, &encoded)?;
                Some((2, self.prim("I")))
            }
            "bs++" => {
                let mut bytes = self.eval_bytes(args[0])?;
                bytes.extend(self.eval_bytes(args[1])?);
                Some((2, self.push_node(Node::Bytes(bytes))))
            }
            "bs++." => {
                let mut bytes = self.eval_bytes(args[0])?;
                bytes.push(b'.');
                bytes.extend(self.eval_bytes(args[1])?);
                Some((2, self.push_node(Node::Bytes(bytes))))
            }
            "bs==" | "bs/=" | "bs<" | "bs<=" | "bs>" | "bs>=" | "bscmp" => {
                let cmp = self.eval_bytes(args[0])?.cmp(&self.eval_bytes(args[1])?);
                let node = match name {
                    "bs==" => self.prim(if cmp == Ordering::Equal { "A" } else { "K" }),
                    "bs/=" => self.prim(if cmp != Ordering::Equal { "A" } else { "K" }),
                    "bs<" => self.prim(if cmp == Ordering::Less { "A" } else { "K" }),
                    "bs<=" => self.prim(if cmp != Ordering::Greater { "A" } else { "K" }),
                    "bs>" => self.prim(if cmp == Ordering::Greater { "A" } else { "K" }),
                    "bs>=" => self.prim(if cmp != Ordering::Less { "A" } else { "K" }),
                    "bscmp" => self.ordering(cmp),
                    _ => unreachable!(),
                };
                Some((2, node))
            }
            "bsreplicate" => {
                let len = int_to_usize(self.eval_int(args[0])?)?;
                let byte = self.eval_int(args[1])? as u8;
                Some((2, self.push_node(Node::Bytes(vec![byte; len]))))
            }
            "bsindex" => {
                let bytes = self.eval_bytes(args[0])?;
                let index = int_to_usize(self.eval_int(args[1])?)?;
                let byte = bytes
                    .get(index)
                    .copied()
                    .ok_or(EvalError::InvalidByteString)?;
                Some((2, self.push_node(Node::Int(byte as i64))))
            }
            "bssubstr" if args.len() >= 3 => {
                let bytes = self.eval_bytes(args[0])?;
                let offset = int_to_usize(self.eval_int(args[1])?)?;
                let len = int_to_usize(self.eval_int(args[2])?)?;
                let end = offset
                    .checked_add(len)
                    .filter(|end| *end <= bytes.len())
                    .ok_or(EvalError::InvalidByteString)?;
                Some((3, self.push_node(Node::Bytes(bytes[offset..end].to_vec()))))
            }
            _ => None,
        };
        Ok(rewrite)
    }

    fn bytes_unop(
        &mut self,
        name: &str,
        args: &[NodeId],
    ) -> Result<Option<(usize, NodeId)>, EvalError> {
        let node = match name {
            "bslength" => {
                let len = i64::try_from(self.eval_bytes(args[0])?.len())
                    .map_err(|_| EvalError::Overflow)?;
                self.push_node(Node::Int(len))
            }
            "headUTF8" => {
                let (codepoint, _) = head_utf8(&self.eval_bytes(args[0])?)?;
                self.push_node(Node::Int(codepoint as i64))
            }
            "tailUTF8" => {
                let bytes = self.eval_bytes(args[0])?;
                let (_, offset) = head_utf8(&bytes)?;
                self.push_node(Node::Bytes(bytes[offset..].to_vec()))
            }
            "bsunpack" => {
                let bytes = self.eval_bytes(args[0])?;
                let values = bytes.into_iter().map(i64::from);
                self.int_list(values)
            }
            "fromUTF8" => {
                let bytes = self.eval_bytes(args[0])?;
                let values = decode_utf8_bytes(&bytes)?;
                self.int_list(values.into_iter().map(i64::from))
            }
            "bsfreeze" => {
                let bytes = self.eval_bytes_id(args[0])?;
                self.freeze_bytes(bytes)?
            }
            _ => return Ok(None),
        };
        Ok(Some((1, node)))
    }

    fn eval_int(&mut self, id: NodeId) -> Result<i64, EvalError> {
        let root = self.reduce_node_whnf(id, 10_000)?;
        match self.nodes[self.resolve(root)?.0] {
            Node::Int(n) => Ok(n),
            _ => Err(EvalError::ExpectedInt(root)),
        }
    }

    fn eval_int64(&mut self, id: NodeId) -> Result<i64, EvalError> {
        let root = self.reduce_node_whnf(id, 10_000)?;
        match self.nodes[self.resolve(root)?.0] {
            Node::Int64(n) => Ok(n),
            _ => Err(EvalError::ExpectedInt64(root)),
        }
    }

    fn eval_float64(&mut self, id: NodeId) -> Result<f64, EvalError> {
        let root = self.reduce_node_whnf(id, 10_000)?;
        match self.nodes[self.resolve(root)?.0] {
            Node::Float64(n) => Ok(n),
            _ => Err(EvalError::ExpectedFloat64(root)),
        }
    }

    fn eval_float32(&mut self, id: NodeId) -> Result<f32, EvalError> {
        let root = self.reduce_node_whnf(id, 10_000)?;
        match self.nodes[self.resolve(root)?.0] {
            Node::Float32(n) => Ok(n),
            _ => Err(EvalError::ExpectedFloat32(root)),
        }
    }

    fn eval_bytes(&mut self, id: NodeId) -> Result<Vec<u8>, EvalError> {
        let id = self.eval_bytes_id(id)?;
        Ok(self.bytes(id)?.to_vec())
    }

    fn eval_bytes_id(&mut self, id: NodeId) -> Result<NodeId, EvalError> {
        let root = self.reduce_node_whnf(id, 10_000)?;
        let id = self.resolve(root)?;
        match self.nodes[id.0] {
            Node::Bytes(_) | Node::MutableBytes { .. } => Ok(id),
            _ => Err(EvalError::ExpectedBytes(root)),
        }
    }

    fn eval_array_id(&mut self, id: NodeId) -> Result<NodeId, EvalError> {
        let root = self.reduce_node_whnf(id, 10_000)?;
        let id = self.resolve(root)?;
        match self.nodes[id.0] {
            Node::Array(_) => Ok(id),
            _ => Err(EvalError::ExpectedArray(root)),
        }
    }

    fn array(&self, id: NodeId) -> Result<&[NodeId], EvalError> {
        match &self.nodes[id.0] {
            Node::Array(items) => Ok(items),
            _ => Err(EvalError::ExpectedArray(id)),
        }
    }

    fn array_mut(&mut self, id: NodeId) -> Result<&mut Vec<NodeId>, EvalError> {
        match &mut self.nodes[id.0] {
            Node::Array(items) => Ok(items),
            _ => Err(EvalError::ExpectedArray(id)),
        }
    }

    fn bytes(&self, id: NodeId) -> Result<&[u8], EvalError> {
        match &self.nodes[id.0] {
            Node::Bytes(bytes) | Node::MutableBytes { bytes, .. } => Ok(bytes),
            _ => Err(EvalError::ExpectedBytes(id)),
        }
    }

    fn bytes_mut(&mut self, id: NodeId) -> Result<&mut Vec<u8>, EvalError> {
        match &mut self.nodes[id.0] {
            Node::Bytes(bytes) | Node::MutableBytes { bytes, .. } => Ok(bytes),
            _ => Err(EvalError::ExpectedBytes(id)),
        }
    }

    fn new_mutable_bytes(&mut self, size: usize, capacity: usize) -> Result<NodeId, EvalError> {
        if size > capacity {
            return Err(EvalError::InvalidByteString);
        }
        let mut bytes = Vec::with_capacity(capacity);
        bytes.resize(size, 0);
        Ok(self.push_node(Node::MutableBytes { bytes, capacity }))
    }

    fn freeze_bytes(&mut self, id: NodeId) -> Result<NodeId, EvalError> {
        let frozen = match &mut self.nodes[id.0] {
            Node::Bytes(_) => return Ok(id),
            Node::MutableBytes { bytes, .. } => std::mem::take(bytes),
            _ => return Err(EvalError::ExpectedBytes(id)),
        };
        self.nodes[id.0] = Node::Bytes(frozen);
        Ok(id)
    }

    fn append_byte(&mut self, id: NodeId, byte: u8) -> Result<(), EvalError> {
        match &mut self.nodes[id.0] {
            Node::Bytes(bytes) => {
                bytes.push(byte);
                Ok(())
            }
            Node::MutableBytes { bytes, capacity } => {
                if bytes.len() >= *capacity {
                    *capacity = (*capacity)
                        .checked_add(*capacity / 2)
                        .and_then(|capacity| capacity.checked_add(2))
                        .ok_or(EvalError::Overflow)?;
                    if *capacity < bytes.len() {
                        return Err(EvalError::Overflow);
                    }
                    if *capacity > bytes.capacity() {
                        bytes.reserve(*capacity - bytes.capacity());
                    }
                }
                bytes.push(byte);
                Ok(())
            }
            _ => Err(EvalError::ExpectedBytes(id)),
        }
    }

    fn append_bytes(&mut self, id: NodeId, bytes: &[u8]) -> Result<(), EvalError> {
        for &byte in bytes {
            self.append_byte(id, byte)?;
        }
        Ok(())
    }

    fn new_stable_ptr(&mut self, value: NodeId) -> Result<NodeId, EvalError> {
        let slot = self
            .stable_ptrs
            .iter()
            .enumerate()
            .skip(1)
            .find_map(|(slot, value)| value.is_none().then_some(slot));
        let slot = match slot {
            Some(slot) => {
                self.stable_ptrs[slot] = Some(value);
                slot
            }
            None => {
                self.stable_ptrs.push(Some(value));
                self.stable_ptrs.len() - 1
            }
        };
        let handle = i64::try_from(slot).map_err(|_| EvalError::Overflow)?;
        Ok(self.push_node(Node::Int(handle)))
    }

    fn stable_ptr_handle(&mut self, id: NodeId) -> Result<usize, EvalError> {
        usize::try_from(self.eval_int(id)?).map_err(|_| EvalError::InvalidStablePtr)
    }

    fn deref_stable_ptr(&self, handle: usize) -> Result<NodeId, EvalError> {
        self.stable_ptrs
            .get(handle)
            .and_then(|value| *value)
            .ok_or(EvalError::InvalidStablePtr)
    }

    fn free_stable_ptr(&mut self, handle: usize) -> Result<(), EvalError> {
        let slot = self
            .stable_ptrs
            .get_mut(handle)
            .ok_or(EvalError::InvalidStablePtr)?;
        if slot.is_none() {
            return Err(EvalError::InvalidStablePtr);
        }
        *slot = None;
        Ok(())
    }

    fn int_list(&mut self, values: impl IntoIterator<Item = i64>) -> NodeId {
        let values: Vec<_> = values.into_iter().collect();
        let mut list = self.prim("K");
        for value in values.into_iter().rev() {
            let cons = self.prim("O");
            let value = self.push_node(Node::Int(value));
            let head = self.app(cons, value);
            list = self.app(head, list);
        }
        list
    }

    fn reduce_node_whnf(&mut self, mut root: NodeId, limit: usize) -> Result<NodeId, EvalError> {
        for _ in 0..limit {
            let current = self.resolve(root)?;
            let Some(next) = self.step(current)? else {
                return Ok(current);
            };
            self.nodes[current.0] = Node::Indir(Some(next));
            root = next;
        }
        Err(EvalError::StepLimit { limit })
    }

    fn rnf(&mut self, noerr: bool, root: NodeId) -> Result<(), EvalError> {
        let mut seen = HashSet::new();
        self.rnf_rec(noerr, root, &mut seen)
    }

    fn rnf_rec(
        &mut self,
        noerr: bool,
        root: NodeId,
        seen: &mut HashSet<NodeId>,
    ) -> Result<(), EvalError> {
        let root = self.resolve(root)?;
        if !seen.insert(root) {
            return Ok(());
        }
        let root = match self.reduce_node_whnf(root, 10_000) {
            Ok(root) => self.resolve(root)?,
            Err(EvalError::Raised(_)) if noerr => return Ok(()),
            Err(err) => return Err(err),
        };
        if let Node::App(fun, arg) = self.nodes[root.0] {
            self.rnf_rec(noerr, fun, seen)?;
            self.rnf_rec(noerr, arg, seen)?;
        }
        Ok(())
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
            Node::Bytes(bytes) | Node::MutableBytes { bytes, .. } => render_bytes(bytes, out),
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
    Ordering(Ordering),
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
    ICmp,
    UCmp,
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
            "icmp" => Self::ICmp,
            "ucmp" => Self::UCmp,
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
            Self::ICmp => return Ok(IntResult::Ordering(x.cmp(&y))),
            Self::UCmp => return Ok(IntResult::Ordering(xu.cmp(&yu))),
        };
        Ok(IntResult::Int(n))
    }
}

enum Int64Result {
    Int64(i64),
    Bool(bool),
    Ordering(Ordering),
}

#[derive(Clone, Copy)]
enum Int64BinOp {
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
    ICmp,
    UCmp,
}

impl Int64BinOp {
    fn from_prim(name: &str) -> Option<Self> {
        Some(match name {
            "I+" => Self::Add,
            "I-" => Self::Sub,
            "I*" => Self::Mul,
            "Iquot" => Self::Quot,
            "Irem" => Self::Rem,
            "Isubtract" => Self::SubR,
            "Iu+" => Self::UAdd,
            "Iu-" => Self::USub,
            "Iu*" => Self::UMul,
            "Iuquot" => Self::UQuot,
            "Iurem" => Self::URem,
            "Iusubtract" => Self::USubR,
            "Iand" => Self::And,
            "Ior" => Self::Or,
            "Ixor" => Self::Xor,
            "Ishl" => Self::Shl,
            "Ishr" => Self::Shr,
            "Iashr" => Self::Ashr,
            "I==" => Self::Eq,
            "I/=" => Self::Ne,
            "I<" => Self::Lt,
            "I<=" => Self::Le,
            "I>" => Self::Gt,
            "I>=" => Self::Ge,
            "Iu<" => Self::Ult,
            "Iu<=" => Self::Ule,
            "Iu>" => Self::Ugt,
            "Iu>=" => Self::Uge,
            "Iicmp" => Self::ICmp,
            "Iucmp" => Self::UCmp,
            _ => return None,
        })
    }

    fn rhs_is_shift(self) -> bool {
        matches!(self, Self::Shl | Self::Shr | Self::Ashr)
    }

    fn apply(self, x: i64, y: i64) -> Result<Int64Result, EvalError> {
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
            Self::Eq => return Ok(Int64Result::Bool(xu == yu)),
            Self::Ne => return Ok(Int64Result::Bool(xu != yu)),
            Self::Lt => return Ok(Int64Result::Bool(x < y)),
            Self::Le => return Ok(Int64Result::Bool(x <= y)),
            Self::Gt => return Ok(Int64Result::Bool(x > y)),
            Self::Ge => return Ok(Int64Result::Bool(x >= y)),
            Self::Ult => return Ok(Int64Result::Bool(xu < yu)),
            Self::Ule => return Ok(Int64Result::Bool(xu <= yu)),
            Self::Ugt => return Ok(Int64Result::Bool(xu > yu)),
            Self::Uge => return Ok(Int64Result::Bool(xu >= yu)),
            Self::ICmp => return Ok(Int64Result::Ordering(x.cmp(&y))),
            Self::UCmp => return Ok(Int64Result::Ordering(xu.cmp(&yu))),
        };
        Ok(Int64Result::Int64(n))
    }
}

enum Int64UnResult {
    Int64(i64),
    Int(i64),
}

#[derive(Clone, Copy)]
enum Int64UnOp {
    Neg,
    UNeg,
    Inv,
    PopCount,
    Clz,
    Ctz,
}

impl Int64UnOp {
    fn from_prim(name: &str) -> Option<Self> {
        Some(match name {
            "Ineg" => Self::Neg,
            "Iuneg" => Self::UNeg,
            "Iinv" => Self::Inv,
            "Ipopcount" => Self::PopCount,
            "Iclz" => Self::Clz,
            "Ictz" => Self::Ctz,
            _ => return None,
        })
    }

    fn apply(self, x: i64) -> Result<Int64UnResult, EvalError> {
        let xu = x as u64;
        Ok(match self {
            Self::Neg => Int64UnResult::Int64(x.checked_neg().ok_or(EvalError::Overflow)?),
            Self::UNeg => Int64UnResult::Int64((0u64.wrapping_sub(xu)) as i64),
            Self::Inv => Int64UnResult::Int64(!xu as i64),
            Self::PopCount => Int64UnResult::Int(xu.count_ones() as i64),
            Self::Clz => Int64UnResult::Int(xu.leading_zeros() as i64),
            Self::Ctz => Int64UnResult::Int(xu.trailing_zeros() as i64),
        })
    }
}

enum Float64Result {
    Float(f64),
    Bool(bool),
}

#[derive(Clone, Copy)]
enum Float64BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

impl Float64BinOp {
    fn from_prim(name: &str) -> Option<Self> {
        Some(match name {
            "d+" => Self::Add,
            "d-" => Self::Sub,
            "d*" => Self::Mul,
            "d/" => Self::Div,
            "d==" => Self::Eq,
            "d/=" => Self::Ne,
            "d<" => Self::Lt,
            "d<=" => Self::Le,
            "d>" => Self::Gt,
            "d>=" => Self::Ge,
            _ => return None,
        })
    }

    fn apply(self, x: f64, y: f64) -> Float64Result {
        match self {
            Self::Add => Float64Result::Float(x + y),
            Self::Sub => Float64Result::Float(x - y),
            Self::Mul => Float64Result::Float(x * y),
            Self::Div => Float64Result::Float(x / y),
            Self::Eq => Float64Result::Bool(x == y),
            Self::Ne => Float64Result::Bool(x != y),
            Self::Lt => Float64Result::Bool(x < y),
            Self::Le => Float64Result::Bool(x <= y),
            Self::Gt => Float64Result::Bool(x > y),
            Self::Ge => Float64Result::Bool(x >= y),
        }
    }
}

#[derive(Clone, Copy)]
enum Float64UnOp {
    Neg,
}

impl Float64UnOp {
    fn from_prim(name: &str) -> Option<Self> {
        Some(match name {
            "dneg" => Self::Neg,
            _ => return None,
        })
    }

    fn apply(self, x: f64) -> f64 {
        match self {
            Self::Neg => -x,
        }
    }
}

enum Float32Result {
    Float(f32),
    Bool(bool),
}

#[derive(Clone, Copy)]
enum Float32BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

impl Float32BinOp {
    fn from_prim(name: &str) -> Option<Self> {
        Some(match name {
            "f+" => Self::Add,
            "f-" => Self::Sub,
            "f*" => Self::Mul,
            "f/" => Self::Div,
            "f==" => Self::Eq,
            "f/=" => Self::Ne,
            "f<" => Self::Lt,
            "f<=" => Self::Le,
            "f>" => Self::Gt,
            "f>=" => Self::Ge,
            _ => return None,
        })
    }

    fn apply(self, x: f32, y: f32) -> Float32Result {
        match self {
            Self::Add => Float32Result::Float(x + y),
            Self::Sub => Float32Result::Float(x - y),
            Self::Mul => Float32Result::Float(x * y),
            Self::Div => Float32Result::Float(x / y),
            Self::Eq => Float32Result::Bool(x == y),
            Self::Ne => Float32Result::Bool(x != y),
            Self::Lt => Float32Result::Bool(x < y),
            Self::Le => Float32Result::Bool(x <= y),
            Self::Gt => Float32Result::Bool(x > y),
            Self::Ge => Float32Result::Bool(x >= y),
        }
    }
}

#[derive(Clone, Copy)]
enum Float32UnOp {
    Neg,
}

impl Float32UnOp {
    fn from_prim(name: &str) -> Option<Self> {
        Some(match name {
            "fneg" => Self::Neg,
            _ => return None,
        })
    }

    fn apply(self, x: f32) -> f32 {
        match self {
            Self::Neg => -x,
        }
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

fn int_to_usize(n: i64) -> Result<usize, EvalError> {
    usize::try_from(n).map_err(|_| EvalError::InvalidByteString)
}

fn head_utf8(bytes: &[u8]) -> Result<(u32, usize), EvalError> {
    let c1 = *bytes.first().ok_or(EvalError::InvalidByteString)?;
    if c1 & 0x80 == 0 {
        return Ok((c1 as u32, 1));
    }

    let c2 = *bytes.get(1).ok_or(EvalError::InvalidByteString)?;
    if c1 & 0xe0 == 0xc0 {
        return Ok(((((c1 & 0x1f) as u32) << 6) | ((c2 & 0x3f) as u32), 2));
    }

    let c3 = *bytes.get(2).ok_or(EvalError::InvalidByteString)?;
    if c1 & 0xf0 == 0xe0 {
        return Ok((
            (((c1 & 0x0f) as u32) << 12) | (((c2 & 0x3f) as u32) << 6) | ((c3 & 0x3f) as u32),
            3,
        ));
    }

    let c4 = *bytes.get(3).ok_or(EvalError::InvalidByteString)?;
    if c1 & 0xf8 == 0xf0 {
        return Ok((
            (((c1 & 0x07) as u32) << 18)
                | (((c2 & 0x3f) as u32) << 12)
                | (((c3 & 0x3f) as u32) << 6)
                | ((c4 & 0x3f) as u32),
            4,
        ));
    }

    Err(EvalError::InvalidByteString)
}

fn decode_utf8_bytes(mut bytes: &[u8]) -> Result<Vec<u32>, EvalError> {
    let mut values = Vec::new();
    while !bytes.is_empty() {
        let (value, offset) = head_utf8(bytes)?;
        values.push(value);
        bytes = &bytes[offset..];
    }
    Ok(values)
}

fn modified_utf8(n: i64) -> Result<Vec<u8>, EvalError> {
    let mut c = u32::try_from(n).map_err(|_| EvalError::InvalidByteString)?;
    if c & 0x1ff800 == 0xd800 {
        c = 0xfffd;
    }
    if c > 0 && c < 0x80 {
        Ok(vec![c as u8])
    } else if c < 0x800 {
        Ok(vec![0xc0 | (c >> 6) as u8, 0x80 | (c & 0x3f) as u8])
    } else if c < 0x10000 {
        Ok(vec![
            0xe0 | (c >> 12) as u8,
            0x80 | ((c >> 6) & 0x3f) as u8,
            0x80 | (c & 0x3f) as u8,
        ])
    } else if c < 0x110000 {
        Ok(vec![
            0xf0 | (c >> 18) as u8,
            0x80 | ((c >> 12) & 0x3f) as u8,
            0x80 | ((c >> 6) & 0x3f) as u8,
            0x80 | (c & 0x3f) as u8,
        ])
    } else {
        Err(EvalError::InvalidByteString)
    }
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
    use crate::{EvalError, parse_program};

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
        assert_eq!(whnf(b"v8.4\n0\nicmp #1 @ #2 @ }"), "K2");
        assert_eq!(whnf(b"v8.4\n0\nucmp #-1 @ #1 @ }"), "KA");
    }

    #[test]
    fn reduces_int64_primitives() {
        assert_eq!(whnf(b"v8.4\n0\nI+ ##40 @ ##2 @ }"), "42i64");
        assert_eq!(whnf(b"v8.4\n0\nIsubtract ##10 @ ##3 @ }"), "-7i64");
        assert_eq!(whnf(b"v8.4\n0\nIquot ##22 @ ##5 @ }"), "4i64");
        assert_eq!(whnf(b"v8.4\n0\nIand ##6 @ ##3 @ }"), "2i64");
        assert_eq!(whnf(b"v8.4\n0\nIshl ##3 @ #2 @ }"), "12i64");
        assert_eq!(whnf(b"v8.4\n0\nIpopcount ##7 @ }"), "3");
        assert_eq!(whnf(b"v8.4\n0\nI== ##2 @ ##2 @ }"), "A");
        assert_eq!(whnf(b"v8.4\n0\nIu> ##-1 @ ##1 @ }"), "A");
        assert_eq!(whnf(b"v8.4\n0\nIicmp ##1 @ ##2 @ }"), "K2");
        assert_eq!(whnf(b"v8.4\n0\nIucmp ##-1 @ ##1 @ }"), "KA");
        assert_eq!(whnf(b"v8.4\n0\nitoI #7 @ }"), "7i64");
        assert_eq!(whnf(b"v8.4\n0\nItoi ##7 @ }"), "7");
    }

    #[test]
    fn reduces_float64_primitives() {
        assert_eq!(whnf(b"v8.4\n0\nd+ &1.5 @ &2.25 @ }"), "3.75");
        assert_eq!(whnf(b"v8.4\n0\nd* &3 @ &2.5 @ }"), "7.5");
        assert_eq!(whnf(b"v8.4\n0\ndneg &1.5 @ }"), "-1.5");
        assert_eq!(whnf(b"v8.4\n0\nd< &1.5 @ &2.25 @ }"), "A");
        assert_eq!(whnf(b"v8.4\n0\nd== &1.5 @ &2.25 @ }"), "K");
        assert_eq!(whnf(b"v8.4\n0\nitod #7 @ }"), "7");
        assert_eq!(whnf(b"v8.4\n0\nItod ##7 @ }"), "7");
        assert_eq!(whnf(b"v8.4\n0\ndtoi &7.75 @ }"), "7");
        assert_eq!(whnf(b"v8.4\n0\nd> utod #-1 @ @ &1000 @ }"), "A");
        assert_eq!(whnf(b"v8.4\n0\ntoDbl fromDbl &1.5 @ @ }"), "1.5");
    }

    #[test]
    fn reduces_float32_primitives() {
        assert_eq!(whnf(b"v8.4\n0\nf+ &&1.5 @ &&2.25 @ }"), "3.75f");
        assert_eq!(whnf(b"v8.4\n0\nf* &&3 @ &&2.5 @ }"), "7.5f");
        assert_eq!(whnf(b"v8.4\n0\nfneg &&1.5 @ }"), "-1.5f");
        assert_eq!(whnf(b"v8.4\n0\nf< &&1.5 @ &&2.25 @ }"), "A");
        assert_eq!(whnf(b"v8.4\n0\nf== &&1.5 @ &&2.25 @ }"), "K");
        assert_eq!(whnf(b"v8.4\n0\nitof #7 @ }"), "7f");
        assert_eq!(whnf(b"v8.4\n0\nItof ##7 @ }"), "7f");
        assert_eq!(whnf(b"v8.4\n0\nftoi &&7.75 @ }"), "7");
        assert_eq!(whnf(b"v8.4\n0\nf> utof #-1 @ @ &&1000 @ }"), "A");
        assert_eq!(whnf(b"v8.4\n0\nftod dtof &1.5 @ @ }"), "1.5");
        assert_eq!(whnf(b"v8.4\n0\ntoFlt fromFlt &&1.5 @ @ }"), "1.5f");
    }

    #[test]
    fn reduces_bytestring_primitives() {
        assert_eq!(whnf(b"v8.4\n0\nbs++ \"foo\" @ \"bar\" @ }"), "\"foobar\"");
        assert_eq!(whnf(b"v8.4\n0\nbs++. \"foo\" @ \"bar\" @ }"), "\"foo.bar\"");
        assert_eq!(whnf(b"v8.4\n0\nbs== \"x\" @ \"x\" @ }"), "A");
        assert_eq!(whnf(b"v8.4\n0\nbs< \"abc\" @ \"abd\" @ }"), "A");
        assert_eq!(whnf(b"v8.4\n0\nbscmp \"abd\" @ \"abc\" @ }"), "KA");
        assert_eq!(whnf(b"v8.4\n0\nbslength \"hello\" @ }"), "5");
        assert_eq!(whnf(b"v8.4\n0\nbsreplicate #3 @ #65 @ }"), "\"AAA\"");
        assert_eq!(whnf(b"v8.4\n0\nbsindex \"ABC\" @ #1 @ }"), "66");
        assert_eq!(
            whnf(b"v8.4\n0\nbssubstr \"abcdef\" @ #2 @ #3 @ }"),
            "\"cde\""
        );
        assert_eq!(whnf(b"v8.4\n0\nheadUTF8 $2 \xc3\xa5 @ }"), "229");
        assert_eq!(whnf(b"v8.4\n0\ntailUTF8 $3 \xc3\xa5x @ }"), "\"x\"");
        assert_eq!(whnf(b"v8.4\n0\nbsunpack \"AB\" @ #0 @ K @ }"), "65");
        assert_eq!(
            whnf(b"v8.4\n0\nbsunpack \"AB\" @ #0 @ A @ #0 @ K @ }"),
            "66"
        );
        assert_eq!(whnf(b"v8.4\n0\nfromUTF8 $3 \xc3\xa5x @ #0 @ K @ }"), "229");
        assert_eq!(
            whnf(b"v8.4\n0\nfromUTF8 $3 \xc3\xa5x @ #0 @ A @ #0 @ K @ }"),
            "120"
        );
    }

    #[test]
    fn reduces_mutable_bytestring_primitives() {
        assert_eq!(whnf(b"v8.4\n0\nbsnew #2 @ #4 @ }"), "\"\\x00\\x00\"");
        assert_eq!(
            whnf(b"v8.4\n1\nseq bswrite bsnew #2 @ #4 @ :0 @ #1 @ #65 @ @ bsread _0 @ #1 @ @ }"),
            "65"
        );
        assert_eq!(
            whnf(b"v8.4\n1\nseq bsappbyte bsnew #0 @ #0 @ :0 @ #65 @ @ bsfreeze _0 @ @ }"),
            "\"A\""
        );
        assert_eq!(
            whnf(b"v8.4\n1\nseq bsappchar bsnew #0 @ #0 @ :0 @ #229 @ @ bsfreeze _0 @ @ }"),
            "\"\\xc3\\xa5\""
        );
        assert_eq!(
            whnf(b"v8.4\n1\nseq bswrite \"abc\" :0 @ #1 @ #88 @ @ bsread _0 @ #1 @ @ }"),
            "88"
        );
    }

    #[test]
    fn reduces_strict_alias_and_probe_primitives() {
        assert_eq!(whnf(b"v8.4\n0\nord #65 @ }"), "65");
        assert_eq!(whnf(b"v8.4\n0\nchr #65 @ }"), "65");
        assert_eq!(whnf(b"v8.4\n0\nseq + #1 @ #2 @ @ #9 @ }"), "9");
        assert_eq!(whnf(b"v8.4\n0\nisint #7 @ }"), "7");
        assert_eq!(whnf(b"v8.4\n0\nisint \"x\" @ }"), "-1");
    }

    #[test]
    fn reduces_rnf_and_exception_primitives() {
        assert_eq!(whnf(b"v8.4\n0\nrnf #0 @ O #1 @ #2 @ @ }"), "I");
        assert_eq!(whnf(b"v8.4\n0\nrnf #1 @ raise #7 @ @ }"), "I");

        let mut program = parse_program(b"v8.4\n0\nraise #7 @ }").unwrap();
        assert!(matches!(
            program.reduce_whnf(100),
            Err(EvalError::Raised(_))
        ));

        assert_eq!(
            whnf(b"v8.4\n0\nIO.performIO catch IO.return #5 @ @ K IO.return #42 @ @ @ @ }"),
            "5"
        );
        assert_eq!(
            whnf(b"v8.4\n0\nIO.performIO catch raise #7 @ @ K IO.return #42 @ @ @ @ }"),
            "42"
        );
    }

    #[test]
    fn reduces_stable_pointer_primitives() {
        assert_eq!(whnf(b"v8.4\n0\nSPnew #42 @ }"), "1");
        assert_eq!(whnf(b"v8.4\n0\nSPderef SPnew #42 @ @ }"), "42");
        assert_eq!(
            whnf(b"v8.4\n2\nseq SPfree SPnew #1 @ :0 @ @ SPnew #2 @ :1 @ }"),
            "1"
        );
        assert_eq!(whnf(b"v8.4\n0\nIO.performIO SPnew #42 @ @ }"), "1");
        assert_eq!(
            whnf(b"v8.4\n0\nIO.performIO SPderef SPnew #42 @ @ @ }"),
            "42"
        );
    }

    #[test]
    fn reduces_array_primitives() {
        assert_eq!(whnf(b"v8.4\n0\nA.alloc #3 @ #7 @ }"), "[7, 7, 7]");
        assert_eq!(whnf(b"v8.4\n0\nA.size A.alloc #3 @ #7 @ @ }"), "3");
        assert_eq!(whnf(b"v8.4\n0\nA.read A.alloc #3 @ #7 @ @ #1 @ }"), "7");
        assert_eq!(whnf(b"v8.4\n1\nA.== #0 [1] :0 @ _0 @ }"), "A");
        assert_eq!(whnf(b"v8.4\n1\nA.== #0 [1] :0 @ A.copy _0 @ @ }"), "K");
        assert_eq!(
            whnf(b"v8.4\n1\nseq A.write #0 #0 #0 [3] :0 @ #1 @ #42 @ @ A.read _0 @ #1 @ @ }"),
            "42"
        );
        assert_eq!(
            whnf(b"v8.4\n1\nseq A.trunc #0 #0 #0 [3] :0 @ #1 @ @ A.size _0 @ @ }"),
            "1"
        );
        assert_eq!(whnf(b"v8.4\n1\nA.== A.alloc #1 @ #0 @ :0 @ _0 @ }"), "A");
        assert_eq!(
            whnf(b"v8.4\n1\nseq A.write A.alloc #3 @ #0 @ :0 @ #1 @ #42 @ @ A.read _0 @ #1 @ @ }"),
            "42"
        );
    }

    #[test]
    fn reduces_io_control_primitives() {
        assert_eq!(whnf(b"v8.4\n0\nIO.performIO IO.return #5 @ @ }"), "5");
        assert_eq!(
            whnf(b"v8.4\n0\nIO.performIO IO.>> IO.return #1 @ @ IO.return #7 @ @ @ }"),
            "7"
        );
        assert_eq!(
            whnf(b"v8.4\n0\nIO.performIO IO.>>= IO.return #1 @ @ K IO.return #7 @ @ @ @ }"),
            "7"
        );
        assert_eq!(
            whnf(b"v8.4\n0\nIO.performIO IO.lazyBind IO.return #3 @ @ IO.return @ @ }"),
            "3"
        );
        assert_eq!(
            whnf(b"v8.4\n0\nIO.performIO IO.strict IO.return #4 @ @ @ }"),
            "4"
        );
        assert_eq!(
            whnf(b"v8.4\n0\nIO.performIO IO.atomic IO.return #6 @ @ @ }"),
            "6"
        );
    }

    #[test]
    fn reduces_array_primitives_as_io_actions() {
        assert_eq!(
            whnf(b"v8.4\n0\nIO.performIO A.alloc #3 @ #7 @ @ }"),
            "[7, 7, 7]"
        );
        assert_eq!(whnf(b"v8.4\n0\nIO.performIO A.size #0 #0 [2] @ @ }"), "2");
        assert_eq!(whnf(b"v8.4\n0\nIO.performIO A.copy #0 [1] @ @ }"), "[0]");
        assert_eq!(
            whnf(b"v8.4\n1\nIO.performIO IO.>> A.write #0 #0 #0 [3] :0 @ #1 @ #42 @ @ A.read _0 @ #1 @ @ @ }"),
            "42"
        );
    }

    #[test]
    fn reduces_mutable_bytestring_primitives_as_io_actions() {
        assert_eq!(
            whnf(b"v8.4\n0\nIO.performIO bsnew #2 @ #4 @ @ }"),
            "\"\\x00\\x00\""
        );
        assert_eq!(
            whnf(b"v8.4\n1\nIO.performIO IO.>> bswrite bsnew #2 @ #4 @ :0 @ #1 @ #65 @ @ bsread _0 @ #1 @ @ @ }"),
            "65"
        );
        assert_eq!(
            whnf(b"v8.4\n1\nIO.performIO IO.>> bsappbyte bsnew #0 @ #0 @ :0 @ #65 @ @ bsfreeze _0 @ @ @ }"),
            "\"A\""
        );
    }
}
