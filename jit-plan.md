# Plan: adding a JIT to MicroHs

**Status: draft.** This is not ready to execute. The main design questions
(see "Open questions") are not answered yet, and most of them need
measurements on real programs first. Phase 0 exists to get those
measurements, and the later phases will be revised from its results.

The plan replaces the summary of the JIT discussion of 2026-11-17. It is
written for the new data type encoding (release 0.17.0.0).

## Goals

* Make hot, strict code much faster: arithmetic, `case` on known data, and
  calls to known functions.
* Compiled code and interpreted code use the same graph and the same data
  representation, so values pass freely between them, and the interpreter
  stays the definition of the semantics.
* Everything is optional. The interpreter remains the only engine on
  platforms without a JIT (WASM, micro-controllers), and the JIT is
  behind a `WANT_JIT` flag.

Not goals: replacing the interpreter, changing laziness or sharing, or
speeding up code that is lazy anyway. Lazy code builds graphs, and the
combinators already do that well.

## What makes this different from other JITs

A JIT for a conventional language compiles functions that exist in the
program: methods, procedures, loops in them. In a combinator graph, most
of the interesting functions do not exist until the program runs.

* A function value is any graph whose head combinator has fewer arguments
  than it needs, i.e., a partial application. Bracket abstraction turns a
  local function into a combinator expression applied to its free
  variables, and an overloaded function applied to a dictionary into a
  partial application. These are built at run time.
* Function values change after they are built. Take
  `insInt = insert dOrdInt`. When `insInt` is first used, it is reduced to
  WHNF and overwritten with a partial application `P`. The combinators
  that distribute the dictionary (`S`, `S'`, ...) build shared nodes such as
  `sel dOrdInt` inside `P`. The first calls of `P` evaluate them and
  overwrite them with the `Int` comparison itself. After a few calls `P` is
  `insert` specialised to `Int`, and calls go to `P`, never through
  `insert`.
* So the graph specialises itself, by ordinary lazy evaluation and
  sharing. The hot code is often a node like `P` that is not in the
  combinator file, has no name, and does not exist in another run of the
  same program.

The consequences for the design:

* **The unit of compilation is a function node in the graph**, not a
  definition. Definitions (the roots of the top level bindings in the
  combinator file) are just the function nodes that happen to exist when
  the program starts.
* Compiled code is installed on the live node it was compiled from.
* Hot function nodes come in three kinds, which need different handling:
  1. **definition roots**, which exist in the file;
  2. **long-lived runtime nodes**, such as `P` above, a CAF's value, or a
     function stored in a data structure, called many times through the
     same node;
  3. **rebuilt nodes**: the same shape built over and over, because it
     depends on an argument of the enclosing function, e.g., a closure
     `\ y -> x + y`, or `insert d` where the dictionary `d` is an argument.
     Each node is used for one call of the enclosing function and then
     dropped.
* The combinator translation is fully lazy, so kind 3 is no bigger than
  it has to be. Bracket abstraction keeps a subexpression that does not
  mention the abstracted variable as one node, which is the sharing that
  Hughes's abstraction of maximal free expressions gives supercombinators.
  Tested with `trace` (2026-11-18): `insert dOrdInt` in the body of a
  monomorphic function, under a `case`, or in a local recursive function
  is specialised once for the whole run. So is `insert (dOrdList dOrdInt)`
  inside a polymorphic function called from one place at `[Int]`. An
  expression that depends on an argument `k` is built once per call of the
  function that binds `k`, and shared by the calls inside it. What full
  laziness does not share is the same expression at several places in the
  program: `buildP dOrdInt` at two call sites gives two specialised copies.
  Merging those is common subexpression elimination, and the number of
  copies is bounded by the size of the program.
* Kind 3 cannot be handled by identity. The nodes have the same code
  part, but different leaves: in `(+) x` or `C' f g x`, the free variable
  `x` is a leaf. Splitting a node's graph into **code** (combinators,
  primitives, definition roots, constants) and **data** (the other
  leaves, i.e., the free variables) gives a structural key: a hash of the
  code part. Compiled code is then shared by all nodes with that key and
  takes the data leaves as parameters, which is closure conversion done
  on the graph. Dictionaries are the hard case: as data they give no
  specialisation, and as code they make the key specific to one
  dictionary node.
* Ahead-of-time compilation can only handle kind 1, which is the less
  interesting kind. So compilation happens at run time, from the start.

## The interpreter today

Facts the JIT has to fit with (see "The evaluator" and "Data type
encoding" in `doc/MicroHs.md`):

* `evali` unwinds the spine onto a single stack and reduces combinators by
  overwriting the topmost application node of the redex, so results are
  shared.
* A constructor is `C_k_n`, and a value is the constructor applied to its
  fields. `case` is `D_a0_..._a{m-1} f0 ... f{m-1} e` (dense, `ak` is the
  number of fields of constructor `k`) or `E_m d A_k1_n1 f1 ... e`
  (sparse). The
  constructor and case combinators are tags in the pointer, and use no heap.
  Evaluating a `case` pushes a marker and evaluates the scrutinee. The
  constructor then finds the marker below it, selects the arm, and
  overwrites the case redex with `fk e1 ... en`, which costs n-1 cells.
* Every arm of a `case` has a known number of fields: a dense case has
  them in its atom (e.g., `D_0_2` for a list), and a sparse case in its
  `A_k_n` tags. The interpreter does not need them, but anything that turns
  the graph back into code does, since they cannot be read back from an arm
  after bracket abstraction (`\ x xs -> x` is just `K`). `Mhs.Reflect`
  uses them.
* Strict primitives (`+` on `Int`, etc.) avoid C recursion with markers on
  the stack (`BININT2`, `BININT1`, ...).
* IO is a function from a world token to a `P` pair. `return` is `P`, and
  `>>=` is `C`.
* Thread switches and exceptions use `longjmp`. On a switch the thread's
  stack is thrown away, and the thread restarts by evaluating its root
  again; progress lives only in the graph. `yield()` is only called at the
  top of `evali`, every `SLICE` (100000) reductions.
* The garbage collector does not move cells, so addresses are stable. Its
  roots are the stack, the threads, and some global tables. After loading,
  two GCs also simplify the graph (`C op` to the flipped `op`, `I x` to `x`,
  ...); later GCs do not.
* Class dictionaries are not constructor data. A dictionary is the class
  constructor partially applied to the superclass dictionaries and the
  methods, and a method is selected by applying the dictionary to a
  projection, e.g., `pure` is `U (K K4)`.

Baseline: `nfib 33` (11.4M calls, `if` on `n < 2`) takes 0.60 s on an
Apple M6. That is 131M reductions, 108M cells, and 19.2G instructions,
i.e., per call 11.5 reductions, 9.5 cells, and about 1700 instructions.
GC time is negligible. Earlier experiments on a cloud VM with the old
encoding put hand-written C equivalents at 0.04 s (boxed `Int`s, direct
calls) and 0.01 s (unboxed worker). Treat those as upper bounds on the
gain.

## Where the speed comes from

Compiled code only pays off in strict contexts:

* Strict primitives on values that are already evaluated are done
  directly, instead of with the marker scheme.
* `case` becomes a load of the constructor number and a branch, and the
  fields go into locals. No marker, and no cells for `fk e1 ... en`. A
  `case` on a comparison becomes compare and branch.
* Calls to known functions in strict positions become direct calls, and
  self tail calls become loops.
* Within a compiled body, `Int`s that are only used by strict primitives
  need not be boxed.
* With known dictionaries, method selection folds away.

Building a function body eagerly is not a gain in general. `nfib`'s body
would be about 13 cells if built eagerly, against 9.5 cells per call
today, since the combinators build the branch that is not taken lazily.
So a lazy subexpression, e.g., an argument to an unknown function, is
still built as a graph, but by straight-line code instead of by
combinator reductions.

## Architecture (tentative)

1. **Finding hot function nodes.** Sample often: every N reductions (N
   around 1000, to be tuned), take the node about to be reduced and walk
   its spine down to the head combinator. That gives the redex, and the
   unsaturated prefixes of the spine inside it (at most as many as the
   combinator's arity), which are the candidate function nodes. Hash each
   candidate's address into a table of (address, count) slots, and
   increment the count when the address matches. When a count reaches a
   threshold, compile the node. This is crude, and the points below are
   where it needs refining.
   * **Cost.** The reduction loop pays nothing extra. The countdown for
     time slices (`--glob_slice` at the top of `evali`) already exists:
     set it to N, take a sample when it runs out, and call `yield()` only
     on every SLICE/N-th expiry.
   * **Sharing.** The address check filters out unshared nodes. In a call
     `P x t` the candidates are `P`'s own spine nodes, `P`, and `P x`.
     `P x` is built for this call and never recurs, so its count stays at
     one, while `P` recurs. Of the candidates that recur, the outermost is
     the function.
   * **Stable addresses.** Candidates are partial applications, i.e.,
     values, and values are never overwritten, so an address names the
     same function for as long as the node lives. (The shared nodes inside
     it do change, as described above.) After a GC, drop the entries for
     cells that were freed (the mark bits say which), so that a reused
     address does not inherit a count.
   * **Inner nodes.** A function node is on the stack only at the first
     reduction of a call. The later reductions of the same call have
     `P`'s inner nodes as candidates, and they recur too, so they get
     counts of their own. Which one to compile, the first to reach the
     threshold or the outermost, has to be decided.
   * **Collisions.** A direct mapped table that replaces on a mismatch
     loses hot entries to cold nodes with the same hash. Possible fixes:
     decrement on a mismatch and replace at zero (which keeps the heavy
     hitters), a few entries per bucket, and aging (halve all counts now
     and then).
   * **Rebuilt nodes.** Kind 3 nodes never recur by address, so this
     table cannot see them. A second table keyed by the structural hash
     of the code part could. Phase 0 measures how much that would catch:
     the samples none of whose candidates recur.
2. **Recovery.** Turn a function node back into a supercombinator: apply
   it to fresh variables, as many as eta expansion gives (there is no
   metadata for runtime nodes), and reduce symbolically with the
   combinator rules, as `Mhs.Reflect` does, but in C. Shared nodes inside
   the function are never copied or evaluated by recovery: an unevaluated
   one stays a reference that the code evaluates with `evali` (and that
   may be a value by then), and an evaluated one is a constant. The
   exception is method selection from an evaluated dictionary, which is
   cheap and cannot fail, so recovery may do it to get the specialisation
   without waiting for the program to do it.
3. **IR and optimisation.** A small IR of strict and lazy expressions:
   evaluate, case, primitive, known call, build a graph, return. Mark the
   strict positions (case scrutinees, arguments of strict primitives,
   `seq`), never more, since evaluating anything else can diverge or raise
   an exception. Fold method selection on known dictionaries, turn self
   tail calls into loops, and unbox `Int`s that are only used strictly.
4. **Code generation.** First C, compiled at run time with the system C
   compiler and loaded with `dlopen` (phase 3); later native code for
   arm64 and x86-64 (phase 4).
5. **Installation.** Overwrite the function node with a `T_JIT` node that
   points to the code and to the original graph. The original graph is
   needed to fall back: when the code yields, when the C stack gets too
   deep, and for arguments the code does not handle. The `T_JIT` node also
   holds the constants the code refers to, so the GC traces them, and the
   code can be freed when the node dies. For kind 3, the code is shared by
   key, and the data leaves are its parameters. How a rebuilt node finds
   its code (at sampling, when built, or by compiling the function that
   builds it) is open.

### Rules for compiled code

These follow from how the runtime works, and they apply to both C and
native code:

* **The graph must be valid whenever control can leave the code
  abnormally.** A call to `evali` can switch threads, and an exception can
  be thrown; both `longjmp` past the compiled code's frames. So the code
  only writes complete reductions into the graph, and treats its own frames
  as disposable. Work that is lost this way is redone when the thread
  restarts from its root.
* **Yielding.** Compiled code decrements `glob_slice` on calls and loop
  back edges. When it runs out, the code builds the rest of its computation
  as a graph, overwrites its redex with it, and returns to `evali`, which
  then yields. Otherwise a long loop in compiled code would starve the
  other threads.
* **GC.** Cells are not moved, but a cell that is only referenced from a C
  local is not a root. Either allocate everything that is needed up front
  after one `GCCHECK` (as the primitives do), or push live pointers on the
  MicroHs stack across allocations.
* **Pointers into the graph.** A pointer in a C local is not safe across
  anything that can GC, which includes calls to `evali`, even when the cell
  is reachable from the graph. Marking short-circuits every reference to an
  indirection and frees the indirection, and the first GCs after loading
  also rewrite the graph. So keep only unboxed values and slots (the fields
  of reachable applications, which do not move) across such calls, and read
  the pointers from the slots after the last `GCCHECK`. The balance
  experiment (phase 1) crashed with a small heap until it did this.
* **The cost to the interpreter.** The code in `evali` that calls compiled
  code must not make the rest of `evali` slower. In the balance experiment,
  a `case` that passed `&x` and `&y` (locals of `evali`) to a helper cost 4%
  more instructions in a program that never used it, since `x` and `y`
  were then kept in memory in all of `evali`. Even a plain call costs 1.7%
  instructions, though no measurable wall clock. Return results in
  registers or through the redex, and measure the interpreter with the hook
  present but unused.
* **Sharing.** The result overwrites the redex (or makes it an
  indirection), exactly as a combinator reduction does. Shared nodes inside
  the compiled function stay shared.
* **C stack.** Direct calls use C stack, as `evali`'s recursive calls
  already do. Limit the depth, and fall back to building a graph when it is
  exceeded.
* **IO.** Compile pure code only to begin with, and let IO go through the
  interpreter.

## Phases

Each phase ends with a measurement and a decision about whether to go on.
Phase 0 is likely to change the phases after it.

### Phase 0: observation

Find out what hot code looks like at run time, using as much existing
machinery as possible.

* The sampler from step 1 of the architecture, behind an RTS flag, where
  reaching the threshold dumps the node instead of compiling it. Report
  the hottest nodes, the share of samples none of whose candidates recur,
  and how the results change with N, the table size, and the collision
  policy.
* Write each hot node with the existing serializer, as `writeSerialized`
  does, and decompile it offline with `Mhs.Reflect`. No recovery in C is
  needed for this phase.
* Run it on `nfib`, the programs in `benchmarks/`, and the compiler
  compiling itself.
* Answer: what share of the samples land on kinds 1, 2, and 3; how much
  of the hot code depends on dictionaries, and whether the selections are
  already evaluated when the code becomes hot; how big the recovered
  functions are, and whether they are strict enough to gain; how nested
  candidates should be chosen.

The sampler is also useful on its own, as a statistical profiler.

### Phase 1: hand-written stand-ins

Before writing any code generator, write by hand, as primitives in a
scratch copy of `eval.c`, the C code that the JIT would produce for a few
of the hot nodes found in phase 0, following the rules above (complete
reductions only, slice counting, GC discipline). Install them on the live
nodes with a test primitive, e.g., `primJitInstall :: Int -> a -> IO ()`,
which overwrites a function value with stand-in number `k`. That tests
installation on runtime nodes without a sampler.

* `nfib`, a tail recursive `Int` loop, and a `case` heavy function over a
  data type;
* an insertion into a search tree, installed on the node it becomes after
  specialisation to `Int`, i.e., `P` above;
* a closure that is rebuilt on every call (kind 3), with its free variable
  as a parameter;
* something hot from the compiler itself, to see what lazy code gets.

Run them on the M6 and on a less capable CPU (e.g., the Lima VM).
Continue only if the strict cases show a large gain (several times
faster) after following the rules.

**First result (2026-11-19): balancing in `MicroHs.IdentMap`.** Not one of
the stand-ins above, but the same idea. `balanceL` and `balanceR` are written
by hand as the primitives `balanceL` and `balanceR` in `eval.c`, and the
compiler uses them when `useBalancePrimitives` in `IdentMap` is `True`
(it is `False` by default). They are definition roots (kind 1) without
dictionaries, so the source refers to the primitives instead of installing
them on a node. The C code evaluates both subtrees (as the Haskell code
does), computes the sizes strictly, rotates, and builds the new nodes
directly. Compiling the compiler (3.84M calls, 14% of them rotate):

| | instructions | wall clock | GC | reductions | cells |
| --- | --- | --- | --- | --- | --- |
| Haskell | 483.9G | 21.24 s | 1.95 s | 3.491G | 3.927G |
| primitives | 439.6G (-9.2%) | 19.11 s (-10.0%) | 1.72 s (-11.8%) | 3.137G (-10.1%) | 3.398G (-13.5%) |

Each call saves about 92 reductions, 138 cells, and 11,500 instructions.
Most of that is combinator plumbing: after bracket abstraction `balanceL`
(four arguments, two `let`s, five branches) is about 60 combinators. The
rest is the size field, `sl + sr + 1`, which is a thunk in the Haskell
code. Computing it eagerly needs no strictness analysis, since `sl` and `sr`
have already been evaluated for the comparisons; it is cheap eagerness,
i.e., doing a cheap primitive on evaluated values. It must not raise an
exception that the lazy code would not. `Int` arithmetic wraps around
(`WANT_OVERFLOW` is off since 2026-11-19), so `+` cannot raise. With
`WANT_OVERFLOW` the code would have to build the thunk instead when the
addition overflows. The only other thing the C code does that the Haskell
code does not is to evaluate the sizes of the two grandchildren in a double
rotation (3.6% of the calls), so code that keeps the laziness exactly
should get almost all of the gain.

So code that is lazy and allocates a lot, like the compiler, can gain too,
when it has hot functions with strict parts. Two of the rules above
(pointers into the graph, and the cost to the interpreter) come from this
experiment. With the primitives in the runtime but not used, the compiler
runs 1.7% more instructions, with no measurable difference in wall clock.

**Second result (2026-11-19): `(==)` on `Ident`.** The profile of a
self-compile (`mhs --profile`) charges 5.3% of the reductions to the
`Eq Ident` instance, although `Ident _ i == Ident _ j = i == j` only
compares the names, which are `Text`s, i.e., `ByteString`s. The primitive
`identEq` does it directly: it evaluates both `Ident`s and their names, and
compares the bytestrings (same pointer, then length, then `memcmp`). The
method uses it when `useIdentEqPrimitive` in `MicroHs.Ident` is `True` (it
is `False` by default). Compiling the compiler (16.19M calls):

| | instructions | wall clock | GC | reductions | cells |
| --- | --- | --- | --- | --- | --- |
| Haskell | 513.7G | 21.81 s | 2.00 s | 3.663G | 4.030G |
| primitive | 502.2G (-2.2%) | 21.18 s (-2.9%) | 1.92 s (-4.0%) | 3.565G (-2.7%) | 3.933G (-2.4%) |

Each call saves about 6.1 reductions, 6.0 cells, and 710 instructions
(about 39 ns). The Haskell code is two `case`s on an `Ident`, the
unwrapping of the `Text` newtype (`cmp (==)`), and `bs==`, which evaluates
its arguments with markers: plumbing around one `memcmp`, with no real work.
Unlike balancing, the gain per call is small, and the total comes from the
number of calls. So for code like this, the cost of getting into compiled
code matters as much as the speed of the code. The profile charges about 11
reductions per call to `Eq Ident`, so about 5 per call are not removed by
the primitive; where they go is not known yet. The baseline differs from
the first result because the compiler, and the program it compiles, have
changed since.

### Phase 2: groundwork in the compiler and the combinator format

* **Arm arities for dense `case`: done** (2026-11-18, format v9.2). The
  atom is `D_a0_..._a{m-1}`, without `m`. The runtime keeps a table of
  `struct case_arity { uint16_t m; uint8_t *arities; }`, found by linear
  search when code is loaded and never freed, and the `V_CASED` tag holds
  the index (19 bits). The `case` code reads `m` with one load from the
  table; a version with a pointer per entry needed one more load and was
  about 1% slower in wall clock. With the evaluation of `D` and `E` split,
  the interpreter got slightly faster: -0.4% instructions compiling the
  compiler, -0.3% on `nfib`, wall clock unchanged. The compiler has 45
  entries (about 1.3 KB). Its combinator file grew 1% (1.3% compressed).
  The compiler now checks the limits of the encoding (2047 constructors,
  255 fields).
* **Names**, optionally, so that reports can say which definition a hot
  node came from. Runtime nodes have no names, but their code part
  usually refers to definition roots.
* **Keep full laziness: done.** It comes from the translation, but the
  compiler's other transformations could lose it. `tests/FullLaziness`
  counts evaluations for the cases above.
* Perhaps common subexpression elimination of constant expressions, e.g.,
  the same dictionary application at several call sites, so that each
  specialisation is made, and compiled, once. With structural keys the
  copies would share compiled code anyway.
* Whatever else phase 0 shows to be needed from the compiler.
* Anything else that changes the combinator file format needs a version
  bump and `make preparedist`.

Measure: the interpreter must not get slower (instructions, wall clock,
and GC time; GC changes do not show in instruction counts).

### Phase 3: recovery, and C compiled at run time

* Recovery and the IR, with a debug flag that prints the recovered code
  (and can be checked against `Mhs.Reflect`).
* Code generation as C. Batches of hot nodes are written to a file,
  compiled to a shared library with the system C compiler, loaded with
  `dlopen`, and installed on the nodes they came from.
* This needs no executable memory and no code generator, and shows the
  real benefit of the scheme before the native emitter is written.
  Compiling takes tens of milliseconds per batch and needs a C compiler on
  the machine, so this is a vehicle for experiments, not the product.
* Differential testing: run the test suite with "compile everything that
  recovery handles" and compare with the interpreter.

### Phase 4: native code generation

* Our own emitter for arm64, then x86-64. The code needs about 15
  operations: loads, stores, compare, branch, add/subtract, calls to
  runtime helpers (`evali`, allocation, GC check, yield). A thin macro layer
  over a few hundred lines per architecture. arm64 is easier (fixed width);
  x86-64 needs REX/ModRM for a few addressing forms.
* Executable memory: `MAP_JIT` with `pthread_jit_write_protect_np` and
  `sys_icache_invalidate` on Apple Silicon; `mmap`/`mprotect` and an
  instruction cache flush on Linux arm64.
* Alternatives if this gets too costly: sljit (one portable IR for both
  targets, own executable memory allocator, but a larger dependency;
  Apple Silicon support not checked), or DynASM from LuaJIT (assembly
  templates per architecture, preprocessor written in Lua, no handling of
  executable memory).
* A threshold that can be set with an RTS flag (0 turns the JIT off).

## Testing

* Differential testing: the whole test suite with the JIT compiling
  everything it can, compared with the interpreter.
* A stress mode with a tiny `SLICE`, so that thread switches happen inside
  compiled code all the time, and one with a tiny heap, so that GCs do too.
* Tests that throw exceptions out of compiled code, and that block on
  `MVar`s from threads that run compiled code.

## Measurements

* Benchmarks: `nfib`, the programs in `benchmarks/` (e.g., `FibInt`,
  `TightIntLoop`, `MergeSort`, `PrimeSieve`, `ExprEval`,
  `NumericIntegration`), and the compiler compiling itself
  (`make timecompile`).
* Report wall clock, instructions retired, GC time, reductions, and
  allocated cells, with interleaved runs, on the M6 and on a weaker CPU.
  Instruction counts do not show memory effects (turning off GC
  prefetching changed instructions by -0.5% but wall clock by +12.5%).

## Open questions

These have to be answered before phase 3. Phase 0 should answer most of
them.

* **What is a function?** The graph has no lambda boundaries. Every
  unsaturated prefix of a spine is a function value: in a call `P x t`,
  both `P` and `P x` are. The useful one is the one that is shared, and
  the sampler finds that out from which addresses recur. Still open is
  how to choose between a function node and its inner nodes, which recur
  too. If the sampler is not good enough, the compiler could mark lambda
  boundaries with a marker combinator (like ticks), which costs a
  reduction per call unless the marker is removed when the JIT is off.
* **Code and data.** Which leaves of a node's graph are code and which
  are data (free variables)? Definition roots, combinators, primitives,
  and literals are code. A runtime node could be either. Possibly: data
  unless it is reached through a definition root, which needs the loader to
  keep the set of roots.
* **Dictionaries.** As data, they give no specialisation; as code, they
  split the key. A dictionary built at run time (e.g., `Eq [a]` from
  `Eq a`) is a fresh node every time, so it also defeats keys by address.
  Possible fixes: a bounded structural hash of the dictionary, or
  replacing a dictionary that is structurally equal to a known one with an
  indirection to it.
* **When to compile.** The graph specialises itself while it runs, so
  compiling early can miss method selections that would have been
  evaluated a few calls later. Recovery can do safe selections itself
  (architecture step 2). Is that enough, or should code be recompiled?
* **Rebuilt nodes.** How does a newly built node of kind 3 find its
  compiled code? At sampling time most calls are already over. Compiling
  the function that builds the node, so that the inner function becomes
  a direct call, may be the better answer. Full laziness already floats
  out everything that does not depend on an argument, so what is left
  really varies at run time.
* **Lifetime.** Code installed on a node can be freed when the node dies.
  Code shared by key has to be kept, or counted.
* **Is recovery from the graph good enough,** or should the compiler emit
  an IR beside the combinators? That gives better code but bigger files,
  and, as above, the interesting functions are mostly not in the file.
* What should the threshold be, and does compiling cause noticeable
  pauses?

## Risks

* The gain on typical, lazy, allocation heavy code may be small.
* If most hot code is in rebuilt nodes (kind 3), and neither keys nor
  compiling the builders work out, a JIT on nodes gains little.
* Two execution engines to keep in sync. Differential testing is the main
  defence.
* Platform details for executable memory, especially W^X on Apple
  Silicon.
* None of it helps WASM or micro-controller targets. (On WASM in a
  browser, generating WASM modules at run time is possible, but it is not
  part of this plan.)
