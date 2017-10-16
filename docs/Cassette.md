# What is Cassette?

Cassette is a Julia package that provides a just-in-time (JIT) pass injection mechanism,
referred to as an **overdubbing mechanism**, that interleaves external context-specific
code transformations with Julia's normal JIT-compilation cycle. Drawing inspiration from the
LLVM community's thriving ecosystem of transformation passes, we hope that Cassette's
overdubbing mechanism will jump-start an ecosystem of Julia compiler "plugins", modular
compiler optimizations/extensions that exist outside of the compiler itself.

On top of its overdubbing mechanism, Cassette implements two new features for the Julia
language: **contextual dispatch** and **contextual metadata propagation**. Using these
features, Julia developers can define new "contexts" that modify the execution of
Cassette-unaware Julia code to achieve a variety of goals.

For example, downstream applications of Cassette include automatic differentiation, interval
constraint programming, dynamic code analysis (e.g. profiling, `rr`-style debugging, etc.),
JIT transpilation to GPU backends, automatic parallelization/rescheduling, memoization,
high-level automated memory management and code fuzzing.

# How does Cassette work?

To understand how Cassette works, one must have at least a superficial understanding of
Julia's run-compile cycle and the language's `@generated` function feature.

## Julia's Run-Compile Cycle

A diagram of Julia's run-compile cycle is shown below. While this diagram is quite
incomplete<sup id="f1-anchor">[1](#f1)</sup>, it is sufficient for the purposes of
understanding Cassette.

![Compilation Cycle](compile-cycle.png)

Here, the colored boxes are phases of the runtime-compile cycle, the arrows denote
execution flow, and the gray boxes are the input/output of each phase. The red boxes
are phases that Cassette interacts with directly; we will describe this interaction
in detail later on. In the meantime, here's an outline of the process described by
the diagram, starting at "Function Call":

1. A function call is the application of a callable Julia object (the function) on other
Julia objects (the arguments). The function call is resolved to its corresponding method
body by dispatching on the call's type signature. For example, `f(1.0, "a", [1 + im])` has
the type signature `Tuple{typeof(f),Float64,String,Vector{Complex{Int}}}`. Cassette
interacts with this phase by replacing `f` with `Context(f)` for a given `Context`.

2. Dispatch occurs via a lookup performed on Julia's internal method table, where the
call's type signature is kind of key. This method lookup can yield one of two results.
If the method body for this function call has already been compiled in the past, and
the cached native code from that compilation is still valid (a determination made via
Julia's "world-age" mechanism), then the rest of the compile cycle for this function
call is skipped and the cached native code is executed. Otherwise, the method lookup
yields an untyped Julia AST, represented in Julia's internal intermediate
representation (IR).

3. Assuming that there is no valid cached native code for the function call, Julia's
compiler performs type inference on the method body AST, leveraging the function call's
type signature as a starting point.

4. Various optimization passes - most notably, inlining and `@generated` function
expansion - are interleaved with Julia's type inference process, hence the cycle
formed between "Type Inference" and "Optimizations/Inlining"


utilizes the function call's type signature along with the method body AST
to


 value, in this case, can be either an AST



function call's type signature is used

into Julia's method table


for "dispatch" - a lookup of
the corresponding value in Julia

 perform a lookup on Julia's
internal method table. If a valid






In particular

red phases

<b id="f1">[1]</b> To be considered complete, this diagram would need to also have a
"parse time" section, which would include Julia AST construction and macro expansion.
[↩](#f1-anchor)
