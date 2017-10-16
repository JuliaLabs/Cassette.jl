# What is Cassette?

Cassette is a Julia package that provides a just-in-time (JIT) pass injection mechanism,
referred to as an **overdubbing mechanism**, that interleaves external context-specific
code transformations with Julia's normal JIT-compilation cycle. Drawing inspiration from the
LLVM community's thriving ecosystem of transformation passes, we hope that Cassette's
overdubbing mechanism will jump-start an ecosystem of Julia compiler "plugins" - modular
extensions that implement new compiler optimizations and features external to the
compiler itself.

On top of its overdubbing mechanism, Cassette provides a **contextual execution framework**
for the Julia language via the implementation of **contextual dispatch** and **contextual
metadata propagation**. This framework enables users to define new "contexts" that overlay
normal Julia methods, which can be dispatched on using Julia's existing operator-overloading
syntax. Using its overdubbing mechanism, Cassette can instrument context-unaware Julia
code with contextually defined behaviors, enabling granular method interception and
metadata propagation within "black-box" user code.

Downstream applications for Cassette include automatic differentiation, interval constraint
programming, dynamic code analysis (e.g. profiling, `rr`-style debugging, etc.), JIT
transpilation to GPU backends, automatic parallelization/rescheduling, memoization,
high-level automated memory management and code fuzzing.

# Background

To understand how Cassette works, one must first have at least a cursory knowledge of
where Cassette fits into Julia's run-compile cycle, as well as Julia's `@generated`
function feature. An explanation of the former is given in the following section, while an
explanation of the latter is given in [Julia's official `@generated` function
documentation](https://docs.julialang.org/en/latest/manual/metaprogramming/#Generated-functions-1).

## Julia's Run-Compile Cycle

A diagram of Julia's run-compile cycle is shown below. While this diagram is quite
incomplete<sup id="f1-anchor">[1](#f1)</sup>, it is sufficient for the purposes of
understanding Cassette.

![Compilation Cycle](compile-cycle.png)

Here, the colored boxes are phases of the run-compile cycle, the arrows denote execution
flow, and the gray boxes are the input/output of each phase. The red boxes are phases that
Cassette interacts with directly; we will describe this interaction in detail later on. In
the meantime, here's an outline of the process described by the diagram, starting at
"Function Call":

1. A function call is the application of a callable Julia object (the function) on other
Julia objects (the arguments). The function call is resolved to its corresponding method
body by dispatching on the call's type signature. For example, `f(1.0, "a", [1 + im])` has
the type signature `Tuple{typeof(f),Float64,String,Vector{Complex{Int}}}`.

2. Dispatch occurs via a lookup performed on Julia's internal method table, where the
call's type signature is kind of key. This method lookup can yield one of two results.
If the method body for this function call has already been compiled in the past, and
the cached native code from that compilation is still valid (a determination made via
Julia's "world-age" mechanism), then the rest of the compile cycle for this function
call is skipped and the cached native code is executed (i.e., we skip to step 6).
Otherwise, the method lookup yields an untyped Julia AST, represented in Julia's internal
intermediate representation (IR).

3. Assuming that there is no valid cached native code for the function call, Julia's
compiler performs type inference on the method body AST, leveraging the function call's
type signature as a starting point.

4. Various optimization passes - most notably, inlining and `@generated` function
expansion - are interleaved with Julia's type inference process, hence the cycle
formed between "Type Inference" and "Optimizations/Inlining".

5. The type inferred and optimized IR for the method body is passed to the compiler's
code generator, where the IR is converted into LLVM bitcode, which is in turn converted
to executable native code.

6. The native code is executed, potentially returning runtime values that are then fed
to another function call, at which point the cycle repeats.

<b id="f1">[1]</b> To be considered complete, this diagram would need to also have a
"parse time" section, which would include Julia AST construction and macro expansion.
[↩](#f1-anchor)

## Where Cassette Fits In The Run-Compile Cycle

As denoted by the red boxes in the run-compile diagram, Cassette interacts with the
"Function Call" and "Optimizations/Inlining" phases.

Cassette interacts with the "Function Call" phase in the sense that a target function is
wrapped in Cassette's special `Execute` callable wrapper and associated with a given
"context type", thus enabling contextual dispatch. Calling this `Execute` object with the
target function call's original arguments incurs a call to a special `@generated` function
defined within Cassette, bringing us to Cassette's interaction with "Optimization/Inlining"
phase.

The generator expansion that occurs during the "Optimization/Inlining" phase serves as the
injection site for Cassette's overdubbing mechanism. In the case of Cassette's built-in
contextual execution framework, the overdubbing mechanism facilitates a JIT-rewrite of the
target method's code in order to propagate the context type and any associated metadata to
downstream function calls.
