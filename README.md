# Cassette

## DISCLAIMER

Cassette is still in development. At any given time, the implementation might be ugly, buggy, incomplete, slow, and/or untested.

Cassette relies on new reflection features and compiler performance improvements that will hopefully land in Julia 1.x. Until an initial version of Cassette is released, I can't guarantee that Cassette's `master` branch won't rely on some weird custom version of Julia.

Cassette targets downstream package developers, not Julia end-users. Downstream developers are expected to have a solid understanding of Julia's type system, metaprogramming facilities, and dispatch mechanism.

Last updated for Julia commit: 69e559d496c69c7909ba87631cb552295e77255f

## Overview

Cassette is a Julia package that provides a mechanism for dynamically injecting code transformation passes into Julia’s just-in-time (JIT) compilation cycle, enabling post hoc analysis, optimization, and modification of "Cassette-unaware" Julia programs. For example, developers can simply write a Julia function of the form `pass(signature::Type{Tuple{...}}, method_body::CodeInfo)::CodeInfo`, and then use Cassette to apply `pass` to every method called within an execution context. Importantly, target programs need not have been written with any knowledge of Cassette - no manual source annotation or refactoring of the target code is required!

Cassette implements several other novel features on top of its JIT pass-injection mechanism. A superficial preview of these features is provided below, but for a more complete description of Cassette, the problems it solves, and the mechanisms that enable those solutions, see Cassette's [design document](docs/design.md) (unfortunately, that document is out-of-date and incomplete at the moment, but I'll update it and add new information when I get the time).

Downstream applications for Cassette include automatic differentiation, interval constraint programming, dynamic code analysis (e.g. profiling, `rr`-style debugging, etc.), JIT transpilation to specialized backends, automatic parallelization/rescheduling, memoization, high-level automated memory management and code fuzzing.

### Contextual Dispatch

On top of its JIT pass-injection mechanism, Cassette implements a new form of dispatch, dubbed **contextual dispatch**. Contextual dispatch leverages Julia's existing multiple dispatch semantics to enable new behaviors to be defined on top of (or in place of) methods called within a specific execution context.

Here's a simple example that uses Cassette to implement a naive form of GPU "transpilation":

```julia
# pretend we have some package that provides a bunch of wrapped GPU kernels
using NativeGPUFunctions

# Declare a new context. This defines a new type `GPUCtx <: Cassette.Context`.
Cassette.@context GPUCtx

# Define some `GPUCtx` "primitives". If, while executing code in a GPU context, some method
# is encountered that matches the signature of one of these primitives, that method call
# will dispatch to the primitive definition provided here. For example, these definitions
# will cause all `Base.sin(x)` calls to dispatch to `NativeGPUFunctions.sin(x)`.
Cassette.@primitive Base.sin(x::Number) where {__CONTEXT__<:GPUCtx} = NativeGPUFunctions.sin(x)
Cassette.@primitive Base.cos(x::Number) where {__CONTEXT__<:GPUCtx} = NativeGPUFunctions.cos(x)
⋮ # pretend we do this for all the functions we care about

f(args...) = # some function implemented in normal, GPU-unaware Julia code

# Execute `f` in a `GPUCtx`. `Cassette.overdub` takes in a context type and a function,
# and produces a new, "contextualized" version of that function, which can then be called
# with the original function's arguments.
Cassette.overdub(GPUCtx()) do
    f(args...)
end
```

Cassette provides many other mechanisms similar to the `@primitive` macro that allow context-authors to precisely alter behavior, interleave side-effects, and generally manipulate programs executed under their context.

### Metadata Propagation

On top of contextual dispatch, Cassette provides a framework for **contextual metadata propagation** that enables both trace-local metadata and argument-local metadata to be stored and passed alongside underlying program state without interfering with target program execution. Importantly, this metadata framework automatically leverages contextual dispatch to circumvent type constraints in target programs, rendering techniques like automatic differentiation applicable to previously inapplicable Julia programs.

In the below example, trace-local metadata is used to count the number of method calls with particular argument type(s):

```julia
julia> using Cassette

julia> mutable struct Count{T}
           count::Int
       end

julia> Cassette.@context CountCtx

# One can use the `@prehook` macro to define a callback that gets called right before
# methods matching the given signature are called. Unlike `@primitive`, `@prehook` does not
# redefine how normal method calls are dispatched in the target program; it is just used to
# add side-effects as specified by the provided callback.
#
# Note here that we are dispatching on the type of trace-level metadata to define a prehook
# that increments a counter every time one or more arguments of type `T` are encountered in
# the execution trace.
julia> Cassette.@prehook function (::Any)(arg::T, args::T...) where {T,__CONTEXT__<:CountCtx,__METADATA__<:Count{T}}
           __trace__.metadata.count += 1
       end

julia> f(x) = map(string, x)
f (generic function with 1 method)

# let's the count the number of calls that have `Union{String,Int}` arguments
julia> c = Count{Union{String,Int}}(0)
Count{Union{Int64, String}}(0)

# here, `c` is our trace-local metadata
julia> Cassette.overdub(CountCtx, f, metadata = c)(1:10)
10-element Array{String,1}:
 "1"
 "2"
 "3"
 "4"
 "5"
 "6"
 "7"
 "8"
 "9"
 "10"

julia> c
Count{Union{Int64, String}}(1464)
```

Argument-local metadata is a bit more involved, so we won't go into it in this README. See the [design document](docs/design.md) for more details.
