# Contextual Dispatch

```@meta
CurrentModule = Cassette
```

In [the previous section](overdub.md), we saw how, within a given execution
trace, Cassette's `overdub` mechanism transforms every method invocation of the
form `f(args...)` into statements similar to the following:

```julia
begin
    Cassette.prehook(context, f, args...)
    %n = Cassette.overdub(context, f, args...)
    Cassette.posthook(context, %n, f, args...)
    %n
end
```

This transformation yields several extra points of overloadability in the form
of various Cassette methods, such as [`prehook`](@ref), [`posthook`](@ref), and
even [`overdub`](@ref) itself. Together, these methods form Cassette's
"contextual dispatch" interface, so-named because it enables an extra context
parameter to participate in what would normally be a simple dispatch to the
underlying method call.

In this section of the documentation, we'll go over these functions in a bit more detail.

To begin, let's define a simple contextual prehook by overloading the [`prehook`](@ref)
method w.r.t. to a dummy context:

```julia
julia> using Cassette

julia> Cassette.@context Ctx;

# this prehook implements simple trace logging for overdubbed functions
julia> Cassette.prehook(::Ctx, f, args...) = println(f, args)

julia> Cassette.overdub(Ctx(), /, 1, 2)
float(1,)
AbstractFloat(1,)
Float64(1,)
sitofp(Float64, 1)
float(2,)
AbstractFloat(2,)
Float64(2,)
sitofp(Float64, 2)
/(1.0, 2.0)
div_float(1.0, 2.0)
0.5
```

Cool beans!

Actually, there's a subtlety about `overdub` here that we should address before
moving on. Why wasn't the first line in the trace log `/(1, 2)`? If the answer
isn't obvious, recall the definition of `overdub` from the previous section. With
that definition in mind, it makes sense that `/(1, 2)` is not printed in the
above example, since `prehook(Ctx(), /, 1, 2)` is not actually ever called in
the above example. If this still seems confusing, compare the output from the
above example with the output generated via `overdub(Ctx(), () -> 1/2)`.

Moving on, let's make our `prehook` slightly more complicated for pedagogy's sake.
This time around, we'll only print calls whose first argument matches a
specific type. A nice configurable way to do this is as follows:

```julia
# reset our prehook fallback for `Ctx` to a no-op
julia> Cassette.prehook(::Ctx, f, args...) = nothing

# parameterize our prehook on the type of metadata stored in our context instance
julia> Cassette.prehook(::Ctx{Val{T}}, f, arg::T, rest...) where {T} = println(f, (arg, rest...))

# construct our context instance with metadata to configure the prehook
julia> Cassette.overdub(Ctx(metadata=Val(Int)), /, 1, 2)
float(1,)
AbstractFloat(1,)
Float64(1,)
float(2,)
AbstractFloat(2,)
Float64(2,)
0.5

julia> Cassette.overdub(Ctx(metadata=Val(DataType)), /, 1, 2)
sitofp(Float64, 1)
sitofp(Float64, 2)
0.5
```

Also of note is `prehook`'s long-lost cousin [`posthook`](@ref), with which `prehook` shares
many similarities. In fact, these functions are so similar that we won't be spending too much
time on `posthook` individually. The key difference between `prehook` and `posthook` is that
`posthook` runs *after* the overdubbed invocation is executed, such that it has access to the
output of the overdubbed invocation.

For example, here we use `posthook` and `prehook` together to accumulate a trace that
preserves nesting information:

```julia
using Cassette

Cassette.@context TraceCtx

mutable struct Trace
    current::Vector{Any}
    stack::Vector{Any}
    Trace() = new(Any[], Any[])
end

function enter!(t::Trace, args...)
    pair = args => Any[]
    push!(t.current, pair)
    push!(t.stack, t.current)
    t.current = pair.second
    return nothing
end

function exit!(t::Trace)
    t.current = pop!(t.stack)
    return nothing
end

Cassette.prehook(ctx::TraceCtx, args...) = enter!(ctx.metadata, args...)
Cassette.posthook(ctx::TraceCtx, args...) = exit!(ctx.metadata)

trace = Trace()
x, y, z = rand(3)
f(x, y, z) = x*y + y*z
Cassette.overdub(TraceCtx(metadata = trace), () -> f(x, y, z))

# returns `true`
trace.current == Any[
    (f,x,y,z) => Any[
        (*,x,y) => Any[(Base.mul_float,x,y)=>Any[]]
        (*,y,z) => Any[(Base.mul_float,y,z)=>Any[]]
        (+,x*y,y*z) => Any[(Base.add_float,x*y,y*z)=>Any[]]
    ]
]
```

Next, let's tackle the meatiest part of the contextual dispatch interface: contextual
primitives. A method invocation of the form `f(args...)` within a given context `Ctx`
is a primitive w.r.t. `Ctx` if `overdub(Ctx(), f, args...)` does not recursively
overdub the function calls comprising the invoked method's implementation.
There are two cases where `overdub(Ctx(), f, args...)` does not correspond to
recursively overdubbing `f`'s implementation:

1. `f(args...)` might be a built-in with no overdubbable Julia implementation (e.g. `getfield`), in which case `overdub(Ctx(), f, args...)` immediately redirects to `Cassette.fallback(Ctx(), f, args...)`.

2. `overdub` can be overloaded by the user such that `overdub(::Ctx, ::typeof(f), ...)` dispatches to a context-specific primitive definition.

If this definition isn't exactly intuitive, never fear - the concept of a
contextual primitive is more easily understood via examples. The simplest
example is to define a context that simply redirects all method call of a
specific type (let's say `sin(x)`) to a different method call of a specific
type (let's say `cos(x)`). This can be expressed as follows:

```julia
using Cassette, Test

Cassette.@context SinToCosCtx

# Override the default recursive `overdub` implementation for `sin(x)`.
# Note that there's no tricks here; this is just a normal Julia method
# overload using the normal multiple dispatch semantics.
Cassette.overdub(::SinToCosCtx, ::typeof(sin), x) = cos(x)

x = rand(10)
y = Cassette.overdub(SinToCosCtx(), sum, i -> cos(i) + sin(i), x)
@test y == sum(i -> 2 * cos(i), x)
```

Pretty nifty!

Here's a more motivating example. Below, we define a context that allows us to
memoize the computation of Fibonacci numbers (many thanks to the illustrious
Simon Byrne, [the original author of this example](https://stackoverflow.com/questions/52050262/how-to-do-memoization-or-memoisation-in-julia-1-0/52062639#52062639)):

```julia
using Cassette: Cassette, @context, overdub, recurse

fib(x) = x < 3 ? 1 : fib(x - 2) + fib(x - 1)
fibtest(n) = fib(2 * n) + n

@context MemoizeCtx

function Cassette.overdub(ctx::MemoizeCtx, ::typeof(fib), x)
    result = get(ctx.metadata, x, 0)
    if result === 0
        result = recurse(ctx, fib, x)
        ctx.metadata[x] = result
    end
    return result
end
```

Note that this example uses Cassette's [`recurse`](@ref) function. This function is
exactly equivalent to Cassette's default `overdub` implementation, but is not
meant to be overloaded by users, thus allowing one to recursively overdub
"through" invocations that might otherwise be contextual primitives.

We can do some toy performance tests to see that we get the expected speedup using this implementation (skipping the warm-up calls used to compile both functions):

```julia
julia> ctx = MemoizeCtx(metadata = Dict{Int,Int}());

julia> @time Cassette.overdub(ctx, fibtest, 20)
  0.000011 seconds (8 allocations: 1.547 KiB)
102334175

julia> @time Cassette.overdub(ctx, fibtest, 20)
  0.000006 seconds (5 allocations: 176 bytes)
102334175

julia> @time fibtest(20)
  0.276069 seconds (5 allocations: 176 bytes)
102334175
```

!!! note
    A bunch of reasonable default contextual primitives are generated automatically
    upon context definition. It is possible, of course, to simply override these
    defaults if necessary. For more details, see [`@context`](@ref).

Finally, to get a sense of the interaction between `recurse` and `overdub`, let's
reimplement our previous nested tracing example using recursion instead of maintaining
a stack:

```julia
using Cassette

Cassette.@context TraceCtx

function Cassette.overdub(ctx::TraceCtx, args...)
    subtrace = Any[]
    push!(ctx.metadata, args => subtrace)
    if Cassette.canrecurse(ctx, args...)
        newctx = Cassette.similarcontext(ctx, metadata = subtrace)
        return Cassette.recurse(newctx, args...)
    else
        return Cassette.fallback(ctx, args...)
    end
end

trace = Any[]
x, y, z = rand(3)
f(x, y, z) = x*y + y*z
Cassette.overdub(TraceCtx(metadata = trace), f, x, y, z)

# returns `true`
trace == Any[
   (f,x,y,z) => Any[
       (*,x,y) => Any[(Base.mul_float,x,y)=>Any[]]
       (*,y,z) => Any[(Base.mul_float,y,z)=>Any[]]
       (+,x*y,y*z) => Any[(Base.add_float,x*y,y*z)=>Any[]]
   ]
]
```
