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
    tmp = Cassette.execute(context, f, args...)
    tmp = isa(tmp, Cassette.OverdubInstead) ? overdub(context, f, args...) : tmp
    Cassette.posthook(context, tmp, f, args...)
    tmp
end
```

This transformation yields several extra points of overloadability in the form of various
Cassette methods, such as [`prehook`](@ref), [`posthook`](@ref), and [`execute`](@ref).
Together, these methods form Cassette's "contextual dispatch" interface, so called because it
allows the extra context parameter to participate in what would normally be a simple dispatch
to the underlying method call.

In this section of the documentation, we'll go over these functions in a bit more detail.

To begin, let's define a simple contextual prehook by overloading the [`prehook`](@ref)
method w.r.t. to a dummy context:

```julia
julia> using Cassette

julia> Cassette.@context Ctx
Cassette.Context{nametype(Ctx),M,P,T,B} where B<:Union{Nothing, IdDict{Module,Dict{Symbol,BindingMeta}}} where P<:Cassette.AbstractPass where T<:Union{Nothing, Tag} where M

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

Actually, there's a subtlety about `overdub` here we should address before moving on. Why
wasn't the first line in the trace log `/(1, 2)`? I'll leave the answer as an exercise to
the reader - just recall the definition of `overdub` from the previous section. If this
the barrier between the `overdub` and the contextual dispatch interface seems confusing, try
comparing the output from the above example with the output generated via
`overdub(Ctx(), () -> 1/2)`.

For pedagogy's sake, let's make our `prehook` slightly more complicated; let's only print
calls whose first argument matches a specific type. A nice configurable way to do this is
as follows:

```julia
# reset our prehook to a no-op
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
primitives, as defined by the [`execute`](@ref). Here's Cassette's default definition
of `execute`:

```julia
execute(::Context, ::Vararg{Any}) = OverdubInstead()
```

With this definition in mind, the default case for the above contextual dispatch
transformation can be reduced to:

```julia
begin
    Cassette.prehook(context, f, args...)
    tmp = overdub(context, f, args...)
    Cassette.posthook(context, tmp, f, args...)
    tmp
end
```

In other words, the `execute`'s default behavior is to not interfere with the recursive
application of `overdub` at all. If `execute` is ever overloaded to return something other
than `OverdubInstead`, however, then it means the recursive overdubbing stops. Thus, in
Cassette terminology, overloading `execute` defines a "contextual primitive" w.r.t. the
overdubbing mechanism.

Note that upon context definition (via [`@context`](@ref)) a bunch of reasonable default
contextual primitives are generated automatically. It is possible, of course, to simply
override these defaults if necessary. For more details, see [`@context`](@ref).

As an aside, one might wonder why the default definition of `execute` isn't simply
`execute(context, args...) = overdub(context, args...)`. The reason is that this definition
is a bit harder on the compiler, since it adds an extra cycle (e.g. `execute` -> `overdub`
-> `execute`) in the recursion inherent to Cassette's overdubbing mechanism. The branching
based definition is much nicer, since it is much cheaper to evaluate a trivial `isa` check
at compile time than it is to determine the worth of inferring through deep multi-cycle
recursion.

To get a sense of the interaction between `execute` and `overdub`, let's reimplement our
previous nested tracing example using recursion instead of maintaining a stack:

```julia
using Cassette

Cassette.@context TraceCtx

function Cassette.execute(ctx::TraceCtx, args...)
    subtrace = Any[]
    push!(ctx.metadata, args => subtrace)
    if Cassette.canoverdub(ctx, args...)
        newctx = Cassette.similarcontext(ctx, metadata = subtrace)
        return Cassette.overdub(newctx, args...)
    else
        return Cassette.fallback(ctx, args...)
    end
end

trace = Any[]
x, y, z = rand(3)
f(x, y, z) = x*y + y*z
Cassette.overdub(TraceCtx(metadata = trace), () -> f(x, y, z))

# returns `true`
trace == Any[
   (f,x,y,z) => Any[
       (*,x,y) => Any[(Base.mul_float,x,y)=>Any[]]
       (*,y,z) => Any[(Base.mul_float,y,z)=>Any[]]
       (+,x*y,y*z) => Any[(Base.add_float,x*y,y*z)=>Any[]]
   ]
]
```
