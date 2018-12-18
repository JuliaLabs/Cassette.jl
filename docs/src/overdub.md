# The Overdubbing Mechanism

```@meta
CurrentModule = Cassette
```

The central mechanism that drives Cassette usage is called the "overdubbing" mechanism.

A naive usage of this mechanism looks like this:

```julia
julia> using Cassette

julia> Cassette.@context Ctx;

julia> Cassette.overdub(Ctx(), /, 1, 2)
0.5
```

Okay - what did we actually just do here? From the output, it seems like we just computed
`1/2`...and indeed, we did! In reality, however, Cassette was doing a lot of work behind the
scenes during this seemingly simple calculation. Let's drill down further into this example
to see what's actually going on.

First, we define a new [`Context`](@ref) type alias called `Ctx` via the [`@context`](@ref)
macro. In practical usage, one normally defines one or more contexts specific to one's
application of Cassette. Here, we just made a dummy one for illustrative purposes. Contexts
are relatively simple to construct and understand, and are of central importance to Cassette's
operation. I recommend skimming the [`Context`](@ref) docstring before moving forward.

Next, we "overdubbed" a call to `1/2` w.r.t. `Ctx()` using the [`overdub`](@ref) function. To
get a sense of what that means, let's look at the lowered IR for the original call:

```julia
julia> @code_lowered 1/2
CodeInfo(
59 1 ─ %1 = (Base.float)(x)
   │   %2 = (Base.float)(y)
   │   %3 = %1 / %2
   └──      return %3
)
```

And now let's look at lowered IR for the call to `overdub(Ctx(), /, 1, 2)`

```julia
julia> @code_lowered Cassette.overdub(Ctx(), /, 1, 2)
CodeInfo(
59 1 ─       #self# = (Core.getfield)(##overdub_arguments#361, 1)                                                 │
   │         x = (Core.getfield)(##overdub_arguments#361, 2)                                                      │
   │         y = (Core.getfield)(##overdub_arguments#361, 3)                                                      │
   │         (Cassette.prehook)(##overdub_context#360, Base.float, x)                                             │
   │   %5  = (Cassette.overdub)(##overdub_context#360, Base.float, x)                                             │
   │         (Cassette.posthook)(##overdub_context#360, %5, Base.float, x)                                        │
   │   %7  = %5                                                                                                   │
   │         (Cassette.prehook)(##overdub_context#360, Base.float, y)                                             │
   │   %9  = (Cassette.overdub)(##overdub_context#360, Base.float, y)                                             │
   │         (Cassette.posthook)(##overdub_context#360, %9, Base.float, y)                                        │
   │   %11 = %9                                                                                                   │
   │         (Cassette.prehook)(##overdub_context#360, Base.:/, %7, %11)                                          │
   │   %13 = (Cassette.overdub)(##overdub_context#360, Base.:/, %7, %11)                                          │
   │         (Cassette.posthook)(##overdub_context#360, %13, Base.:/, %7, %11)                                    │
   │   %15 = %13                                                                                                  │
   └──       return %15                                                                                           │
)
```

There's obviously a lot more going on here than in the lowered IR for `1/2`, but if you
squint, you might notice that the overdubbed IR is actually the original IR with a special
transformation applied to it. Specifically, the overdubbed IR is the lowered IR for the given
function call with all internal method invocations of the form `f(args...)` replaced by
statements similar to the following:

```julia
begin
    Cassette.prehook(context, f, args...)
    %n = Cassette.overdub(context, f, args...)
    Cassette.posthook(context, %n, f, args...)
    %n
end
```

It is here that we experience our first bit of overdubbing magic: for every method call
in the overdubbed trace, we obtain several extra points of overloadability that we didn't
have before! In the [following section on contextual dispatch](contextualdispatch.md), we'll
explore how [`prehook`](@ref), [`posthook`](@ref), and even [`overdub`](@ref) itself can be
overloaded to add new contextual behaviors to overdubbed programs.

In the meantime, we should clarify how `overdub` is achieving this feat. Let's start by
examining a "pseudo-implementation" of `overdub`:

```julia
@generated function overdub(context::C, args...) where C<:Context
    reflection = Cassette.reflect(args)
    if isa(reflection, Cassette.Reflection)
        Cassette.overdub_pass!(reflection, C)
        return reflection.code_info
    else
        return :(Cassette.fallback(context, args...))
    end
end
```

As you can see, `overdub` is a `@generated` function, and thus returns a method body
computed from the run-time types of its inputs. To actually compute this method body,
`overdub` is doing something quite special.

First, via `Cassette.reflect`, `overdub` asks Julia's compiler to provide it with a bunch of
information about the original method call as specified by `args`. The result of this query
is `reflection`, which is a [`Cassette.Reflection`](@ref) object if the compiler found lowered IR for
`args` and `nothing` otherwise (e.g. if `args` specifies a built-in call like `getfield`
whose implementation is not, itself, Julia code). For the former case, we execute a pass over
the `reflection` and the lowered IR stored within (`Cassette.overdub_pass!`) to perform the
previously presented transformation, returning the new lowered IR as a `CodeInfo` object.
Otherwise, if `reflection` is not a `Reflection` object, then no lowered IR is available, so
we simply call the context's [`fallback`](@ref) method (which, by default, simply calls the
provided function).
