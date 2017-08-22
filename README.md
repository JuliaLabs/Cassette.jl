# Cassette

## DISCLAIMER

Cassette is still in development. At any given time, the implementation might be ugly,
buggy, incomplete, slow, and/or untested. Cassette relies on new reflection features and
compiler performance improvements that will hopefully land in Julia 1.0; until an initial
version of Cassette is released, I can't guarantee that Cassette's `master` branch won't
rely on some weird custom version of Julia.

## What is Cassette?

Cassette is a Julia package that provides...

- ...a *contextual call interceptor*.
- ...a *contextual metadata propagation* framework.
- ...a *computation graph framework* with two graph implementations (one for optimized for
a dynamic regime, the other for a static regime).

Cassette targets downstream package developers, not Julia end-users. Cassette's API and
documentation expects downstream developers to have a solid understanding of Julia's type
system, metaprogramming facilities, and dispatch mechanism.

Downstream applications for Cassette include automatic differentiation, interval constraint
programming, dynamic code analysis (e.g. profiling, `rr`-style debugging, etc.), JIT
transpilation to different backends, automatic parallelization/rescheduling, memoization,
automatic preallocation, and code fuzzing.

## Cassette's Contextual Call Interceptor

First and foremost, "contextual call intercepting" is a phrase I just made up. If anybody
knows of any existing terms in the literature for Cassette's weird brand of execution
tracing, feel free to let me know.

Cassette can instrument your Julia code in order to intercept native Julia method calls as
they occur during program execution. Which calls are intercepted and what actually happens
when interception occurs are both defined with respect to a Cassette "context", which is
itself defined by Cassette users.

The easiest way to see what I mean is via an example.

```julia
julia> using Cassette: @context, @hook, Intercept, unwrap

# Define a new Cassette context called `PrintCtx`.
julia> @context PrintCtx

# This hook will be called every time a `PrintCtx` function is called.
# The `:::` is not a typo; it's Cassette's context dispatch syntax. The
# full syntax is `f::F:Ctx`, which can be read as `f of type F in context
# Ctx`. If a `T` is not provided, the type defaults to `Any` (just like
# normal type dispatch).
julia> @hook (f:::PrintCtx)(args...) =  println("calling ", unwrap(f), args)

# Define the best toy example Julia function ever.
julia> function rosenbrock(x::Vector{Float64})
           a = 1.0
           b = 100.0
           result = 0.0
           for i in 1:length(x)-1
               result += (a - x[i])^2 + b*(x[i+1] - x[i]^2)^2
           end
           return result
       end
rosenbrock (generic function with 1 method)

julia> Intercept(PrintCtx(rosenbrock))(rand(3))
calling arraylen([0.474659, 0.838978, 0.481891],)
calling UnitRange{Int64}(1, 2)
calling Int64(1,)
calling not_int(false,)
calling add_int(1, 1)
calling Core.arrayref([0.474659, 0.838978, 0.481891], 1)
calling mul_float(0.5253413404302651, 0.5253413404302651)
calling add_int(1, 1)
calling Core.arrayref([0.474659, 0.838978, 0.481891], 1)
calling Val{2}()
calling sub_float(0.8389779293459476, 0.22530084310453743)
calling Val{2}()
calling mul_float(100.0, 0.37659956617774715)
calling add_float(0.0, 37.935940141739785)
calling not_int(false,)
calling add_int(2, 1)
calling Core.arrayref([0.474659, 0.838978, 0.481891], 2)
calling mul_float(0.16102207065405238, 0.16102207065405238)
calling add_int(2, 1)
calling Core.arrayref([0.474659, 0.838978, 0.481891], 2)
calling Val{2}()
calling sub_float(0.4818909816759547, 0.7038839659296139)
calling Val{2}()
calling mul_float(100.0, 0.049280885057845385)
calling add_float(37.935940141739785, 4.954016613022257)
calling not_int(true,)
42.88995675476204
```

So, what actually happened here? Here's an overly-detailed, step-by-step breakdown:

---

We defined a new Cassette context called `PrintCtx` using the `@context` macro. This
macro merely defines a normal Julia `struct` with the name `PrintCtx`. To prove that, let's
expand the call to the `@context` macro. Don't get hung up on the tagging details here -
we'll cover that later. Here's the expanded code, with some manual clean-up for readability:

```julia
julia> @macroexpand @context PrintCtx
quote
    begin
        struct PrintCtx{T, F} <: Cassette.AbstractContext{T, F}
            tag::Cassette.Tag{T}
            func::F
            PrintCtx(tag::Cassette.Tag{T}, func::F) where {T, F} = new{T, F}(tag, func)
            PrintCtx(tag::Cassette.Tag{T}, func::Type{F}) where {T, F} = new{T, Type{F}}(tag, func)
            PrintCtx(tag::Cassette.Tag{T}, func::Cassette.AbstractContext) where {T} = error("cannot nest contexts without an Intercept barrier")
        end
        PrintCtx(f) = PrintCtx(Cassette.Tag(f, Val(:PrintCtx)), f)
        Cassette.wrap(ctx::PrintCtx, f::F) where {F} = PrintCtx(ctx.tag, f)
    end
end
```

---

Next, we defined what it means to call a function wrapped in `PrintCtx`. Despite the fancy
syntax, this is literally just overloading call for `PrintCtx` objects. Once again, we
can macro-expand the code to confirm this. I've also added an extra `TypeVar` `F` to
demonstrate how the `@contextual` macro transforms the triple-colon syntax:

```julia
julia> @macroexpand @hook (f::F|PrintCtx)(args...) where {F} =  println("calling ", unwrap(f), args)
```

---

Finally, we recursively intercepted all method calls within `rosenbrock`, and at the base cases -
called "primitives" in Cassette-lingo - we called our `PrintCtx` method, which logged and
called the underlying method. Note that, as a fallback, `Core` methods and unreflectable
methods are always considered primitives.

To illustrate what's actually going on, let's look at the lowered code for a normal
`rosenbrock` call and compare it to the lowered code for a call to
`Intercept(PrintCtx(rosenbrock))`. Once again, I've manually munged the output for
readability:

```julia
julia> @code_lowered rosenbrock(rand(3))
CodeInfo(:(begin
        a = 1.0
        b = 100.0
        result = 0.0
        SSAValue(0) = (Main.colon)(1, (Main.length)(x) - 1)
        #temp# = (Base.start)(SSAValue(0))
        10:
        unless !((Base.done)(SSAValue(0), #temp#)) goto 19
        SSAValue(1) = (Base.next)(SSAValue(0), #temp#)
        i = (Core.getfield)(SSAValue(1), 1)
        #temp# = (Core.getfield)(SSAValue(1), 2)
        result = result + ((Base.literal_pow)(Main.^, a - (Main.getindex)(x, i), ((Core.apply_type)(Base.Val, 2))()) + b * (Base.literal_pow)(Main.^, (Main.getindex)(x, i + 1) - (Base.literal_pow)(Main.^, (Main.getindex)(x, i), ((Core.apply_type)(Base.Val, 2))()), ((Core.apply_type)(Base.Val, 2))()))
        goto 10
        19:
        return result
    end))

julia> @code_lowered Intercept(PrintCtx(rosenbrock))(rand(3))
CodeInfo(:(begin
        nothing
        a = 1.0
        b = 100.0
        result = 0.0
        SSAValue(0) = ((Cassette.Intercepted)(#self#, Main.colon))(1, ((Cassette.Intercepted)(#self#, Main.length))(x) - 1)
        #temp# = ((Cassette.Intercepted)(#self#, Base.start))(SSAValue(0))
        10:
        unless ((Cassette.Intercepted)(#self#, Base.!))((Base.done)(SSAValue(0), #temp#)) goto 19
        SSAValue(1) = ((Cassette.Intercepted)(#self#, Base.next))(SSAValue(0), #temp#)
        i = (Core.getfield)(SSAValue(1), 1)
        #temp# = (Core.getfield)(SSAValue(1), 2)
        result = ((Cassette.Intercepted)(#self#, Main.+))(result, ((Cassette.Intercepted)(#self#, Base.literal_pow))(Main.^, a - ((Cassette.Intercepted)(#self#, Main.getindex))(x, i), ((Core.apply_type)(Base.Val, 2))()) + ((Cassette.Intercepted)(#self#, Main.*))(b, (Base.literal_pow)(Main.^, ((Cassette.Intercepted)(#self#, Main.-))((Main.getindex)(x, ((Cassette.Intercepted)(#self#, Main.+))(i, 1)), (Base.literal_pow)(Main.^, ((Cassette.Intercepted)(#self#, Main.getindex))(x, i), ((Cassette.Intercepted)(#self#, (Core.apply_type)(Base.Val, 2)))())), ((Cassette.Intercepted)(#self#, (Core.apply_type)(Base.Val, 2)))())))
        goto 10
        19:
        return result
    end))
```

## Cassette's Contextual Metadata Propagation Framework

TODO

## Cassette's Computation Graph Framework

TODO

## Why the name "Cassette"?

Because it enables you to "overdub" Julia "tapes" with new behaviors! :D
