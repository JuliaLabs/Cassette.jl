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

julia> @context PrintCtx

julia> @hook PrintCtx @ctx(f)(args...) =  println("calling ", unwrap(f), args)

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
calling length([0.792214, 0.757227, 0.825464],)
calling arraylen([0.792214, 0.757227, 0.825464],)
calling colon(1, 2)
calling UnitRange{Int64}(1, 2)
calling start(1:2,)
calling oneunit(Int64,)
calling Int64(1,)
calling oftype(2, 1)
calling convert(Int64, 1)
calling !(false,)
calling not_int(false,)
calling next(1:2, 1)
calling convert(Int64, 1)
calling +(1, 1)
calling add_int(1, 1)
calling getindex([0.792214, 0.757227, 0.825464], 1)
calling Core.arrayref([0.792214, 0.757227, 0.825464], 1)
calling Base.literal_pow(^, 0.20778562536661283, Val{2}())
calling *(0.20778562536661283, 0.20778562536661283)
calling mul_float(0.20778562536661283, 0.20778562536661283)
calling +(1, 1)
calling add_int(1, 1)
calling getindex([0.792214, 0.757227, 0.825464], 1)
calling Core.arrayref([0.792214, 0.757227, 0.825464], 1)
calling Val{2}()
calling -(0.7572270252068938, 0.6276036153757687)
calling sub_float(0.7572270252068938, 0.6276036153757687)
calling Val{2}()
calling *(100.0, 0.016802228376247813)
calling mul_float(100.0, 0.016802228376247813)
calling +(0.0, 1.7233977037337755)
calling add_float(0.0, 1.7233977037337755)
calling !(false,)
calling not_int(false,)
calling next(1:2, 2)
calling convert(Int64, 2)
calling +(2, 1)
calling add_int(2, 1)
calling getindex([0.792214, 0.757227, 0.825464], 2)
calling Core.arrayref([0.792214, 0.757227, 0.825464], 2)
calling Base.literal_pow(^, 0.24277297479310622, Val{2}())
calling *(0.24277297479310622, 0.24277297479310622)
calling mul_float(0.24277297479310622, 0.24277297479310622)
calling +(2, 1)
calling add_int(2, 1)
calling getindex([0.792214, 0.757227, 0.825464], 2)
calling Core.arrayref([0.792214, 0.757227, 0.825464], 2)
calling Val{2}()
calling -(0.8254637584336366, 0.5733927677036817)
calling sub_float(0.8254637584336366, 0.5733927677036817)
calling Val{2}()
calling *(100.0, 0.063539784367581)
calling mul_float(100.0, 0.063539784367581)
calling +(1.7233977037337755, 6.412917154047994)
calling add_float(1.7233977037337755, 6.412917154047994)
calling !(true,)
calling not_int(true,)
8.13631485778177
```

<!-- So, what actually happened here? Here's an overly-detailed, step-by-step breakdown:

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
        Cassette._wrap(ctx::PrintCtx, f::F) where {F} = PrintCtx(ctx.tag, f)
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
``` -->

## Cassette's Contextual Metadata Propagation Framework

TODO

## Cassette's Computation Graph Framework

TODO

## Why the name "Cassette"?

Because it enables you to "overdub" Julia "tapes" with new behaviors! :D
