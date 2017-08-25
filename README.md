# Cassette

## DISCLAIMER

Cassette is still in development. At any given time, the implementation might be ugly,
buggy, incomplete, slow, and/or untested. You may not be able to reproduce any examples
given in the README. Cassette relies on new reflection features and compiler performance
improvements (e.g. fixing JuliaLang/julia#5402) that will hopefully land in Julia 1.0.
Until an initial version of Cassette is released, I can't guarantee that Cassette's
`master` branch won't rely on some weird custom version of Julia.

Also note that whenever I show macro-expanded/lowered code in this README, I've cleaned up
the output for readability (e.g. removing metadata like line number info).

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

## Why "Cassette"?

Because the package enables you to "overdub" Julia "tapes" with new behaviors :D

## Cassette's Contextual Call Interceptor

First and foremost, "contextual call intercepting" is a phrase I just made up. If anybody
knows of any existing terms in the literature for Cassette's weird brand of execution
tracing, feel free to let me know.

Cassette can instrument your Julia code in order to intercept native Julia method calls as
they occur during program execution. Which calls are intercepted and what actually happens
when interception occurs are both defined with respect to a Cassette "context", which is
itself defined by Cassette users.

### Contextual Code Execution

The easiest way to understand Cassette's call interception is via an example:

```julia
julia> using Cassette: @context, @hook, Intercept, unwrap

julia> @context MyCtx

julia> @hook MyCtx @ctx(f)(args...) = println("calling ", unwrap(f), args)

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

julia> Intercept(MyCtx(rosenbrock))(rand(2))
calling rosenbrock([0.812348, 0.781836],)
calling length([0.812348, 0.781836],)
calling arraylen([0.812348, 0.781836],)
calling -(2, 1)
calling sub_int(2, 1)
calling colon(1, 1)
calling UnitRange{Int64}(1, 1)
calling start(1:1,)
calling oneunit(Int64,)
calling one(Int64,)
calling convert(Int64, 1)
calling Int64(1,)
calling +(1, 1)
calling add_int(1, 1)
calling oftype(2, 1)
calling typeof(2,)
calling convert(Int64, 1)
calling done(1:1, 1)
calling oftype(1, 1)
calling typeof(1,)
calling convert(Int64, 1)
calling oneunit(Int64,)
calling one(Int64,)
calling convert(Int64, 1)
calling Int64(1,)
calling +(1, 1)
calling add_int(1, 1)
calling ==(1, 2)
calling ===(1, 2)
calling !(false,)
calling not_int(false,)
calling next(1:1, 1)
calling convert(Int64, 1)
calling oneunit(Int64,)
calling one(Int64,)
calling convert(Int64, 1)
calling Int64(1,)
calling +(1, 1)
calling add_int(1, 1)
calling getindex([0.812348, 0.781836], 1)
calling Core.arrayref([0.812348, 0.781836], 1)
calling -(1.0, 0.8123482798451529)
calling sub_float(1.0, 0.8123482798451529)
calling Val{2}()
calling Base.literal_pow(^, 0.1876517201548471, Val{2}())
calling *(0.1876517201548471, 0.1876517201548471)
calling mul_float(0.1876517201548471, 0.1876517201548471)
calling +(1, 1)
calling add_int(1, 1)
calling getindex([0.812348, 0.781836], 2)
calling Core.arrayref([0.812348, 0.781836], 2)
calling getindex([0.812348, 0.781836], 1)
calling Core.arrayref([0.812348, 0.781836], 1)
calling Val{2}()
calling Base.literal_pow(^, 0.8123482798451529, Val{2}())
calling *(0.8123482798451529, 0.8123482798451529)
calling mul_float(0.8123482798451529, 0.8123482798451529)
calling -(0.781835663042431, 0.6599097277673789)
calling sub_float(0.781835663042431, 0.6599097277673789)
calling Val{2}()
calling Base.literal_pow(^, 0.12192593527505213, Val{2}())
calling *(0.12192593527505213, 0.12192593527505213)
calling mul_float(0.12192593527505213, 0.12192593527505213)
calling *(100.0, 0.014865933692696202)
calling mul_float(100.0, 0.014865933692696202)
calling +(0.03521316807707305, 1.4865933692696203)
calling add_float(0.03521316807707305, 1.4865933692696203)
calling +(0.0, 1.5218065373466934)
calling add_float(0.0, 1.5218065373466934)
calling done(1:1, 2)
calling oftype(2, 1)
calling typeof(2,)
calling convert(Int64, 1)
calling oneunit(Int64,)
calling one(Int64,)
calling convert(Int64, 1)
calling Int64(1,)
calling +(1, 1)
calling add_int(1, 1)
calling ==(2, 2)
calling ===(2, 2)
calling !(true,)
calling not_int(true,)
1.5218065373466934
```

So, what actually happened here? Here's an overly-detailed, step-by-step breakdown.

#### 1. We defined a context

We defined a new Cassette context called `MyCtx` using the `@context` macro. This
macro merely defines a normal Julia `struct` with the name `MyCtx`, and overloads a
couple of Cassette's internal methods with this new type. To prove that, let's expand the
call to the `@context` macro. Don't get hung up on the details here - I just want to prove
that nothing diabolical is going on. Here's the expanded code, with some manual clean-up
for readability:

```julia
julia> @macroexpand @context MyCtx
quote
    struct MyCtx{T, F} <: Cassette.CtxCall{:MyCtx, T, F}
        tag::Cassette.Tag{:MyCtx, T}
        func::F
        MyCtx(tag::Cassette.Tag{:MyCtx, T}, func::F) where {T, F} = new{T, F}(tag, func)
        MyCtx(tag::Cassette.Tag{:MyCtx, T}, func::Type{F}) where {T, F} = new{T, Type{F}}(tag, func)
        MyCtx(tag::Cassette.Tag{:MyCtx, T}, func::Cassette.CtxCall) where T = error("cannot nest contexts without an Intercept barrier")
    end
    MyCtx(f) = MyCtx(Cassette.Tag(Val{:MyCtx}(), f), f)
    Cassette._wrap(ctx::MyCtx, f::F) where F = MyCtx(ctx.tag, f)
    Cassette._hook(ctx::MyCtx, args...) = nothing
    Cassette._isprimitive(ctx::MyCtx, args...) = Val(false)
    (f::MyCtx{##TagTypeVar#778})(args...) where ##TagTypeVar#778 = Cassette.ctxcall(unwrap(f), f, args...)
end
```

This last line is particularly important - it's a fallback definition that describes how
to execute a function wrapped in `MyCtx`. It uses `Cassette.ctxcall`, which is a nifty
function we'll learn more about in the [Contextual Metadata Propagation](#contextual-metadata-propagation)
section. Briefly, `ctxcall(f, ctx::CtxCall, args...)` will call `f(unwrap(ctx, arg[1]), unwrap(ctx, arg[2]), ...)`,
where `unwrap(ctx, arg)` returns `arg` stripped of contextual metadata (if any was present).

#### 2. We defined a hook

Next, we defined a Cassette "hook" for `MyCtx` method calls via the `@hook` macro.
Cassette will call this hook every time it intercepts a method call. Once again, let's
see what's really going on by macro-expanding the code (and once again, I've cleaned
this up a little bit for readability):

```julia
julia> @macroexpand @hook MyCtx @ctx(f)(args...) = println("calling ", unwrap(f), args)
quote
    function Cassette._hook(f::MyCtx{##TagTypeVar#779}, args...) where ##TagTypeVar#779
        println("calling ", unwrap(f), args)
    end
end
```

As you can see, this macro overrides the `Cassette._hook` method originally defined by the
`@context` macro. You may also notice that the arguments are also available for dispatch;
let's leverage this to add more specialized hooks:

```julia
julia> @hook MyCtx @ctx(f)(args::Number...) = println("OH WOW, NUMERIC ARGUMENTS! ", unwrap(f), args)

julia> Intercept(MyCtx(rosenbrock))(rand(2))
calling rosenbrock([0.833447, 0.798788],)
calling length([0.833447, 0.798788],)
calling arraylen([0.833447, 0.798788],)
OH WOW, NUMERIC ARGUMENTS! -(2, 1)
OH WOW, NUMERIC ARGUMENTS! sub_int(2, 1)
OH WOW, NUMERIC ARGUMENTS! colon(1, 1)
OH WOW, NUMERIC ARGUMENTS! UnitRange{Int64}(1, 1)
calling start(1:1,)
calling oneunit(Int64,)
calling one(Int64,)
calling convert(Int64, 1)
OH WOW, NUMERIC ARGUMENTS! Int64(1,)
OH WOW, NUMERIC ARGUMENTS! +(1, 1)
OH WOW, NUMERIC ARGUMENTS! add_int(1, 1)
OH WOW, NUMERIC ARGUMENTS! oftype(2, 1)
OH WOW, NUMERIC ARGUMENTS! typeof(2,)
calling convert(Int64, 1)
calling done(1:1, 1)
OH WOW, NUMERIC ARGUMENTS! oftype(1, 1)
OH WOW, NUMERIC ARGUMENTS! typeof(1,)
calling convert(Int64, 1)
calling oneunit(Int64,)
calling one(Int64,)
calling convert(Int64, 1)
OH WOW, NUMERIC ARGUMENTS! Int64(1,)
OH WOW, NUMERIC ARGUMENTS! +(1, 1)
OH WOW, NUMERIC ARGUMENTS! add_int(1, 1)
OH WOW, NUMERIC ARGUMENTS! ==(1, 2)
OH WOW, NUMERIC ARGUMENTS! ===(1, 2)
OH WOW, NUMERIC ARGUMENTS! !(false,)
OH WOW, NUMERIC ARGUMENTS! not_int(false,)
calling next(1:1, 1)
calling convert(Int64, 1)
calling oneunit(Int64,)
calling one(Int64,)
calling convert(Int64, 1)
OH WOW, NUMERIC ARGUMENTS! Int64(1,)
OH WOW, NUMERIC ARGUMENTS! +(1, 1)
OH WOW, NUMERIC ARGUMENTS! add_int(1, 1)
calling getindex([0.833447, 0.798788], 1)
calling Core.arrayref([0.833447, 0.798788], 1)
OH WOW, NUMERIC ARGUMENTS! -(1.0, 0.8334467955851765)
OH WOW, NUMERIC ARGUMENTS! sub_float(1.0, 0.8334467955851765)
OH WOW, NUMERIC ARGUMENTS! Val{2}()
calling Base.literal_pow(^, 0.16655320441482346, Val{2}())
OH WOW, NUMERIC ARGUMENTS! *(0.16655320441482346, 0.16655320441482346)
OH WOW, NUMERIC ARGUMENTS! mul_float(0.16655320441482346, 0.16655320441482346)
OH WOW, NUMERIC ARGUMENTS! +(1, 1)
OH WOW, NUMERIC ARGUMENTS! add_int(1, 1)
calling getindex([0.833447, 0.798788], 2)
calling Core.arrayref([0.833447, 0.798788], 2)
calling getindex([0.833447, 0.798788], 1)
calling Core.arrayref([0.833447, 0.798788], 1)
OH WOW, NUMERIC ARGUMENTS! Val{2}()
calling Base.literal_pow(^, 0.8334467955851765, Val{2}())
OH WOW, NUMERIC ARGUMENTS! *(0.8334467955851765, 0.8334467955851765)
OH WOW, NUMERIC ARGUMENTS! mul_float(0.8334467955851765, 0.8334467955851765)
OH WOW, NUMERIC ARGUMENTS! -(0.7987882654939731, 0.694633561071199)
OH WOW, NUMERIC ARGUMENTS! sub_float(0.7987882654939731, 0.694633561071199)
OH WOW, NUMERIC ARGUMENTS! Val{2}()
calling Base.literal_pow(^, 0.10415470442277408, Val{2}())
OH WOW, NUMERIC ARGUMENTS! *(0.10415470442277408, 0.10415470442277408)
OH WOW, NUMERIC ARGUMENTS! mul_float(0.10415470442277408, 0.10415470442277408)
OH WOW, NUMERIC ARGUMENTS! *(100.0, 0.010848202453395435)
OH WOW, NUMERIC ARGUMENTS! mul_float(100.0, 0.010848202453395435)
OH WOW, NUMERIC ARGUMENTS! +(0.02773996990084597, 1.0848202453395435)
OH WOW, NUMERIC ARGUMENTS! add_float(0.02773996990084597, 1.0848202453395435)
OH WOW, NUMERIC ARGUMENTS! +(0.0, 1.1125602152403895)
OH WOW, NUMERIC ARGUMENTS! add_float(0.0, 1.1125602152403895)
calling done(1:1, 2)
OH WOW, NUMERIC ARGUMENTS! oftype(2, 1)
OH WOW, NUMERIC ARGUMENTS! typeof(2,)
calling convert(Int64, 1)
calling oneunit(Int64,)
calling one(Int64,)
calling convert(Int64, 1)
OH WOW, NUMERIC ARGUMENTS! Int64(1,)
OH WOW, NUMERIC ARGUMENTS! +(1, 1)
OH WOW, NUMERIC ARGUMENTS! add_int(1, 1)
OH WOW, NUMERIC ARGUMENTS! ==(2, 2)
OH WOW, NUMERIC ARGUMENTS! ===(2, 2)
OH WOW, NUMERIC ARGUMENTS! !(true,)
OH WOW, NUMERIC ARGUMENTS! not_int(true,)
1.1125602152403895
```

#### 3. We contextually executed some code

Finally, we recursively intercepted all method calls within `rosenbrock`. At the base cases
- called "primitives" in Cassette-lingo - we called the `(f::MyCtx)(args...)` method
automatically defined by `@context`. Note that, as a fallback, Cassette always considers
`Core` methods and unreflectable methods primitives.

To illustrate what's actually going on, let's look at the lowered code for a normal `rosenbrock`
call and compare it to the lowered code for a call to `Cassette.Enter(MyCtx(rosenbrock))`, where `Cassette.Enter(MyCtx(f))(args...)` is what `Intercept(MyCtx(f))(args...)` calls if `MyCtx(f)`
is not a primitive.

```julia
julia> @code_lowered rosenbrock(rand(2))
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

julia> @code_lowered Cassette.Enter(MyCtx(rosenbrock))(rand(2))
CodeInfo(:(begin
        nothing
        a = 1.0
        b = 100.0
        result = 0.0
        SSAValue(0) = ((Cassette.Intercept)(#self#, Main.colon))(1, ((Cassette.Intercept)(#self#, Main.-))(((Cassette.Intercept)(#self#, Main.length))(x), 1))
        #temp# = ((Cassette.Intercept)(#self#, Base.start))(SSAValue(0))
        10:
        unless ((Cassette.Intercept)(#self#, Base.!))(((Cassette.Intercept)(#self#, Base.done))(SSAValue(0), #temp#)) goto 19
        SSAValue(1) = ((Cassette.Intercept)(#self#, Base.next))(SSAValue(0), #temp#)
        i = (Core.getfield)(SSAValue(1), 1)
        #temp# = (Core.getfield)(SSAValue(1), 2)
        result = ((Cassette.Intercept)(#self#, Main.+))(result, ((Cassette.Intercept)(#self#, Main.+))(((Cassette.Intercept)(#self#, Base.literal_pow))(Main.^, ((Cassette.Intercept)(#self#, Main.-))(a, ((Cassette.Intercept)(#self#, Main.getindex))(x, i)), ((Cassette.Intercept)(#self#, (Core.apply_type)(Base.Val, 2)))()), ((Cassette.Intercept)(#self#, Main.*))(b, ((Cassette.Intercept)(#self#, Base.literal_pow))(Main.^, ((Cassette.Intercept)(#self#, Main.-))(((Cassette.Intercept)(#self#, Main.getindex))(x, ((Cassette.Intercept)(#self#, Main.+))(i, 1)), ((Cassette.Intercept)(#self#, Base.literal_pow))(Main.^, ((Cassette.Intercept)(#self#, Main.getindex))(x, i), ((Cassette.Intercept)(#self#, (Core.apply_type)(Base.Val, 2)))())), ((Cassette.Intercept)(#self#, (Core.apply_type)(Base.Val, 2)))()))))
        goto 10
        19:
        return result
    end))
```

The lowered code for `Cassette.Enter(MyCtx(rosenbrock))` is the same as the lowered code for
`rosenbrock`, but every callable object that is being called has been wrapped in the `Intercept`
type, which can then be called instead. Internally, the definitions for `Enter` and `Intercept`
look similar to the following:

```julia
struct Enter{C<:CtxCall}
    call::C
end

@generated function (::Enter)(args...)
    # I'm not going to show this because it's pretty complicated, but
    # what this does is run the underlying contextualized method call
    # with all subcalls wrapped in `Intercept`
end

struct Intercept{C<:CtxCall}
    call::C
end

Intercept(e::Enter, f) = Intercept(_wrap(e.call, f))

Intercept(e::Enter) = Intercept(e.call)

function (i::Intercept)(args...)
    hook(i.call, args...)
    return execute(i, args...)
end

execute(i::Intercept, args...) = execute(isprimitive(i.call, args...), i, args...)
execute(::Val{true}, i::Intercept, args...) = i.call(args...)
execute(::Val{false}, i::Intercept, args...) = Enter(i.call)(args...)
```

### Contextual primitives

Earlier, I mentioned primitives:

> At the base cases - called "primitives" in Cassette-lingo - we called the `(f::MyCtx)(args...)`
method automatically defined by `@context`. Note that, as a fallback, Cassette always considers
`Core` methods and unreflectable methods primitives.

Unlike hooks, which can only really be used to generate side-effects if the original program is
otherwise pure, new primitive definitions can alter a program's execution behavior.

Note that there are really two separate mechanisms at work here. The first is defining what
it means for a given primitive to execute in a given context. The second is defining which
methods actually count as primitives; in other words, which methods Cassette shouldn't
recursively apply `Enter` to, but instead invoke the primitive execution method on.

For the first mechanism, Cassette provides the `@contextual` macro, which wraps a method
definition in a similar manner to `@hook`. For the second mechanism, Cassette provides the
`@isprimitive` macro, which takes a method signature and registers it as a primitive method
call for the given context. For convenience, Cassette provides a `@primitive` macro which
simultaneously passes the given method definition to `@contextual` and marks its signature
with `@primitive`.

For the sake of example, let's use these macros to wreak some havoc by redirecting all
intercepted `sin` calls to `cos` calls:

```julia
julia> using Cassette: @contextual, @isprimitive, @primitive

# Define the behavior of `sin` in `MyCtx`. Note that the syntax
# accepted by `@ctx` is the generic call overloading syntax, not
# the `Function` specific sugar; you have to do `typeof(sin)`.
julia> @contextual MyCtx @ctx(f::typeof(sin))(x) = cos(x)

# Mark the method with this signature as a Cassette primitive.
# If we don't do this, our `@contextual` definition will never
# get called, because Cassette will just recurse into `sin` calls
# rather than treating them as primitives!
julia> @isprimitive MyCtx @ctx(f::typeof(sin))(x)

julia> Intercept(MyCtx(sin))(1.0) == cos(1.0)
OH WOW, NUMERIC ARGUMENTS! sin(1.0,)
true

julia> Intercept(MyCtx(x -> sin(x) + cos(x)))(1.0) == 2 * cos(1.0)
OH WOW, NUMERIC ARGUMENTS! #1(1.0,)
OH WOW, NUMERIC ARGUMENTS! sin(1.0,)
OH WOW, NUMERIC ARGUMENTS! cos(1.0,)
calling Base.cconvert(Float64, 1.0)
calling convert(Float64, 1.0)
calling Base.unsafe_convert(Float64, 1.0)
OH WOW, NUMERIC ARGUMENTS! Base.Math.nan_dom_err(0.5403023058681398, 1.0)
OH WOW, NUMERIC ARGUMENTS! isnan(0.5403023058681398,)
OH WOW, NUMERIC ARGUMENTS! !=(0.5403023058681398, 0.5403023058681398)
OH WOW, NUMERIC ARGUMENTS! ne_float(0.5403023058681398, 0.5403023058681398)
OH WOW, NUMERIC ARGUMENTS! isnan(1.0,)
OH WOW, NUMERIC ARGUMENTS! !=(1.0, 1.0)
OH WOW, NUMERIC ARGUMENTS! ne_float(1.0, 1.0)
OH WOW, NUMERIC ARGUMENTS! !(false,)
OH WOW, NUMERIC ARGUMENTS! not_int(false,)
OH WOW, NUMERIC ARGUMENTS! &(false, true)
OH WOW, NUMERIC ARGUMENTS! and_int(false, true)
OH WOW, NUMERIC ARGUMENTS! +(0.5403023058681398, 0.5403023058681398)
OH WOW, NUMERIC ARGUMENTS! add_float(0.5403023058681398, 0.5403023058681398)
true
```

Note that we used the same signature for our `@contextual` and `@isprimitive` calls. In this
case, we could've just used the `@primitive` macro to define the execution and mark it as a
primitive at the same time, like so:

```julia
# does @contextual and @isprimitive at the same time
@primitive MyCtx @ctx(f::typeof(sin))(x) = cos(x)
```

## Cassette's Contextual Metadata Propagation Framework

TODO

## Cassette's Computation Graph Framework

TODO
