# Cassette

## DISCLAIMER

Cassette is still in development. At any given time, the implementation might be ugly,
buggy, incomplete, slow, and/or untested. You might not be able to reproduce the examples
given in the README.

Cassette relies on new reflection features and compiler performance improvements that will
hopefully land in Julia 1.0. Until an initial version of Cassette is released, I can't
guarantee that Cassette's `master` branch won't rely on some weird custom version of Julia.

Also note that whenever I show macro-expanded/lowered code in this README, I've cleaned up
the output for readability (e.g. removing metadata like line number info).

At the time of writing, all of the examples in this README work using Julia commit aa3d2be
(Version 0.7.0-DEV.1485) and Cassette commit 5de482e.

## Table of Contents

- [What is Cassette?](#what-is-cassette)
- [Why "Cassette"?](#why-cassette)
- [Cassette's Contextual Code Execution Framework](#cassettes-contextual-code-execution-framework)
- [Cassette's Contextual Metadata Propagation Framework](#cassettes-contextual-metadata-propagation-framework)
- [Cassette's Computation Graph Framework](#cassettes-computation-graph-framework)
- [Similarities to Aspect-Oriented Programming](#similarities-to-aspect-oriented-programming)

## What is Cassette?

[top](#cassette)

Cassette is a Julia package that provides...

- ...a *contextual code execution* framework.
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

[top](#cassette)

Because the package enables you to "overdub" Julia "tapes" with new behaviors :D

## What is Cassette's current development status?

[top](#cassette)

First, see the disclaimer above.

I'll update this section when reasonable, but I don't guarantee this is up to date.

Cassette is still in prototypical development, so in this section, I define "completeness"
as a complete prototype that is able to support an automatic differentiation implementation.
Once such a prototype exists (hopefully in Fall 2017), I will encourage interested
developers to play around with the prototype and provide feedback. Using the feedback, I'll
take a second pass at Cassette's design, get tests/benchmarks in place, etc., and finally
release the package (planned Winter 2017/2018, assuming Julia's compiler will be sufficient
by then).

- Contextual Code Execution Framework: Design is complete. Besides some significant
details that require more compiler improvements, implementation is complete. Performance
right now is pretty bad due to fixable compiler issues (specifically JuliaLang/julia#5402).
Theoretically, once these bugs have been fixed, Cassette overhead should be quite low
(<5% of original program runtime). I have been testing this by measuring the performance of
Cassette code rewritten to avoid the compiler performance problems (e.g. removing all
splats), which makes the code pretty horrible and un-generic, but should be representative
of post-fix performance.

- Contextual Metadata Propagation Framework: Design is mostly complete, and for the
design areas that are complete, implementation is mostly complete (though I'm still actively
experimenting with alternatives). The central remaining problem has to do with propagating
metadata through mutable structures. At this point, I expect this to take several weeks.
Note that the same compiler performance problems that affect the call interceptor will
affect metadata propagation.

- Computation Graph Framework: I had a prototype of the graph framework running at JuliaCon
2017, but I threw the prototype away a couple months ago so that I could focus on nailing
down Cassette's call interceptor and metadata propagation framework without worrying about
updating any coupled graph code. Once those implementations are complete, I will redesign
and rebuild the graph framework on top of them. I expect this to take a 2-3 weeks of
development time (longer in reality, since I have other responsibilities).

## Cassette's Contextual Code Execution Framework

[top](#cassette)

Cassette can instrument your Julia code in order to intercept native Julia method calls as
they occur during program execution. Which calls are intercepted and what actually happens
when interception occurs are both defined with respect to Cassette "contexts", which are
defined by Cassette users.

### Contextual Code Execution

The easiest way to understand Cassette's code execution is via an example:

```julia
julia> using Cassette: @context, @hook, @execute

julia> @context MyCtx

julia> @hook MyCtx f(args...) = println("calling ", f, args)

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

julia> @execute MyCtx rosenbrock(rand(2))
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

We defined a new Cassette context called `MyCtx` using the `@context` macro. This macro
merely defines a normal Julia `struct` with the name `MyCtx`, and overloads a couple of
Cassette's internal methods with this new type. If we expand this macrocall, it'd look
something like the following (don't get too hung up on the details here - I just want to
show that nothing diabolical is going on):

```julia
struct MyCtx{T} <: Cassette.Context{:MyCtx,T}
    tag::Cassette.Tag{T}
end
MyCtx(x) = MyCtx(Cassette.Tag(x))
Cassette.@hook MyCtx f(args...) = nothing
Cassette.@execution ctx::MyCtx f(args...) = Cassette.escapecall(f, ctx, args...)
```

This last line is particularly important - it's a fallback definition that describes how
to execute a function within in `MyCtx`. It uses `Cassette.escapecall`, which is a nifty
function we'll learn more about in the [Contextual Metadata Propagation Framework](#cassettes-contextual-metadata-propagation-framework)
section. Briefly, `escapecall(f, ctx, args...)` will strip `f` and `args` of any contextual
metadata w.r.t. `ctx`, and then call `f(args...)`.

#### 2. We defined a hook

Next, we defined a Cassette "hook" for `MyCtx` method calls via the `@hook` macro. Cassette
will call this hook every time it intercepts a method call. Once again, let's see what's
really going on by macro-expanding the code (I've cleaned this up a little bit for
readability):

```julia
julia> @macroexpand @hook MyCtx f(args...) = println("calling ", f, args)
quote
    function Cassette._hook(::##ContextTypeVar#798, f, args...) where ##ContextTypeVar#798 <: MyCtx
        println("calling ", f, args)
    end
end
```

As you can see, this macro overrides the `Cassette._hook` method originally defined by the
`@context` macro. You may also notice that the arguments are also available for dispatch;
let's leverage this to add a more specialized hook:

```julia
julia> @hook MyCtx f(args::Number...) = println("OH WOW, NUMERIC ARGUMENTS! ", f, args)

julia> @execute MyCtx rosenbrock(rand(2))
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

Finally, we recursively intercepted all method calls within `rosenbrock`. At the base
cases - called "primitives" in Cassette-lingo - we called the `@execution` method defined
by `@context`. Note that, as a fallback, Cassette always considers `Core` methods and
unreflectable methods to be primitives.

Let's macroexpand our `@execute` call to get a better idea of what's actually going on:

```julia
julia> @macroexpand @execute MyCtx rosenbrock(rand(2))
:((Cassette.Execute(MyCtx(rosenbrock), rosenbrock))(rand(2)))
```

For clarity, let's break this into two parts:

```julia
# construct callable `Execute` struct
julia> f = Cassette.Execute(MyCtx(rosenbrock), rosenbrock)
Cassette.Execute{MyCtx{0xa7c6d08bc9254c7f},#rosenbrock}(MyCtx{12089579548516371583}(), rosenbrock)

# call `f` on our argument
julia> f(rand(2))
calling rosenbrock([0.336142, 0.293493],)
calling length([0.336142, 0.293493],)
calling arraylen([0.336142, 0.293493],)
OH WOW, NUMERIC ARGUMENTS! -(2, 1)
⋮
```

If everything is inlined aggressively, then the lowered code for `f(rand(2))` will be nearly
the same as the lowered code for `rosenbrock(rand(2))`, with the key difference that every
method call within the program has been wrapped in an `Execute`. Internally, the
definition of `Execute` looks similar to the following (this is *not* the real code, but
hopefully illuminates the kind of thing going on under the hood):

```julia
struct Intercept{C<:Context,F}
    context::C
    func::F
end

@generated function (i::Intercept)(args...)
    # I'm not going to show this because it's pretty complicated, but
    # what this does is run the underlying contextualized method call
    # with all subcalls wrapped in `Execute`
end

struct Execute{C<:Context,F}
    context::C
    func::F
end

Execute(i::Intercept, f) = Execute(i.context, f)

hook(e::Execute, args...) = _hook(e.context, e.func, args...)

isprimitive(e::Execute, args...) = _isprimitive(e.context, e.func, args...)

execute(e::Execute, args...) = execute(isprimitive(e, args...), e, args...)
execute(::Val{true}, e::Execute, args...) = _execution(e.context, e.func, args...)
execute(::Val{false}, e::Execute, args...) = Intercept(e.context, e.func)(args...)

(e::Execute)(args...) = (hook(e, args...); execute(e, args...))
```

To further illustrate the execution mechanism, let's look at the lowered code for a normal
`rosenbrock` call and compare it to the lowered code for a call to an `Intercept` struct:

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

julia> f = Cassette.Intercept(MyCtx(rosenbrock), rosenbrock)
Cassette.Intercept{MyCtx{0xa7c6d08bc9254c7f},#rosenbrock}(MyCtx{12089579548516371583}(), rosenbrock)

julia> @code_lowered f(rand(2))
CodeInfo(:(begin
        nothing
        a = 1.0
        b = 100.0
        result = 0.0
        SSAValue(0) = ((Cassette.Execute)(#self#, Main.colon))(1, ((Cassette.Execute)(#self#, Main.-))(((Cassette.Execute)(#self#, Main.length))(x), 1))
        #temp# = ((Cassette.Execute)(#self#, Base.start))(SSAValue(0))
        10:
        unless ((Cassette.Execute)(#self#, Base.!))(((Cassette.Execute)(#self#, Base.done))(SSAValue(0), #temp#)) goto 19
        SSAValue(1) = ((Cassette.Execute)(#self#, Base.next))(SSAValue(0), #temp#)
        i = (Core.getfield)(SSAValue(1), 1)
        #temp# = (Core.getfield)(SSAValue(1), 2)
        result = ((Cassette.Execute)(#self#, Main.+))(result, ((Cassette.Execute)(#self#, Main.+))(((Cassette.Execute)(#self#, Base.literal_pow))(Main.^, ((Cassette.Execute)(#self#, Main.-))(a, ((Cassette.Execute)(#self#, Main.getindex))(x, i)), ((Cassette.Execute)(#self#, (Core.apply_type)(Base.Val, 2)))()), ((Cassette.Execute)(#self#, Main.*))(b, ((Cassette.Execute)(#self#, Base.literal_pow))(Main.^, ((Cassette.Execute)(#self#, Main.-))(((Cassette.Execute)(#self#, Main.getindex))(x, ((Cassette.Execute)(#self#, Main.+))(i, 1)), ((Cassette.Execute)(#self#, Base.literal_pow))(Main.^, ((Cassette.Execute)(#self#, Main.getindex))(x, i), ((Cassette.Execute)(#self#, (Core.apply_type)(Base.Val, 2)))())), ((Cassette.Execute)(#self#, (Core.apply_type)(Base.Val, 2)))()))))
        goto 10
        19:
        return result
    end))
```

### Contextual primitives

Earlier, I mentioned primitives:

> At the base cases - called "primitives" in Cassette-lingo - we called the `(f::MyCtx)(args...)`
method automatically defined by `@context`. Note that, as a fallback, Cassette always considers
`Core` methods and unreflectable methods to be primitives.

Unlike hooks, new primitive definitions can actually alter an otherwise pure program's
execution behavior.

There are two separate mechanisms that encompass Cassette's notion of a "primitive". The
first mechanism is the definition of a given primitive's execution in a given context. The
second is a predicate definition that specifies whether a given method is counts as a
primitive in a given context. Cassette uses this second mechanism to decide when to
invoke the first mechanism, or to otherwise recursively apply `Intercept` to a method call.

For the first mechanism, Cassette provides the `@execution` macro, which wraps a method
definition in a similar manner to `@hook`. For the second mechanism, Cassette provides the
`@isprimitive` macro, which takes a method signature and registers it as a primitive method
call for the given context. For convenience, Cassette provides a `@primitive` macro which
simultaneously passes the given method definition to `@execution` and marks its signature
with `@isprimitive`.

For the sake of example, let's use these macros to wreak some havoc by redirecting all
intercepted `sin` calls to `cos` calls:

```julia
julia> using Cassette: @execution, @isprimitive, @primitive

# Define the behavior of `sin` in `MyCtx`. Note that Cassette's
# macros only accept the syntax for generic call overloading,
# not the `<:Function` specific sugar; thus, you have to use
# `(::typeof(sin))(...)` instead of `Base.sin(...)`.
julia> @execution MyCtx (::typeof(sin))(x) = cos(x)

# Mark the method with this signature as a Cassette primitive.
# If we don't do this, our `@execution` definition will never
# get called, because Cassette will just recurse into `sin`
# calls rather than treating them as primitives!
julia> @isprimitive MyCtx (::typeof(sin))(x)

julia> @execute(MyCtx, sin(1.0)) == cos(1.0)
OH WOW, NUMERIC ARGUMENTS! sin(1.0,)
true

julia> @execute(MyCtx, (x -> sin(x) + cos(x))(1.0)) == 2 * cos(1.0)
OH WOW, NUMERIC ARGUMENTS! #6(1.0,)
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

Note that we used the same signature for our `@execution` and `@isprimitive` calls. In this
case, we could've just used the `@primitive` macro to define the execution and mark it as a
primitive at the same time, like so:

```julia
# does @execution and @isprimitive at the same time
@primitive MyCtx (::typeof(sin))(x) = cos(x)
```

## Cassette's Contextual Metadata Propagation Framework

[top](#cassette)

Description coming soon!

## Cassette's Computation Graph Framework

[top](#cassette)

Description coming soon!

## Similarities to Aspect-Oriented Programming

[top](#cassette)

Cassette shares many concepts in common with [aspect-oriented programming (AOP)](https://en.wikipedia.org/wiki/Aspect-oriented_programming).

Cassette's code execution framework is similar to an aspect weaver, except that the
"weaving" is method-local, based on operator-overloading, occurs at runtime, and is JIT
compiled. Cassette's call hooks can be used to provide at AOP-style "advice", where
the pointcut is specified via Julia's multiple dispatch mechanism.

Unlike AOP, the motivation behind Cassette isn't to provide a paradigm for modularizing
cross-cutting concerns. In some sense, Cassette could facilitate such a paradigm, but only
as a side-effect of being able to intercept and alter the execution of native Julia code.
