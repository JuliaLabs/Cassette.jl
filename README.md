# Cassette

## DISCLAIMER

Cassette is still in development. At any given time, the implementation might be ugly,
buggy, incomplete, slow, and/or untested. Cassette relies on new reflection features and
compiler performance improvements that will hopefully land in Julia 1.0; until an initial
version of Cassette is released, I can't guarantee that Cassette's `master` branch won't
rely on some weird custom version of Julia.

## Intro

Cassette is a Julia package that provides...

- ...a *contextual call interceptor*.
- ...a *contextual metadata propagation* framework.
- ...a *dynamic computational graph* implementation.
- ...a *static computational graph* implementation.

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

Cassette can instrument your Julia code in order to dynamically intercept native Julia
method calls as they occur during program execution. Which calls are intercepted and
what actually happens when interception occurs are both defined with respect to a
Cassette "context", which is itself defined by Cassette users.

The easiest way to see what I mean is via an example.

```julia
julia> using Cassette: @context, Trace, unwrap

julia> @context PrintCtx

# define what it means to call a function wrapped in `PrintCtx`
julia> function (ctx::PrintCtx)(args...)
           f = unwrap(ctx)
           println("calling ", f, args)
           return f(args...)
       end

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

julia> Trace(PrintCtx(rosenbrock))(rand(3))
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



## Cassette's Computation Graph Framework

TODO

## Why the name "Cassette"?

Because it enables you to "overdub" Julia "tapes" with new behaviors!
