
<p align="center">
<img width="700px" src="https://cdn.rawgit.com/jrevels/Cassette.jl/master/docs/cassette-logo.svg"/>
</p>

## DISCLAIMERS

- Cassette targets Julia package developers, not end-users. Downstream developers are expected to have a solid understanding of Julia's type system, metaprogramming facilities, and dispatch mechanism.

- Cassette is still in development. At any given time, the implementation might be ugly, buggy, incomplete, slow, and/or untested. Cassette relies on new reflection features and compiler performance improvements that will hopefully land in Julia 1.x. Until an initial version of Cassette is released, I can't guarantee that Cassette's `master` branch won't rely on some weird custom version of Julia.

- Cassette is currently poorly documented. I hope for this to change in the following months.

- Cassette was last updated against Julia commit: f1b23284bb881023d3581053640f025f071a7f9c

## Overview

Cassette is a Julia package that provides a mechanism for dynamically injecting code transformation passes into Julia’s just-in-time (JIT) compilation cycle, enabling post hoc analysis and modification of "Cassette-unaware" Julia programs without requiring manual source annotation or refactoring of the target code.

Cassette's API is built upon the notion of user-definable execution contexts, which are represented as normal Julia types. Using Cassette, any normal Julia function can be invoked within an execution context via a process called "overdubbing". Cassette provides users with multiple ways to define the execution of code overdubbed with their context type. For example, Cassette's lowest level interface allows users to apply Julia functions of the form `mypass(::Type{MyContext}, signature::Type{Tuple{...}}, method_body::CodeInfo)::CodeInfo` to every method body encountered during overdubbed execution at compile-time. Cassette also provides a higher-level, dispatch-based interface that allows users to safely and quickly overload existing Julia methods with context-specific behaviors without ever needing to handle Julia’s IR directly.

On top of contextual pass injection and contextual dispatch, Cassette implements (or, rather, [will implement](https://github.com/jrevels/Cassette.jl/pull/46)) a system for tagging underlying program values with contextual metadata (e.g. a numerical value's derivative) and automatically propagating this metadata through both structural and dispatch type constraints within the underlying program.

Downstream applications for Cassette include dynamic code analysis (e.g. profiling, rr-style debugging, etc.), JIT compilation to new hardware/software backends, automatic differentiation, interval constraint programming, automatic parallelization/rescheduling, automatic memoization, lightweight multistage programming, graph extraction, and more.

If you'd like to know more, please see [this Youtube video of a recent talk at MIT](https://www.youtube.com/watch?v=lyX-isPDS2M) that dives into the details of Cassette's design and implementation.
