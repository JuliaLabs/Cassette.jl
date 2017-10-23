# Cassette

Cassette is a Julia package that provides a just-in-time (JIT) IR pass injection mechanism,
or **overdubbing mechanism**, that can be used to interleave external code transformations
with Julia's normal JIT-compilation cycle. As part of this overdubbing mechanism, Cassette
provides a **contextual execution framework** for the Julia language via the implementation
of **contextual dispatch** and **contextual metadata propagation**. This framework enables
users to define new "contexts" that overlay normal Julia methods that can then be dispatched
on using Julia's existing operator-overloading syntax. Using its overdubbing mechanism,
Cassette can equip context-unaware Julia code with context-specific behaviors, enabling
granular method interception and metadata propagation within "black-box" user code.

Downstream applications for Cassette include automatic differentiation, interval constraint
programming, dynamic code analysis (e.g. profiling, `rr`-style debugging, etc.), JIT
transpilation to GPU backends, automatic parallelization/rescheduling, memoization,
high-level automated memory management and code fuzzing.

For a more complete description of Cassette, the problems it solves, and the mechanisms
that enable those solutions, see Cassette's [design document](docs/design.md).

## DISCLAIMER

Cassette is still in development. At any given time, the implementation might be ugly,
buggy, incomplete, slow, and/or untested. You might not be able to reproduce the examples
given in the README.

Cassette relies on new reflection features and compiler performance improvements that will
hopefully land in Julia 1.x. Until an initial version of Cassette is released, I can't
guarantee that Cassette's `master` branch won't rely on some weird custom version of Julia.

Cassette targets downstream package developers, not Julia end-users. Downstream developers
are expected to have a solid understanding of Julia's type system, metaprogramming
facilities, and dispatch mechanism.
