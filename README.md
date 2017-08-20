# Cassette

## DISCLAIMER

Cassette is still in development. At any given time, the implementation might be ugly,
buggy, incomplete, slow, and/or untested. Cassette relies on new reflection features and
compiler performance improvements that will hopefully land in Julia 1.0; until an initial
version of Cassette is released, I can't guarantee that Cassette's `master` branch won't
rely on some weird custom version of Julia.

## Intro

Cassette is a Julia package that provides...

- ...a *contextual granular tracer* for the Julia language!
- ...a *dynamic computational graph* implementation!
- ...a *static computational graph* implementation!

Cassette targets downstream package developers, not Julia end-users. Cassette's API and
documentation expects that developers have a solid understanding of Julia's type system
and dispatch mechanism.

Downstream applications for Cassette include automatic differentiation, interval constraint
programming, dynamic code analysis (e.g. profiling, debugging), JIT transpilation to
different backends, automatic parallelization/rescheduling, memoization, automatic
preallocation, and code fuzzing.

## What is a contextual granular tracer?

First and foremost, "contextual granular tracer" is a phrase I made up. If anybody can think
of a better one,
Well, I just made up this phrase specifically to
describe Cassette, so let's unpack it (in rev):

- Cassette is a *Julia tracer*. It dynamically intercepts native Julia method calls as they occur
during program execution.

- Cassette's tracing

## Cassette's Computation Graph Framework

TODO
