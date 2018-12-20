
<p align="center">
<img width="350px" src="https://raw.githubusercontent.com/jrevels/Cassette.jl/master/docs/img/cassette-logo.png"/>
</p>

[![Build Status](https://travis-ci.org/jrevels/Cassette.jl.svg?branch=master)](https://travis-ci.org/jrevels/Cassette.jl)
[![Coverage Status](https://coveralls.io/repos/github/jrevels/Cassette.jl/badge.svg)](https://coveralls.io/github/jrevels/Cassette.jl)

[![](https://img.shields.io/badge/docs-stable-blue.svg)](https://jrevels.github.io/Cassette.jl/stable/)
[![](https://img.shields.io/badge/docs-latest-blue.svg)](https://jrevels.github.io/Cassette.jl/latest/)

[![DOI](https://zenodo.org/badge/86752121.svg)](https://zenodo.org/badge/latestdoi/86752121)

## Overview

Cassette lets you easily extend the Julia language by directly injecting the Julia compiler with new, context-specific behaviors.

More technically, Cassette is a Julia package that provides a mechanism for dynamically injecting code transformation passes into Julia’s just-in-time (JIT) compilation cycle, enabling post hoc analysis and modification of "Cassette-unaware" Julia programs without requiring manual source annotation or refactoring of the target code.

Cassette's API is built upon the notion of user-definable execution contexts, which are represented as normal Julia types. Using Cassette, any normal Julia function can be invoked within an execution context via a process called "overdubbing". Cassette provides users with multiple ways to define the execution of code overdubbed with their context type. For example, Cassette's lowest level interface allows users to implement arbitrary `CodeInfo` transformations via normal Julia functions, and easily apply these transformations to overdubbed method bodies at compile-time. Cassette also provides a higher-level "contextual dispatch" interface that allows users to safely and quickly overload existing Julia methods with context-specific behaviors without ever needing to handle Julia’s IR directly.

On top of contextual pass injection and contextual dispatch, Cassette implements a system for "tagging" values with respect to a context, optionally attaching metadata to these tagged values (e.g. the value's derivative). Cassette can then automatically propagate these tagged values throughout target programs, even in the presence of structural and/or dispatch type constraints.

Downstream applications for Cassette include dynamic code analysis (e.g. profiling, rr-style debugging, etc.), JIT compilation to new hardware/software backends, automatic differentiation, interval constraint programming, automatic parallelization/rescheduling, automatic memoization, lightweight multistage programming, graph extraction, and more.
