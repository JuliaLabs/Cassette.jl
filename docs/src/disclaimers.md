# Disclaimers (Don't Say I Didn't Warn You)

Cassette can be a powerful tool for extending the Julia language, but it functions equally
well as a loaded foot-gun. Here are some things one should know before using Cassette:

- **Cassette, its API, and its documentation targets Julia package developers and/or those
    interested in doing compiler research using Julia.** Cassette users are expected to have
    a working understanding of Julia's compiler, type system and metaprogramming facilities.

- **Practical usage of Cassette will quite likely reveal both performance and correctness
    bugs caused by either Cassette or Julia itself** (especially in this early stage of
    development). This is especially likely when doing nested overdubbing, or composing
    multiple Cassette contexts. Please file issues on the Cassette and/or Julia issue
    tracker where appropriate.

- **The performance of Cassette's implementation of the contextual tagging system heavily
    depends on compiler improvements planned for the Julia `1.x` release cycle.** In theory,
    given these compiler improvements, the contextual tagging system could achieve
    performance comparable to alternatives (e.g. ForwardDiff's dual number implementation),
    but for now, the contextual tagging system is quite slow and allocation-heavy.

- Cassette enables interaction with many parts of the Julia compiler, a lot of which are
    undocumented or sparsely documented. **It is extremely easy to accidentally implement a
    pass that breaks internal compiler assumptions in some subtle way.** If (and when) you
    run into these scenarios, it would be helpful to open an issue on the Julia issue tracker
    proposing better documentation (or even a stable API) for a specific part of the
    compiler.

- Due to limitations of Julia's current world-age mechanism, **Cassette exhibits a similar
    recompilation problem to the famous
    [JuliaLang/julia#265](https://github.com/JuliaLang/julia/issues/265)** (see
    [jrevels/Cassette.jl#6](https://github.com/jrevels/Cassette.jl/issues/6) for details). In
    order to resolve this issue, an update to Julia's world-age mechanism is planned for the
    Julia `1.x` release cycle.
