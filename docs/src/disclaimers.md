# Disclaimers (Don't Say I Didn't Warn You)

Cassette is a powerful tool for extending the Julia language, but it is also a loaded
foot-gun. Here are some things one should know before using Cassette:

- Cassette, its API, and its documentation targets Julia package developers and/or those
interested in doing compiler research using Julia. Cassette users are expected to have a
working understanding of Julia's compiler, type system and metaprogramming facilities.

- Practical usage of Cassette will quite likely reveal both performance and correctness
bugs caused by either Cassette or Julia itself (especially in this early stage of
development). This is especially likely when doing nested overdubbing, or composing
multiple Cassette contexts. Please file issues on the Cassette and/or Julia issue tracker
where appropriate.

- For now, Cassette technically only supports a single specific version of Julia at a time;
differing by even a patch version could (theoretically) break Cassette entirely. This is
because of Cassette's close interaction with Julia internals that traditionally have license
to change between patch versions. In the future, the planned resolution for the issue is for
Julia itself to perform reverse-dependency testing against Cassette's tests, such that
breaking changes to Julia compiler are discovered and fixed immediately.

- The performance of Cassette's implementation of the contextual tagging system heavily
depends on compiler improvements planned for the Julia `1.x` release cycle. In theory, given
these compiler improvements, the contextual tagging system could achieve performance
comparable to alternatives (e.g. ForwardDiff's dual number implementation), but for now, the
contextual tagging system is quite slow and allocation-heavy.

- Cassette enables interaction with many parts of the Julia compiler, a lot of which
are undocumented or sparsely documented. It is extremely easy to accidentally implement a
pass that is breaks internal compiler assumptions in some subtle way. If (and when) you
run into these scenarios, it would be helpful to open an issue on the Julia issue tracker
proposing better documentation (or even a stable API) for a specific part of the compiler.

- Due to limitations of Julia's current world-age mechanism, Cassette exhibits a similar
recompilation problem to the famous JuliaLang/julia#265. See [TODO: put world-age cassette
issue here]. In order to resolve this issue, a world-age mechanism redesign is planned for
the Julia `1.x` release cycle.
