# Contextual Compiler Pass Injection

```@meta
CurrentModule = Cassette
```

In the previous section on [Cassette's overdubbing mechanism](overdub.md), we explored how
Cassette can automatically transform methods' lowered representation to insert a bunch of
statements around method calls encountered while overdubbing. In [the section that followed](contextualdispatch.md),
we discussed the result of this IR transformation: Cassette's contextual dispatch interface,
a suite of normal Julia methods that can be easily overloaded to perform the kinds of method
replacement and instrumentation that would otherwise require manually implemented compiler
passes.

Some use cases, however, require the ability to access and/or alter properties of the
execution trace that just can't be reached via simple method overloading, like control
flow or the surrounding scope of a method call.

To facilitate these use cases, Cassette allows users to write and inject their own arbitrary
post-lowering, pre-inference compiler passes as part of the overdubbing process. This feature
of Cassette is called "contextual pass injection".
