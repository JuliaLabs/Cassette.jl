# Contextual Dispatch

```@meta
CurrentModule = Cassette
```

In [the previous section](contextualdispatch.md), we saw how, within a given execution
trace, Cassette's `overdub` mechanism transforms every method invocation of the
form `f(args...)` into statements similar to the following:

```julia
begin
    Cassette.prehook(context, f, args...)
    tmp = Cassette.execute(context, f, args...)
    tmp = isa(tmp, Cassette.OverdubInstead) ? overdub(context, f, args...) : tmp
    Cassette.posthook(context, tmp, f, args...)
    tmp
end
```

This transformation yields several extra points of overloadability in the form of various
Cassette methods, such as [`prehook`](@ref), [`posthook`](@ref), and [`execute`](@ref).
Together, these methods form Cassette's "contextual dispatch" interface, so-called because it
allows an extra context parameter to participate in what would normally be a simple dispatch
to the underlying method call.

In this section, we'll go over these functions in a bit more detail.

Coming Soon!
