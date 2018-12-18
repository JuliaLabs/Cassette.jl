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
flow or the surrounding scope of a method call. In these cases, you probably *do* want to
manually implement a compiler pass!

To facilitate these use cases, Cassette allows users to write and inject their own arbitrary
post-lowering, pre-inference compiler passes as part of the overdubbing process. This feature
of Cassette is called "contextual pass injection". As we did in the preceding sections,
we'll be using the classic "trial-by-fire" technique to better understand this feature.

Note that the following example was originally inspired by [jrevels/Cassette.jl#66](https://github.com/jrevels/Cassette.jl/issues/66).

Let's say you wanted to use Cassette to ["slice" various separable
subcomputations out from an overall computation](https://en.wikipedia.org/wiki/Program_slicing).
For a specific example, let's say you wanted to implement a tool that takes a
Julia function and strips out calls to `println` encountered in the trace. When
this function returns, we also want to return a callback that executes all the
`println` calls that we stripped out. How would you implement this with
Cassette?

Well, it's not too hard to achieve this via the contextual dispatch interface:

```julia
using Cassette

Cassette.@context Ctx

mutable struct Callback
    f::Any
end

function Cassette.overdub(ctx::Ctx, ::typeof(println), args...)
    previous = ctx.metadata.f
    ctx.metadata.f = () -> (previous(); println(args...))
    return nothing
end
```

We can check our implementation using the following test case:

```julia
julia> begin
           a = rand(3)
           b = rand(3)
           function add(a, b)
               println("I'm about to add $a + $b")
               c = a + b
               println("c = $c")
               return c
           end
           add(a, b)
       end
I'm about to add [0.457465, 0.62078, 0.954555] + [0.0791336, 0.744041, 0.976194]
c = [0.536599, 1.36482, 1.93075]
3-element Array{Float64,1}:
 0.5365985032259399
 1.3648210555868863
 1.9307494378914405

julia> ctx = Ctx(metadata = Callback(() -> nothing));

julia> c = Cassette.overdub(ctx, add, a, b)
3-element Array{Float64,1}:
 0.5365985032259399
 1.3648210555868863
 1.9307494378914405

julia> ctx.metadata.f()
I'm about to add [0.457465, 0.62078, 0.954555] + [0.0791336, 0.744041, 0.976194]
c = [0.536599, 1.36482, 1.93075]
```

This is pretty cool, but also a bit limited. First of all, what if we want to move more than
just `println` invocations into our callback, e.g. what if we want to slice the construction
of `println`'s arguments as well? Another potential issue is that this implementation
requires an explicit `Any` barrier, preventing the compiler from inferring callback
construction (note, however, that this does not prevent inferring *invocation* of the
callback). This is possibly desirable in some cases, since you're being easier on the
compiler, but what if you really wanted to expose Julia's type inference to callback
construction?

To resolve issues like these, we'll need to dig deeper than contextual dispatch and implement an
actual compiler pass. First, let's go over a high-level description of the pass we'll implement.
Essentially, we want this method:

```julia
function add(a, b)
    println("I'm about to add $a + $b")
    c = a + b
    println("c = $c")
    return c
end
```

...to become something akin to the following when overdubbed:

```julia
function overdub(ctx::Ctx, add, a, b)
    _callback_ = ctx.metadata
    _, _callback_ = overdub(ctx, println, _callback_, "I'm about to add $a + $b")
    c, _callback_ = overdub(ctx, +, _callback_, a, b)
    _, _callback_ = overdub(ctx, println, _callback_, "c = $c")
    return c, _callback_
end
```

Note that I reduced a lot of the contextual dispatch boilerplate, such that the above is
essentially pseudocode.

Here, we can overload `Ctx`'s `overdub` method with the following definitions:

```julia
function Cassette.overdub(ctx::SliceCtx, f, callback, args...)
    if Cassette.canrecurse(ctx, f, args...)
        _ctx = Cassette.similarcontext(ctx, metadata = callback)
        return Cassette.recurse(_ctx, f, args...) # return result, callback
    else
        return Cassette.fallback(ctx, f, args...), callback
    end
end

function Cassette.overdub(ctx::SliceCtx, ::typeof(println), callback, args...)
    return nothing, () -> (callback(); println(args...))
end
```

This, then, essentially accumulates the same closure we were accumulating before, but does so in a
way where...

- ...in theory, there is no longer any barrier to the inference of the closure construction.
- ...the pass itself determines the "capture region" manually, such that one could just alter it
    to do e.g. linear dependence analysis to capture `println` argument construction code an arbitrary
    number of degrees out from the actual `println` invocation.

Next, let's list the steps our compiler pass will actually need to perform in order to actually
accomplish the above:

- At the beginning of each method body, insert something like `_callback_ = context.metadata`
- Change every method invocation of the form `f(args...)` to `f(_callback_, args...)`.
- Change every return statement of the form `return x` to `return (x, _callback_)`
- Ensure the output of every method invocation is properly destructured into the original assignment slot/SSAValue and the `_callback_` slot.

Okay! Now that we have a high-level description of our pass, let's look at the code that implements
it. **I highly recommend reading the documentation for [`@pass`](@ref) and
[`insert_statements!`](@ref) before trying to understand this code**.

```julia
using Cassette
using Core: CodeInfo, SlotNumber, SSAValue

Cassette.@context SliceCtx

function Cassette.overdub(ctx::SliceCtx, f, callback, args...)
    if Cassette.canrecurse(ctx, f, args...)
        _ctx = Cassette.similarcontext(ctx, metadata = callback)
        return Cassette.recurse(_ctx, f, args...) # return result, callback
    else
        return Cassette.fallback(ctx, f, args...), callback
    end
end

function Cassette.overdub(ctx::SliceCtx, ::typeof(println), callback, args...)
    return nothing, () -> (callback(); println(args...))
end

function sliceprintln(::Type{<:SliceCtx}, reflection::Cassette.Reflection)
    ir = reflection.code_info
    callbackslotname = gensym("callback")
    push!(ir.slotnames, callbackslotname)
    push!(ir.slotflags, 0x00)
    callbackslot = SlotNumber(length(ir.slotnames))
    getmetadata = Expr(:call, Expr(:nooverdub, GlobalRef(Core, :getfield)), Expr(:contextslot), QuoteNode(:metadata))

    # insert the initial `callbackslot` assignment into the IR.
    Cassette.insert_statements!(ir.code, ir.codelocs,
                                (stmt, i) -> i == 1 ? 2 : nothing,
                                (stmt, i) -> [Expr(:(=), callbackslot, getmetadata), stmt])

    # replace all calls of the form `f(args...)` with `f(callback, args...)`, taking care to
    # properly handle Core._apply calls and destructure the returned `(result, callback)`
    # into the appropriate statements
    Cassette.insert_statements!(ir.code, ir.codelocs,
                                (stmt, i) -> begin
                                    i > 1 || return nothing # don't slice the callback assignment
                                    stmt = Base.Meta.isexpr(stmt, :(=)) ? stmt.args[2] : stmt
                                    if Base.Meta.isexpr(stmt, :call)
                                        isapply = Cassette.is_ir_element(stmt.args[1], GlobalRef(Core, :_apply), ir.code)
                                        return 3 + isapply
                                    end
                                    return nothing
                                end,
                                (stmt, i) -> begin
                                    items = Any[]
                                    callstmt = Base.Meta.isexpr(stmt, :(=)) ? stmt.args[2] : stmt
                                    callssa = SSAValue(i)
                                    if Cassette.is_ir_element(callstmt.args[1], GlobalRef(Core, :_apply), ir.code)
                                        push!(items, Expr(:call, Expr(:nooverdub, GlobalRef(Core, :tuple)), callbackslot))
                                        push!(items, Expr(:call, callstmt.args[1], callstmt.args[2], SSAValue(i), callstmt.args[3:end]...))
                                        callssa = SSAValue(i + 1)
                                    else
                                        push!(items, Expr(:call, callstmt.args[1], callbackslot, callstmt.args[2:end]...))
                                    end
                                    push!(items, Expr(:(=), callbackslot, Expr(:call, Expr(:nooverdub, GlobalRef(Core, :getfield)), callssa, 2)))
                                    result = Expr(:call, Expr(:nooverdub, GlobalRef(Core, :getfield)), callssa, 1)
                                    if Base.Meta.isexpr(stmt, :(=))
                                        result = Expr(:(=), stmt.args[1], result)
                                    end
                                    push!(items, result)
                                    return items
                                end)

    # replace return statements of the form `return x` with `return (x, callback)`
    Cassette.insert_statements!(ir.code, ir.codelocs,
                                  (stmt, i) -> Base.Meta.isexpr(stmt, :return) ? 2 : nothing,
                                  (stmt, i) -> begin
                                      return [
                                          Expr(:call, Expr(:nooverdub, GlobalRef(Core, :tuple)), stmt.args[1], callbackslot)
                                          Expr(:return, SSAValue(i))
                                      ]
                                  end)
    return ir
end

const sliceprintlnpass = Cassette.@pass sliceprintln
```

Here's how to invoke this new implementation on the above test case:

```julia
julia> begin
           a = rand(3)
           b = rand(3)
           function add(a, b)
               println("I'm about to add $a + $b")
               c = a + b
               println("c = $c")
               return c
           end
           add(a, b)
       end
I'm about to add [0.325019, 0.19358, 0.200598] + [0.195759, 0.653, 0.498859]
c = [0.520778, 0.84658, 0.699457]
3-element Array{Float64,1}:
 0.5207782045663867
 0.846579992552251
 0.6994565474128307

julia> ctx = SliceCtx(pass=sliceprintlnpass, metadata = () -> nothing);

julia> result, callback = Cassette.@overdub(ctx, add(a, b))
([0.520778, 0.84658, 0.699457], getfield(Main, Symbol("##4#5")){getfield(Main, Symbol("##4#5")){getfield(Main, Symbol("##18#19")),Tuple{String}},Tuple{String}}(getfield(Main, Symbol("##4#5")){getfield(Main, Symbol("##18#19")),Tuple{String}}(getfield(Main, Symbol("##18#19"))(), ("I'm about to add [0.325019, 0.19358, 0.200598] + [0.195759, 0.653, 0.498859]",)), ("c = [0.520778, 0.84658, 0.699457]",)))

julia> callback()
I'm about to add [0.325019, 0.19358, 0.200598] + [0.195759, 0.653, 0.498859]
c = [0.520778, 0.84658, 0.699457]
```
