#########
# @pass #
#########

"""
```
Cassette.@pass transform
```

Return a Cassette pass that can be provided to the `Context` constructor's `pass` keyword
argument in order to apply `transform` to the lowered IR representations of all methods
invoked during contextual execution.

`transform` must be a Julia object that is callable with the following signature:

```
transform(::Type{<:Context}, ::Cassette.Reflection)::Union{Expr,CodeInfo}
```

If `isa(transform(...), Expr)`, then the returned `Expr` will be emitted immediately
without any additional processing. Otherwise, if `isa(transform(...), CodeInfo)`, then
the returned `CodeInfo` will undergo the rest of Cassette's overdubbing transformation
before being emitted from the `overdub` generator.

Two special `Expr` heads are available to Cassette pass authors that are not normally valid
in Julia IR. `Expr`s with these heads can be used to interact with the downstream built-in
Cassette passes that consume them.

- `:nooverdub`: Wrap an `Expr` with this head value around the first argument in an
    `Expr(:call)` to tell downstream built-in Cassette passes not to overdub that call. For
    example, `Expr(:call, Expr(:nooverdub, GlobalRef(MyModule, :myfunc)), args...)`.

- `:contextslot`: Cassette will replace any `Expr(:contextslot)` with the actual `SlotNumber`
    corresponding to the context object associated with the execution trace. For example, one
    could construct an IR element that accesses the context's `metadata` field by emitting:
    `Expr(:call, Expr(:nooverdub, GlobalRef(Core, :getfield)), Expr(:contextslot), QuoteNode(:metadata))`

Cassette provides a few IR-munging utility functions of interest to pass authors; for details,
see [`insert_statements!`](@ref), [`replace_match!`](@ref), and [`is_ir_element`](@ref).

Note that the `@pass` macro expands to an `eval` call and thus should only be called at
top-level. Furthermore, to avoid world-age issues, `transform` should not be overloaded after
it has been registered with `@pass`.

Note also that `transform` should be "relatively pure." More specifically, Julia's compiler
has license to apply `transform` multiple times, even if only compiling a single method
invocation once. Thus, it is required that `transform` always return a generally "equivalent"
`CodeInfo` for a given context, method body, and signature. If your `transform`
implementation is not naturally "pure" in this sense, then it is still possible to guarantee
this property by memoizing your implementation (i.e. maintaining a cache of previously
computed IR results, instead of recomputing results every time).

See also: [`Context`](@ref), [`overdub`](@ref)
"""
macro pass(transform)
    Pass = gensym("PassType")
    line = Expr(:quote, __source__.line)
    file = Expr(:quote, __source__.file)
    return esc(quote
        import Cassette.__overdub_generator__
        struct $Pass <: $Cassette.AbstractPass end
        (::Type{$Pass})(ctxtype, reflection) = $transform(ctxtype, reflection)
        Core.eval($__module__, $Cassette.overdub_definition($line, $file))
        $Pass()
    end)
end

#############
# utilities #
#############

"""
```
replace_match!(replace, ismatch, x)
```

Return `x` with all subelements `y` replaced with `replace(y)` if `ismatch(y)`. If
`!ismatch(y)`, but `y` is of type `Expr`, `Array`, or `SubArray`, then replace `y`
in `x` with `replace_match!(replace, ismatch, y)`.

Generally, `x` should be of type `Expr`, `Array`, or `SubArray`.

Note that this function modifies `x` (and potentially its subelements) in-place.

See also: [`insert_statements!`](@ref), [`is_ir_element`](@ref)
"""
function replace_match!(replace, ismatch, x)
    if ismatch(x)
        return replace(x)
    elseif isa(x, Array) || isa(x, SubArray)
        for i in eachindex(x)
            x[i] = replace_match!(replace, ismatch, x[i])
        end
    elseif isa(x, Expr)
        replace_match!(replace, ismatch, x.args)
    end
    return x
end


"""
```
insert_statements!(code::Vector, codelocs::Vector, stmtcount, newstmts)
```

For every statement `stmt` at position `i` in `code` for which `stmtcount(stmt, i)` returns
an `Int`, remove `stmt`, and in its place, insert the statements returned by
`newstmts(stmt, i)`. If `stmtcount(stmt, i)` returns `nothing`, leave `stmt` alone.

For every insertion, all downstream `SSAValue`s, label indices, etc. are incremented
appropriately according to number of inserted statements.

Proper usage of this function dictates that following properties hold true:

- `code` is expected to be a valid value for the `code` field of a `CodeInfo` object.
- `codelocs` is expected to be a valid value for the `codelocs` field of a `CodeInfo` object.
- `newstmts(stmt, i)` should return a `Vector` of valid IR statements.
- `stmtcount` and `newstmts` must obey `stmtcount(stmt, i) == length(newstmts(stmt, i))` if
    `isa(stmtcount(stmt, i), Int)`.

To gain a mental model for this function's behavior, consider the following scenario. Let's
say our `code` object contains several statements:

```
code = Any[oldstmt1, oldstmt2, oldstmt3, oldstmt4, oldstmt5, oldstmt6]
codelocs = Int[1, 2, 3, 4, 5, 6]
```

Let's also say that for our `stmtcount` returns `2` for `stmtcount(oldstmt2, 2)`, returns `3`
for `stmtcount(oldstmt5, 5)`, and returns `nothing` for all other inputs. From this setup, we
can think of `code`/`codelocs` being modified in the following manner:

```
newstmts2 = newstmts(oldstmt2, 2)
newstmts5 = newstmts(oldstmt5, 5)
code = Any[oldstmt1,
           newstmts2[1], newstmts2[2],
           oldstmt3, oldstmt4,
           newstmts5[1], newstmts5[2], newstmts5[3],
           oldstmt6]
codelocs = Int[1, 2, 2, 3, 4, 5, 5, 5, 6]
```

See also: [`replace_match!`](@ref), [`is_ir_element`](@ref)
"""
function insert_statements!(code, codelocs, stmtcount, newstmts)
    ssachangemap = fill(0, length(code))
    labelchangemap = fill(0, length(code))
    worklist = Tuple{Int,Int}[]
    for i in 1:length(code)
        stmt = code[i]
        nstmts = stmtcount(stmt, i)
        if nstmts !== nothing
            addedstmts = nstmts - 1
            push!(worklist, (i, addedstmts))
            ssachangemap[i] = addedstmts
            if i < length(code)
                labelchangemap[i + 1] = addedstmts
            end
        end
    end
    Core.Compiler.renumber_ir_elements!(code, ssachangemap, labelchangemap)
    for (i, addedstmts) in worklist
        i += ssachangemap[i] - addedstmts # correct the index for accumulated offsets
        stmts = newstmts(code[i], i)
        @assert length(stmts) == (addedstmts + 1)
        code[i] = stmts[end]
        for j in 1:(length(stmts) - 1) # insert in reverse to maintain the provided ordering
            insert!(code, i, stmts[end - j])
            insert!(codelocs, i, codelocs[i])
        end
    end
end


"""
```
is_ir_element(x, y, code::Vector)
```

Return `true` if `x === y` or if `x` is an `SSAValue` such that
`is_ir_element(code[x.id], y, code)` is `true`.

See also: [`replace_match!`](@ref), [`insert_statements!`](@ref)
"""
function is_ir_element(x, y, code::Vector)
    result = false
    while true # break by default
        if x === y #
            result = true
            break
        elseif isa(x, SSAValue)
            x = code[x.id]
        else
            break
        end
    end
    return result
end
