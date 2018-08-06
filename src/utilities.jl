############################
# Expression Match/Replace #
############################

"""
```
replace_match!(replace, ismatch, x)
```

Return `x` with all subelements `y` replaced with `replace(y)` if `ismatch(y)`. If
`!ismatch(y)`, but `y` is of type `Expr`, `Array`, or `SubArray`, then replace `y`
in `x` with `replace_match!(replace, ismatch, y)`.

Generally, `x` should be of type `Expr`, `Array`, or `SubArray`.

Note that this function modifies `x` (and potentially its subelements) in-place.

See also: [`insert_statements!`](@ref)
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

############
# Julia IR #
############

#=== reflection ===#

mutable struct Reflection
    signature::DataType
    method::Method
    static_params::Vector{Any}
    code_info::CodeInfo
end

# Return `Reflection` for signature `sigtypes` and `world`, if possible. Otherwise, return `nothing`.
function reflect(@nospecialize(sigtypes::Tuple), world::UInt = typemax(UInt))
    # This works around a subtyping bug. Basically, callers can deconstruct upstream
    # `UnionAll` types in such a way that results in a type with free type variables, in
    # which case subtyping can just break.
    #
    # God help you if you try to use a type parameter here (e.g. `::Type{S} where S<:Tuple`)
    # instead of this nutty workaround, because the compiler can just rewrite `S` into
    # whatever it thinks is "type equal" to the actual provided value. In other words, if
    # `S` is defined as e.g. `f(::Type{S}) where S`, and you call `f(T)`, you should NOT
    # assume that `S === T`. If you did, SHAME ON YOU. It doesn't matter that such an
    # assumption holds true for essentially all other kinds of values. I haven't counted in
    # a while, but I'm pretty sure I have ~40+ hellish years of Julia experience, and this
    # still catches me every time. Who even uses this crazy language?
    S = Tuple{map(s -> Core.Compiler.has_free_typevars(s) ? typeof(s.parameters[1]) : s, sigtypes)...}
    (S.parameters[1]::DataType).name.module === Core.Compiler && return nothing
    _methods = Base._methods_by_ftype(S, -1, world)
    length(_methods) == 1 || return nothing
    type_signature, raw_static_params, method = first(_methods)
    method_instance = Core.Compiler.code_for_method(method, type_signature, raw_static_params, world, false)
    method_instance === nothing && return nothing
    method_signature = method.sig
    static_params = Any[raw_static_params...]
    code_info = Core.Compiler.retrieve_code_info(method_instance)
    isa(code_info, CodeInfo) || return nothing
    code_info = Core.Compiler.copy_code_info(code_info)
    return Reflection(S, method, static_params, code_info)
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

See also: [`replace_match!`](@ref)
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

#############
# Debugging #
#############

overdub_typed(args...; optimize=false) = code_typed(overdub, map(Core.Typeof, args); optimize=optimize)
