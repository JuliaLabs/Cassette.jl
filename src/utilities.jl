############################
# Expression Match/Replace #
############################

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

function insert_ir_elements!(code, codelocs, itemcount, getitems)
    ssachangemap = fill(0, length(code))
    labelchangemap = fill(0, length(code))
    worklist = Tuple{Int,Int}[]
    for i in 1:length(code)
        stmt = code[i]
        nitems = itemcount(stmt, i)
        if nitems !== nothing
            addeditems = nitems - 1
            push!(worklist, (i, addeditems))
            ssachangemap[i] = addeditems
            if i < length(code)
                labelchangemap[i + 1] = addeditems
            end
        end
    end
    Core.Compiler.renumber_ir_elements!(code, ssachangemap, labelchangemap)
    for (i, addeditems) in worklist
        i += ssachangemap[i] - addeditems # correct the index for accumulated offsets
        stmt = code[i]
        items = getitems(stmt, i)
        @assert length(items) == (addeditems + 1)
        code[i] = items[end]
        for j in 1:(length(items) - 1) # insert in reverse to maintain the provided ordering
            insert!(code, i, items[end - j])
            insert!(codelocs, i, codelocs[i])
        end
    end
end

#############
# Debugging #
#############

overdub_typed(args...; optimize=false) = code_typed(overdub, map(Core.Typeof, args); optimize=optimize)
