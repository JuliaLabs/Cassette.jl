#########################
# Expression Predicates #
#########################

is_call(x) = Base.Meta.isexpr(x, :call)

is_new(x) = Base.Meta.isexpr(x, :new)

is_macro(x, name) = Base.Meta.isexpr(x, :macrocall) && x.args[1] == name

function is_method_definition(x)
    if isa(x, Expr)
        if x.head == :function
            return true
        elseif x.head == :(=) && isa(x.args[1], Expr)
            lhs = x.args[1]
            if lhs.head == :where
                lhs = lhs.args[1]
            end
            return lhs.head == :call
        end
    end
    return false
end

############################
# Expression Match/Replace #
############################

function replace_match!(replace, ismatch, x)
    if ismatch(x)
        return replace(x)
    elseif isa(x, Array)
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

#=== IR Repair ===#

# TODO: update this
function fix_labels_and_gotos!(code::Vector)
    changes = Dict{Int,Int}()
    for (i, stmnt) in enumerate(code)
        if isa(stmnt, LabelNode)
            code[i] = LabelNode(i)
            changes[stmnt.label] = i
        end
    end
    for (i, stmnt) in enumerate(code)
        if isa(stmnt, GotoNode)
            code[i] = GotoNode(get(changes, stmnt.label, stmnt.label))
        elseif Base.Meta.isexpr(stmnt, :enter)
            stmnt.args[1] = get(changes, stmnt.args[1], stmnt.args[1])
        elseif Base.Meta.isexpr(stmnt, :gotoifnot)
            stmnt.args[2] = get(changes, stmnt.args[2], stmnt.args[2])
        end
    end
    return code
end

#################
# Miscellaneous #
#################

unquote(x) = x
unquote(x::QuoteNode) = x.value
unquote(x::Expr) = x.head == :quote ? first(x.args) : x

function unqualify_name(e::Expr)
    @assert e.head == :(.)
    return unqualify_name(last(e.args))
end

unqualify_name(name::Symbol) = name

# define safe loggers for use in generated functions (where task switches are not allowed)
for level in [:debug, :info, :warn, :error]
    @eval begin
        macro $(Symbol("safe_$level"))(ex...)
            macrocall = :(@placeholder $(ex...))
            # NOTE: `@placeholder` in order to avoid hard-coding @__LINE__ etc
            macrocall.args[1] = Symbol($"@$level")
            quote
                old_logger = global_logger()
                global_logger(Logging.ConsoleLogger(Core.stderr, old_logger.min_level))
                ret = $(esc(macrocall))
                global_logger(old_logger)
                ret
            end
        end
    end
end

# avoid JuliaLang/julia#28070
@generated function specialized_apply(f, args...)
    quote
        $(Expr(:meta, :inline))
        Core._apply(f, $([:(args[$i]) for i = 1:nfields(args)]...))
    end
end

@generated function specialized_tuple_apply(args...)
    quote
        $(Expr(:meta, :inline))
        Core._apply(Core.tuple, $([:(args[$i]) for i = 1:nfields(args)]...))
    end
end
