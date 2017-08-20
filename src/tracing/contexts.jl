#######
# Tag #
#######
# Note that the code/comments here were originally used for ForwardDiff's tagging system,
# and have been modified from their original version to fit your TV - er, to fit Cassette's
# use case.

struct Tag{T} end

@inline tagtype(::Tag{T}) where {T} = T

# Here, we could've just as easily used `hash`; however, this is unsafe/undefined behavior
# if `hash(::Type{F})` is overloaded in a module loaded after Cassette. Thus, we instead
# use `hash(Symbol(F))`, which is somewhat safer since it's far less likely that somebody
# would overwrite the Base definition for `Symbol(::DataType)` or `hash(::Symbol)`.
@generated function Tag(::F, ::Val{C}) where {F,C}
    @assert isa(C, Symbol)
    T = hash(C, hash(Symbol(F)))
    return quote
        $(Expr(:meta, :inline))
        Tag{$T}()
    end
end

##################################
# AbstractContext/AbstractCtxArg #
##################################

abstract type AbstractContext{T,F} end
abstract type AbstractCtxArg{T,V,M,U} end

function wrap end # this stub gets overloaded by Cassette.@context

@inline value(ctx, arg) = error("cannot extract `value` field: tag of context $(ctx) does not match tag of argument $(arg)")
@inline value(::AbstractContext{T}, arg::AbstractCtxArg{T}) where {T} = arg.value
@inline value(::Type{C}, ::Type{A}) where {T,V,M,U,C<:AbstractContext{T},A<:AbstractCtxArg{T,V,M,U}} = U

@inline meta(ctx, arg) = error("cannot extract `meta` field: tag of context $(ctx) does not match tag of argument $(arg)")
@inline meta(::AbstractContext{T}, arg::AbstractCtxArg{T}) where {T} = arg.meta
@inline meta(::Type{C}, ::Type{A}) where {T,V,M,C<:AbstractContext{T},A<:AbstractCtxArg{T,V,M}} = M

@inline unwrap(x) = x
@inline unwrap(ctx::AbstractContext) = ctx.func
@inline unwrap(::Type{C}) where {T,F,C<:AbstractContext{T,F}} = F

@inline unwrap(::AbstractContext, arg) = arg
@inline unwrap(ctx::AbstractContext{T}, arg::AbstractCtxArg{T}) where {T} = value(ctx, arg)
@inline unwrap(::Type{C}, ::Type{A}) where {C<:AbstractContext,A} = A
@inline unwrap(::Type{C}, ::Type{A}) where {T,C<:AbstractContext{T},A<:AbstractCtxArg{T}} = value(C, A)

@generated function unwrapcall(ctx::AbstractContext, args...)
    args = [:(unwrap(ctx, args[$i])) for i in 1:nfields(args)]
    return quote
        $(Expr(:meta, :inline))
        unwrap(ctx)($(args...))
    end
end

@inline ctxcall(f, g, ::AbstractContext, arg::Any) = g(arg)
@inline ctxcall(f, g, ctx::AbstractContext{T}, arg::AbstractCtxArg{T}) where {T} = f(value(ctx, arg), meta(ctx, arg))

@inline hascontext(::AbstractContext, ::Any) = false
@inline hascontext(::Type{<:AbstractContext}, ::Type{<:Any}) = false
@inline hascontext(::AbstractContext{T}, ::AbstractCtxArg{T}) where {T} = true
@inline hascontext(::Type{C}, ::Type{A}) where {T,C<:AbstractContext{T},A<:AbstractCtxArg{T}} = true

############
# @context #
############

const DEFINED_CONTEXTS = Dict{Symbol,Symbol}()

macro context(Ctx, CtxArg = nothing)
    expr = Expr(:block)
    haskey(DEFINED_CONTEXTS, Ctx) && error("context type ", Ctx, " is already defined")
    push!(expr.args, quote
        struct $Ctx{T,F} <: $Cassette.AbstractContext{T,F}
            tag::$Cassette.Tag{T}
            func::F
            @inline $Ctx(tag::$Cassette.Tag{T}, func::F) where {T,F} = new{T,F}(tag, func)
            @inline $Ctx(tag::$Cassette.Tag{T}, func::$Cassette.AbstractContext) where {T} = error("cannot nest contexts without a Trace barrier")
        end
        @inline $Ctx(f) = $Ctx($Cassette.Tag(f, Val($(Expr(:quote, Ctx)))), f)
        @inline $Cassette.wrap(ctx::$Ctx, f) = $Ctx(ctx.tag, f)
    end)
    if CtxArg !== nothing
        in(CtxArg, values(DEFINED_CONTEXTS)) && error("context argument type ", CtxArg, " is already defined")
        push!(expr.args, quote
            struct $CtxArg{T,V,M,U} <: $Cassette.AbstractCtxArg{T,V,M,U}
                tag::$Cassette.Tag{T}
                value::U
                meta::M
                @inline function $CtxArg(ctx::$Ctx{T}, value::V, meta::M = nothing) where {T,V,M}
                    new{T,V,M,V}(ctx.tag, value, meta)
                end
                @inline function $CtxArg(ctx::$Ctx{T}, value::Type{V}, meta::M = nothing) where {T,V,M}
                    new{T,Type{V},M,Type{V}}(ctx.tag, value, meta)
                end
                @inline function $CtxArg(ctx::$Ctx{T}, value::$Cassette.AbstractCtxArg{<:Any,V}, meta::M = nothing) where {T,V,M}
                    new{T,V,M,typeof(value)}(ctx.tag, value, meta)
                end
            end
        end)
    end
    DEFINED_CONTEXTS[Ctx] = CtxArg
    return esc(expr)
end

###############
# @contextual #
###############

function is_ctx_dispatch(x)
    if isa(x, Expr)
        if x.head == :(::)
            return is_quoted_symbol(last(x.args))
        elseif x.head == :(:)
            v, C = x.args
            return is_non_ctx_dispatch(v) && isa(C, Symbol)
        end
    end
    return false
end

is_non_ctx_dispatch(x) = isa(x, Expr) && x.head == :(::) && !is_ctx_dispatch(x)

function parse_ctx_dispatch(x)
    if x.head == :(::)
        v = length(x.args) == 1 ? nothing : x.args[1]
        V = :Any
        C = extract_quoted_symbol(last(x.args))
    elseif x.head == :(:)
        y, C = x.args
        v = length(y.args) == 1 ? nothing : y.args[1]
        V = last(y.args)
    else
        error("encountered malformed `@contextual` syntax: ", x)
    end
    return v, V, C
end

function transform_ctx_dispatch(v, V, C, tag)
    v === nothing && V === :Any && return :(::$C{$tag})
    v === nothing && return :(::$C{$tag,<:$V})
    V === :Any && return :($v::$C{$tag})
    return :($v::$C{$tag,<:$V})
end

function contextual_transform!(def)
    @assert is_method_definition(def)
    signature, body = def.args
    sig_caller = extract_caller_from_signature(signature)
    sig_args = extract_args_from_signature(signature)
    @assert is_ctx_dispatch(sig_caller)

    # add the tag parameter to the signature's `where` clause
    tag = gensym("TagTypeVar")
    add_type_variable!(def, tag)

    # replace use of triple-colon syntax with contextualized equivalent
    v, V, MC = parse_ctx_dispatch(sig_caller)
    haskey(DEFINED_CONTEXTS, MC) || error("context type not defined: ", MC)
    replace_signature_caller!(signature, transform_ctx_dispatch(v, V, MC, tag))

    AC = DEFINED_CONTEXTS[MC]
    for i in eachindex(sig_args)
        x = sig_args[i]
        if is_non_ctx_dispatch(x)
            # replace `::V` with `::Union{AbstractCtxArg{<:Any,V},V}`
            i = length(x.args)
            (i == 1 || i == 2) || error("failed to parse dispatch syntax in subexpression ", x)
            V = x.args[i]
            x.args[i] = :(Union{$Cassette.AbstractCtxArg{<:Any,$V},$V})
        elseif is_ctx_dispatch(x)
            # replace use of triple-colon syntax with contextualized equivalent
            v, V, C = parse_ctx_dispatch(x)
            if C !== AC
                error("argument context type ", C, " is not paired with the method context type ",
                      MC, "; the correct argument context type for ", MC, " is ", AC)
            end
            sig_args[i] = transform_ctx_dispatch(v, V, C, tag)
        end
    end

    # make sure the user didn't mistakenly use triple-colon syntax in the method body
    replace_match!(is_ctx_dispatch, body) do x
        error("Triple-colon syntax can only be used in the method signature, not body! ",
              "Encountered triple-colon syntax in method body subexpression: ", x)
    end

    return def
end

macro contextual(def)
    contextual_transform!(def)
    return esc(def)
end
