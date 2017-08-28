#######
# Tag #
#######

struct Tag{T} end

@generated function Tag(::T) where {T}
    return quote
        $(Expr(:meta, :inline))
        Tag{$(object_id(T))}()
    end
end

###########
# Context #
###########

abstract type Context{S,T} end

Base.show(io::IO, ::Context{S,T}) where {S,T} = print(io, "$S{$T}()")

########
# Meta #
########

struct Meta{C<:Context,V,M,U}
    context::C
    value::V
    meta::M
    @inline Meta(context::C, value::V, meta::M = nothing) where {C,V,M} = new{C,V,M,V}(context, value, meta)
    @inline Meta(context::C, value::Type{V}, meta::M = nothing) where {C,V,M} = new{C,Type{value},M,Type{value}}(context, value, meta)
    @inline Meta(context::C, value::Meta{<:Context,<:Any,<:Any,U}, meta::M = nothing) where {C,M,U} = new{C,typeof(value),M,U}(context, value, meta)
end

@inline value(::Context, x) = x
@inline value(::Type{C}, ::Type{X}) where {C,X} = X
@inline value(::C, x::Meta{C}) where {C<:Context} = x.value
@inline value(::Type{C}, ::Type{X}) where {C<:Context,V,X<:Meta{C,V}} = V

@inline meta(::C, x::Meta{C}) where {C<:Context} = x.meta

@generated function lowercall(f, ctx::Context, args...)
    valargs = [:(value(ctx, args[$i])) for i in 1:nfields(args)]
    return quote
        $(Expr(:meta, :inline))
        value(ctx, f)($(valargs...))
    end
end

##################
# @context macro #
##################

macro context(Ctx)
    @assert isa(Ctx, Symbol) "context name must be a Symbol"
    name = Expr(:quote, Ctx)
    return esc(quote
        struct $Ctx{T} <: $Cassette.Context{$name,T}
            tag::$Cassette.Tag{T}
        end
        @inline $Ctx(x) = $Ctx($Cassette.Tag(x))
        $Cassette.@hook $Ctx f(args...) = nothing
        $Cassette.@execution ctx::$Ctx f(args...) = $Cassette.lowercall(f, ctx, args...)
    end)
end

###############
# @hook macro #
###############

function _hook end

@inline hook(::Val{world}, ctx::Context, f, args...) where {world} = _hook(ctx, f, args...)

macro hook(ctx, def)
    return contextual_transform!(ctx, :($Cassette._hook), def)
end

####################
# @execution macro #
####################

function _execution end

@inline execution(::Val{world}, ctx::Context, f, args...) where {world} = _execution(ctx, f, args...)

macro execution(ctx, def)
    return contextual_transform!(ctx, :($Cassette._execution), def)
end

######################
# @isprimitive macro #
######################

@inline _isprimitive(args...) = Val(false)

# passing world age here forces recompilation
@generated function isprimitive(::Val{world}, ctx::Context, f::F, args...) where {world,F}
    if F.name.module == Core || F <: Core.Builtin
        body = :(Val(true))
    else
        body = :($Cassette._isprimitive(ctx, f, args...))
    end
    return quote
        $(Expr(:meta, :inline))
        $(body)
    end
end

macro isprimitive(ctx, signature)
    body = Expr(:block)
    push!(body.args, :(return Val(true)))
    return contextual_transform!(ctx, :($Cassette._isprimitive), signature, body)
end

####################
# @primitive macro #
####################

macro primitive(ctx, def)
    @assert is_method_definition(def)
    signature = deepcopy(first(def.args))
    return esc(quote
        $Cassette.@execution $ctx $def
        $Cassette.@isprimitive $ctx $signature
    end)
end

#############
# utilities #
#############

macro Meta(args...)
    error("cannot use @Meta macro outside of the scope of @execution, @isprimitive, ",
          "@primitive or @hook method definitions.")
end

function contextual_transform!(ctx, f, method)
    @assert is_method_definition(method)
    signature, body = method.args
    return contextual_transform!(ctx, f, signature, body)
end

function contextual_transform!(ctx, f, signature::Expr, body::Expr)
    @assert is_valid_ctx_specification(ctx) "invalid context specifier: $ctx. Valid syntax is `ContextType` or `context_name::ContextType`."

    if signature.head != :where
        signature = Expr(:where, signature)
    end

    ctxtypevar = gensym("ContextTypeVar")
    if isa(ctx, Expr) && ctx.head == :(::)
        ctxtype = last(ctx.args)
        ctx.args[end] = ctxtypevar
    else
        ctxtype = ctx
        ctx = :(::$(ctxtypevar))
    end
    push!(signature.args, :($ctxtypevar <: $ctxtype))

    callargs = signature.args[1].args
    for i in 1:length(callargs)
        x = callargs[i]
        if isa(x, Expr) && x.head == :(::)
            xtype = last(x.args)
            if isa(xtype, Expr) && xtype.head == :macrocall && xtype.args[1] == Symbol("@Meta")
                metaargs = xtype.args[3:end]
                if isempty(metaargs)
                    U, M = :Any, :Any
                elseif length(metaargs) == 1
                    U, M = first(metaargs), :Any
                elseif length(metaargs) == 2
                    U, M = metaargs
                else
                    error("incorrect usage of `@Meta`: $(xtype)")
                end
                new_xtype = :($Cassette.Meta{$ctxtypevar,<:Any,<:$M,<:$U})
            else
                new_xtype = :(Union{$Cassette.Meta{<:Any,<:Any,<:Any,<:$xtype},$xtype})
            end
            x.args[end] = new_xtype
        end
    end

    signature.args[1] = Expr(:call, f, ctx, callargs...)

    unshift!(body.args, Expr(:meta, :inline))

    return esc(Expr(:function, signature, body))
end

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

function is_valid_ctx_specification(x)
    if isa(x, Expr)
        T = x.head == :(::) ? last(x.args) : x
        return is_valid_ctx_type(T)
    end
    return isa(x, Symbol)
end

is_valid_ctx_type(x::Symbol) = true
is_valid_ctx_type(x) = isa(x, Expr) && x.head == :(.) && is_valid_ctx_type(unquote(last(x.args)))

unquote(x) = x
unquote(x::QuoteNode) = x.value
unquote(x::Expr) = x.head == :quote ? first(x.args) : x

function unqualify_name(e::Expr)
    @assert e.head == :(.)
    return unqualify_name(last(e.args))
end

unqualify_name(name::Symbol) = name
