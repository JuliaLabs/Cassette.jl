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
@inline value(::Type{C}, ::Type{Union{}}) where {C<:Context} = Union{}
@inline value(::Type{Type{C}}, ::Type{Type{X}}) where {C<:Context,X<:Meta} = value(C, X)
@inline value(::Type{Type{C}}, ::Type{X}) where {C<:Context,X<:Meta} = value(C, X)
@inline value(::Type{C}, ::Type{Type{X}}) where {C<:Context,X<:Meta} = value(C, X)

@inline meta(::C, x::Meta{C}) where {C<:Context} = x.meta
@inline meta(ctx::Context) = ctx.meta

@generated function lowercall(f, ctx::Context, args...)
    valargs = [:(value(ctx, args[$i])) for i in 1:nfields(args)]
    return quote
        $(Expr(:meta, :inline))
        value(ctx, f)($(valargs...))
    end
end

###############################################
# method stubs for macros (see src/macros.jl) #
###############################################

function _hook end

@inline hook(::Val{world}, ctx::Context, f, args...) where {world} = _hook(ctx, f, args...)

function _execution end

@inline execution(::Val{world}, ctx::Context, f, args...) where {world} = _execution(ctx, f, args...)

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
