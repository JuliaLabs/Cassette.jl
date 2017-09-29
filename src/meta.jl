################
# AbstractMeta #
################

"""
    AbstractMeta{C<:Context,V,M,U}

The supertype for a variety of metadata wrapper types.

- `C` is the type of the context with which the metadata is associated
- `V` is the type of the directly wrapped value, i.e. `value(ctx::C, m::AbstractMeta{C,V})::V`
- `M` is the type of the metadata contained in the wrapper
- `U` is the type of the fully unwrapped underlying value, e.g. in the case of nested
contexts/metadata propagation. This is necessary to allow context creators to dispatch
transparently on the underlying value type, i.e. it ensures that nested metadata wrapping
doesn't interfere with contextual dispatch.
"""
abstract type AbstractMeta{C<:Context,V,M,U} end

@inline value(::Context, x) = x
@inline value(::Type{C}, ::Type{Union{}}) where {C<:Context} = Union{}
@inline value(::Type{C}, ::Type{X}) where {C<:Context,V,X<:AbstractMeta{C,V}} = V
@inline value(::Type{Type{C}}, ::Type{Type{X}}) where {C<:Context,X} = value(C, X)
@inline value(::Type{Type{C}}, ::Type{X}) where {C<:Context,X} = value(C, X)
@inline value(::Type{C}, ::Type{Type{X}}) where {C<:Context,X} = value(C, X)

@generated function escapecall(f, ctx::Context, args...)
    valargs = [:(value(ctx, args[$i])) for i in 1:nfields(args)]
    return quote
        $(Expr(:meta, :inline))
        value(ctx, f)($(valargs...))
    end
end

#############
# MetaValue #
#############

struct MetaValue{C<:Context,V,M,U} <: AbstractMeta{C,V,M,U}
    context::C
    value::V
    meta::M
    @inline MetaValue(context::C, value::V, meta::M = nothing) where {C,V,M} = new{C,V,M,V}(context, value, meta)
    @inline MetaValue(context::C, value::Type{V}, meta::M = nothing) where {C,V,M} = new{C,Type{value},M,Type{value}}(context, value, meta)
    @inline MetaValue(context::C, value::MetaValue{<:Context,<:Any,<:Any,U}, meta::M = nothing) where {C,M,U} = new{C,typeof(value),M,U}(context, value, meta)
end

@inline value(::C, x::MetaValue{C}) where {C<:Context} = x.value
@inline meta(::C, x::MetaValue{C}) where {C<:Context} = x.meta

#################
# MetaContainer #
#################

struct MetaContainer{C<:Context,V,M,U} <: AbstractMeta{C,V,M,U}
    context::C
    value::V
    meta::RefValue{M}
    @inline function MetaContainer(context::C, cfg, value::V) where {C,V}
        M = metacontainertype(context, cfg, V)
        return new{C,V,M,V}(kind, context, value, RefValue{M}())
    end
    @inline function MetaContainer(context::C, cfg, value::MetaContainer{<:Context,<:Any,<:Any,U}) where {C,U}
        V = typeof(value)
        M = metacontainertype(context, cfg, V)
        return new{C,V,M,U}(kind, context, value, RefValue{M}())
    end
end

@inline metatype(ctx::Context, cfg, T) = Void

@inline function metacontainertype(ctx::Context, cfg, ::Type{A}) where {T,N,A<:Array{T,N}}
    M = metatype(ctx, cfg, T)
    if M <: Void
        return Void
    else
        return Array{M,N}
    end
end

# containerize #
#--------------#

@inline containerize(ctx::Context, cfg, x::Tuple) = map(i -> containerize(ctx, cfg, i), x)
@inline containerize(ctx::Context, cfg, x::Array) = MetaContainer(ctx, cfg, x)
@inline containerize(ctx::Context, cfg, x::MetaContainer) = MetaContainer(ctx, cfg, x)
@inline containerize(ctx::C,       cfg, x::MetaContainer{C}) where {C<:Context} = x
@inline containerize(ctx::Context, cfg, x) = x

# @inline function containerize(ctx::Context, x)
#     if isimmutable(x)
#         return x
#     else
#         return MetaContainer(ctx, x)
#     end
# end

# arrayref/arrayset #
#-------------------#

@inline meta_arrayref(args...) = Core.arrayref(args...)

@inline meta_arrayset(args...) = Core.arrayset(args...)

@inline meta_arrayref(boundscheck::Bool, x::MetaContainer{<:Context,<:Array,Void}, i) = Core.arrayref(boundscheck, x.value, i)

@inline meta_arrayset(boundscheck::Bool, x::MetaContainer{<:Context,<:Array,Void}, y, i) = Core.arrayset(boundscheck, x.value, y, i)

@inline function meta_arrayref(boundscheck::Bool, x::MetaContainer{C,<:Array{V},<:Array{M}}, i) where {C<:Context,V,M}
    if isassigned(x.meta)
        y = MetaValue(x.context, Core.arrayref(boundscheck, x.value, i), Core.arrayref(boundscheck, x.meta[], i))
    else
        y = Core.arrayref(boundscheck, m.value, i)
    end
    return y::Union{V,MetaValue{C,V,M}}
end

@inline function meta_arrayset(boundscheck::Bool, x::MetaContainer{C,<:Array,<:Array{M}}, y::MetaValue{C}, i) where {C<:Context,M}
    if !(isassigned(x.meta))
        x.meta[] = similar(x.value, M)
    end
    Core.arrayset(boundscheck, x.value, value(x.context, y), i)
    Core.arrayset(boundscheck, x.meta[], meta(x.context, y), i)
    return x
end
