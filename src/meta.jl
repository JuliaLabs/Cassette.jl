################
# AbstractMeta #
################

abstract type AbstractMeta{C<:Context,V,M,U} end

@inline value(::Context, x) = x
@inline value(::Type{C}, ::Type{Union{}}) where {C<:Context} = Union{}
@inline value(::Type{C}, ::Type{X}) where {C<:Context,V,X<:AbstractMeta{C,V}} = V
@inline value(::Type{Type{C}}, ::Type{Type{X}}) where {C<:Context,X} = value(C, X)
@inline value(::Type{Type{C}}, ::Type{X}) where {C<:Context,X} = value(C, X)
@inline value(::Type{C}, ::Type{Type{X}}) where {C<:Context,X} = value(C, X)

@generated function lowercall(f, ctx::Context, args...)
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
@inline meta(ctx::Context) = ctx.meta

#################
# MetaContainer #
#################

struct MetaContainer{C<:Context,V,M,U} <: AbstractMeta{C,V,M,U}
    context::C
    value::V
    meta::RefValue{M}
    @inline function MetaContainer(context::C, value::V) where {C,V}
        M = meta_containertype(C, V)
        return new{C,V,M,V}(kind, context, value, RefValue{M}())
    end
    @inline function MetaContainer(context::C, value::MetaContainer{<:Context,<:Any,<:Any,U}) where {C,U}
        M = meta_containertype(C, typeof(value))
        return new{C,typeof(value),M,U}(kind, context, value, RefValue{M}())
    end
end

# containerize #
#--------------#

@inline containerize(ctx::Context, x::Tuple) = containerize.(ctx, x)
@inline containerize(ctx::Context, x::Array) = MetaContainer(ctx, x)
@inline containerize(ctx::Context, x::MetaContainer) = MetaContainer(ctx, x)
@inline containerize(ctx::C,       x::MetaContainer{C}) where {C<:Context} = x
@inline containerize(ctx::Context, x) = x

# @inline function containerize(ctx::Context, x)
#     if isimmutable(x)
#         return x
#     else
#         return MetaContainer(ctx, x)
#     end
# end

# MetaContainer{<:Context,<:Array} #
#----------------------------------#

@inline function meta_containertype(::Type{C}, ::Type{A}) where {C<:Context,T,A<:Array{T}}
    return meta_arraytype(A, meta_eltype(C, T))
end

@inline meta_eltype(C, T) = Void

@inline meta_arraytype(::Type{<:Array}, ::Type{Void}) = Void

@inline meta_arraytype(::Type{A}, ::Type{M}) where {T,N,A<:Array{T,N},M} = Array{M,N}

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
