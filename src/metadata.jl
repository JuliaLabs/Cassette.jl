################
# FieldStorage #
################

abstract type FieldStorage{D} end

# Mutable #
#---------#

mutable struct Mutable{D} <: FieldStorage{D}
    data::D
    Mutable{D}(data) where D = new{D}(data)
end

@inline Mutable(data::D) where {D} = Mutable{D}(data)

@inline Base.getindex(x::Mutable) = x.data

@inline Base.setindex!(x::Mutable, y) = (x.data = y)

# Immutable #
#-----------#

struct Immutable{D} <: FieldStorage{D}
    data::D
end

@inline Base.getindex(x::Immutable) = x.data

@inline Base.setindex!(x::Immutable, y) = error("cannot mutate immutable field")

###########################
# metadata type hierarchy #
###########################
#=
TODO: clean up/formalize these notes

The optimal dualtype specification for a given non-leaftype is the union of the dualtypes of
its subtypes, and thus can be implemented as:

```
function dualtype(::Type{C}, ::Type{T}) where {C,T}
    if isempty(subtypes(T))
        return metatype(C, T)
    else
        return Union{map(X -> dualtype(C, X), subtypes(T))...}
    end
end
```

However, this is an expensive implementation of this operation. Thus, our actual `dualtype`
implementation returns a still-correct, but an extremely pessimistic dualtype with the
benefit that dualtype computation is very fast. If, in the future, `dualtype` is
parameterized on world age, then we can call `subtypes` at compile time, and compute a more
optimally bounded dualtype.
=#

const MetaTree = Union{NamedTuple,Array,Unused}

struct MetaNode{D,T<:MetaTree}
    data::Union{D,Unused}
    tree::T
    MetaNode{D}(data, tree::T) where {D,T} = new{D,T}(data, tree)
end

@inline function metanodetype(::Type{C}, ::Type{T}) where {C<:Context,T}
    if isconcretetype(T)
        return Union{MetaNode{metatype(C, T),subtreetype(C, T)},Unused}
    end
    return Union{MetaNode,Unused}
end

@generated function subtreetype(::Type{C}, ::Type{T}) where {C,T}
    if !(isconcretetype(T))
        return :(error("cannot call subtreetype on non-concrete type ", $T))
    end
    if T <: Array
        body = :(Array{metanodetype(C, $(eltype(T))),$(ndims(T))})
    elseif fieldcount(T) == 0
        body = :(Unused)
    else
        S = isimmutable(T) ? :Immutable : :Mutable
        fnames = Expr(:tuple, map(Meta.quot, fieldnames(T))...)
        ftypes = map(F -> :($S{metanodetype(C, $F)}), T.types)
        body = :(NamedTuple{$fnames,Tuple{$(ftypes...)}})
    end
    return quote
        $(Expr(:meta, :inline))
        $body
    end
end

@generated function initmetanode(::C, ::V, metadata::D) where {C<:Context,V,D}
    if V <: Array
        return quote
            $(Expr(:meta, :inline))
            MetaNode{metatype(C, V)}(metadata, similar(value, metanodetype(C, eltype(V))))
        end
    elseif fieldcount(V) == 0
        return quote
            $(Expr(:meta, :inline))
            MetaNode{metatype(C, V)}(metadata, unused)
        end
    else
        S = isimmutable(V) ? :Immutable : :Mutable
        fnames = fieldnames(V)
        fdatas = map(F -> :($S{metanodetype(C, $F)}(unused)), V.types)
        fields = Expr(:tuple)
        for i in 1:fieldcount(V)
            push!(fields.args, :($(fnames[i]) = $(fdatas[i])))
        end
        return quote
            $(Expr(:meta, :inline))
            MetaNode{metatype(C, V)}(metadata, $fields)
        end
    end
end

#######
# Box #
#######

#=
Here, `U` is the innermost, "underlying" type of the value being wrapped. This parameter is
precomputed so that Cassette can directly dispatch on it in the signatures generated for
contextual primitives.
=#
struct Box{C,U,V,D,T}
    context::C
    value::V
    meta::MetaNode{D,T}
    function Box(context::C, value::V, meta::MetaNode{D,T}) where {C<:Context,V,D,T}
        U = _underlying_type(V)
        return new{C,U,V,D,T}(context, value, meta)
    end
end

function Box(context::C, value, metadata = unused) where {C<:Context}
    return Box(context, value, initmetanode(context, value, metadata))
end

_underlying_type(::Type{V}) where {V} = V
_underlying_type(::Type{<:Box{<:Context,U}}) where {U} = U

#######
# API #
#######

@inline isboxed(::Context, ::Any) = false
@inline isboxed(::C,       ::Box{C}) where {C<:Context} = true
@inline isboxed(::Type{C}, ::DataType) where {C<:Context} = false
@inline isboxed(::Type{C}, ::Type{<:Box{C}}) where {C<:Context} = true

@inline unbox(ctx::Context, x) = isboxed(ctx, x) ? x.value : x
@inline unbox(::Type{C},    T::DataType) where {C<:Context,X} = T
@inline unbox(::Type{C},    ::Type{<:Box{C,U,V}}) where {C<:Context,U,V} = V

@inline meta(::C,       x::Box{C}) where {C<:Context} = x.meta.data
@inline meta(::Type{C}, ::Type{<:Box{C,U,V,D}}) where {C<:Context,U,V,D} = D

@inline hasmeta(::Context, ::Any) = false
@inline hasmeta(ctx::C,    x::Box{C}) where {C<:Context} = isa(meta(ctx, x), Unused)

@inline metatype(::Type{<:Context}, ::DataType) = Unused

@generated function unboxcall(ctx::Context, f, args...)
    N = fieldcount(typeof(args))
    return quote
        $(Expr(:meta, :inline))
        $(Expr(:call, :f, [:(unbox(ctx, args[$i])) for i in 1:N]...))
    end
end

#########
# `new` #
#########

@generated function _newbox(ctx::C, ::Type{T}, args...) where {C<:Context,T}
    unboxed_args = [:(unbox(ctx, args[$i])) for i in 1:length(args)]
    if length(args) == fieldcount(T)
        fields = Expr(:tuple)
        fnames = fieldnames(T)
        boxcount = 0
        S = isimmutable(T) ? :Immutable : :Mutable
        for i in 1:fieldcount(T)
            arg = args[i]
            fname = fnames[i]
            ftype = :(metanodetype(C, $(T.types[i])))
            if arg <: Box{C}
                boxcount += 1
                argmetanode = :(args[$i].meta)
            else
                argmetanode = :unused
            end
            push!(fields.args, :($fname = $S{$ftype}($argmetanode)))
        end
        if !(boxcount == 0 && isimmutable(T))
            return quote
                $(Expr(:meta, :inline))
                Box(ctx, _new(T, $(unboxed_args...)), MetaNode{metatype(C, T)}(unused, $fields))
            end
        end
    end
    return quote
        $(Expr(:meta, :inline))
        _new(T, $(unboxed_args...))
    end
end

@generated function _new(::Type{T}, args...) where {T}
    return quote
        $(Expr(:meta, :inline))
        $(Expr(:new, T, [:(args[$i]) for i in 1:nfields(args)]...))
    end
end

##########################
# `getfield`/`setfield!` #
##########################

@inline _getfield(x, name) = getfield(x, name)
@inline _getfield(x::Box, name) = Box(x.context, getfield(x.value, name), getfield(x.meta.tree, name)[])

@inline _setfield!(x, name, y) = setfield!(x, name, y)
@inline _setfield!(x::Box, name, y) = _setfield!(x, name, y, y, unused)
@inline _setfield!(x::Box{C}, name, y::Box{C}) where {C} = _setfield!(x, name, y, y.value, y.meta)

@inline function _setfield!(x::Box, name, y, y_value, y_meta)
    setfield!(x.value, name, y_value)
    setindex!(getfield(x.meta.tree, name), y_meta)
    return y
end

#########################
# `arrayref`/`arrayset` #
#########################

@inline _arrayref(check, x, i) = arrayref(check, x, i)
@inline _arrayref(check, x::Box, i) = Box(x.context, arrayref(check, x.value, i), arrayref(check, x.meta.tree, i))

@inline _arrayset(check, x, y, i) = arrayset(check, x, y, i)
@inline _arrayset(check, x::Box, y, i) = _arrayset(check, x, y, unused, i)
@inline _arrayset(check, x::Box{C}, y::Box{C}, i) where {C} = _arrayset(check, x, y.value, y.meta, i)

@inline function _arrayset(check, x::Box, y_value, y_meta, i)
    arrayset(check, x.value, y_value, i)
    arrayset(check, x.meta, y_meta, i)
    return x
end
