#######
# Box #
#######

struct Unused end

struct Meta{D,A,F}
    data::D
    fields::Anonymous{F}
    Meta(data::D, fields::Anonymous{F}) where {D<:Any,F} = new{D,true,F}(data, fields)
    Meta(data::D, fields::Anonymous{F}) where {D<:Unused,F} = new{D,false,F}(data, fields)
end

Base.show(io::IO, m::Meta) = print(io, "Meta(", repr(m.data), ", ", m.fields, ")")

struct Box{C,U,V,M,A,F}
    context::C
    value::V
    meta::Meta{M,A,F}
    function Box(context::C, value::V, meta::Meta{M,A,F}) where {C<:Context,V,M,A,F}
        return new{C,V,V,M,A,F}(context, value, meta)
    end
    function Box(context::C, value::Box{<:Context,U}, meta::Meta{M,A,F}) where {C<:Context,U,M,A,F}
        return new{C,U,typeof(value),M,A,F}(context, value, meta)
    end
end

Box(context::Context, value, metadata = Unused()) = Box(context, value, Meta(metadata, @anon()))

Base.show(io::IO, b::Box) = print(io, "Box(", repr(b.value), ", ", b.meta, ")")

##################
# safe accessors #
##################

@inline isboxed(::Context, x) = false
@inline isboxed(::C,       x::Box{C}) where {C<:Context} = true
@inline isboxed(::Type{C}, T::DataType) where {C<:Context} = false
@inline isboxed(::Type{C}, ::Type{<:Box{C}}) where {C<:Context} = true

@inline unbox(ctx::Context, x) = isboxed(ctx, x) ? x.value : x
@inline unbox(::Type{C}, T::DataType) where {C<:Context,X} = T
@inline unbox(::Type{C}, ::Type{<:Box{C,U,V}}) where {C<:Context,U,V} = V

@inline meta(::C, x::Box{C}) where {C<:Context} = x.meta.data
@inline meta(::Type{C}, ::Type{<:Box{C,U,V,M}}) where {C<:Context,U,V,M} = M

@inline initmetadata(::Context, ::DataType, ::Any) = Unused()
@inline initmetatype(::Type{<:Context}, ::DataType) = Any

#########
# `new` #
#########

@generated function _newbox(ctx::C, ::Type{T}, args...) where {C<:Context,T}
    _new_args = Any[]
    fields = Any[]
    fnames = fieldnames(T)
    for (i, arg) in enumerate(args)
        fname = fnames[i]
        if T.mutable
            if arg <: Box{C}
                M = initmetatype(C, meta(C, arg))
                push!(fields, :(mut($fname::$M = args[$i].meta)))
                push!(_new_args, :(args[$i].value))
            else
                M = initmetatype(C, arg)
                push!(fields, :(mut($fname::$M)))
                push!(_new_args, :(args[$i]))
            end
        else
            if arg <: Box{C}
                push!(fields, :($fname = args[$i].meta))
                push!(_new_args, :(args[$i].value))
            else
                push!(_new_args, :(args[$i]))
            end
        end
    end
    if isempty(fields)
        return quote
            $(Expr(:meta, :inline))
            _new(T, $(_new_args...))
        end
    else
        return quote
            $(Expr(:meta, :inline))
            Box(ctx, _new(T, $(_new_args...)), Meta(Unused(), @anon($(fields...))))
        end
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

@inline box_from_getfield(ctx, value, meta) = Box(ctx, value, meta)
@inline box_from_getfield(ctx, value, ::Unused) = value

@generated function _getfield(b::Box{C,U,V,M,A,F}, name::Name{n}) where {C,U,V,M,A,F,n}
    for fieldtype in F.parameters
        if nametype(fieldtype) === n
            return quote
                $(Expr(:meta, :inline))
                value = _getfield(b.value, name)
                meta = _getfield(b.meta.fields, name)
                box_from_getfield(b.context, value, meta)
            end
        end
    end
    return quote
        $(Expr(:meta, :inline))
        _getfield(b.value, name)
    end
end

@generated function _setfield!(b::Box{C,U,V,M,A,F}, name::Name{n}, x) where {C,U,V,M,A,F,n}
    if x <: Box{C}
        return quote
            $(Expr(:meta, :inline))
            _setfield!(b.value, name, x.value)
            _setfield!(b.meta.fields, name, x.meta)
            return x
        end
    else
        for fieldtype in F.parameters
            if nametype(fieldtype) === n
                return quote
                    $(Expr(:meta, :inline))
                    _setfield!(b.value, name, x)
                    T = _fieldtype(b.meta.fields, name)
                    _setfield!(b.meta.fields, name, initmetadata(b.context, T, x))
                    return x
                end
            end
        end
        return quote
            $(Expr(:meta, :inline))
            _setfield!(b.value, name, x)
        end
    end
end
