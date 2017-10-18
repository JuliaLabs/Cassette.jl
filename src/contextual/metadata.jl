###########
# Wrapper #
###########

abstract type MetaStatus end
struct Inactive <: MetaStatus end
struct Active   <: MetaStatus end

struct Meta{S<:MetaStatus,D,F}
    status::S
    data::D
    fields::Anonymous{F}
    Meta(data::D, fields::Anonymous{F}) where {D,F} = new{Inactive,D,F}(Inactive(), data, fields)
    Meta(data::D, fields::Anonymous{F}) where {D,F<:Tuple{}} = new{Active,D,F}(Active(), data, fields)
end

Base.show(io::IO, m::Meta{S}) where {S} = print(io, "Meta{$(S.name.name)}(", repr(m.data), ", ", m.fields, ")")

struct Wrapper{C,U,V,S,M,F}
    context::C
    value::V
    meta::Meta{S,M,F}
    function Wrapper(context::C, value::V, meta::Meta{S,M,F}) where {C<:Context,V,S,M,F}
        return new{C,V,V,S,M,F}(context, value, meta)
    end
    function Wrapper(context::C, value::Wrapper{<:Context,U}, meta::Meta{S,M,F}) where {C<:Context,U,S,M,F}
        return new{C,U,typeof(value),S,M,F}(context, value, meta)
    end
end

Wrapper(context::Context, value, metadata = nothing) = Wrapper(context, value, Meta(metadata, @anon()))

Base.show(io::IO, w::Wrapper) = print(io, "Wrapper(", repr(w.value), ", ", w.meta, ")")

##################
# safe accessors #
##################

@inline unwrap(::Context, x) = x
@inline unwrap(::C,       x::Wrapper{C}) where {C<:Context} = x.value
@inline unwrap(::Type{C}, T::Type{X}) where {C<:Context,X} = T
@inline unwrap(::Type{C}, ::Type{<:Wrapper{C,U,V}}) where {C<:Context,U,V} = V

@generated function unwrapcall(f, ctx::Context, args...)
    unwrapped_args = [:(unwrap(ctx, args[$i])) for i in 1:nfields(args)]
    return quote
        $(Expr(:meta, :inline))
        unwrap(ctx, f)($(unwrapped_args...))
    end
end

@inline meta(::C, x::Wrapper{C,<:Any,<:Any,Active}) where {C<:Context} = x.meta.data

#########
# `new` #
#########

# TODO: better metadata types for mutable fields (instead of just `Any`)
@generated function wrapper_new(ctx::C, ::Type{T}, args...) where {C<:Context,T}
    _new_args = Any[]
    fields = Any[]
    fnames = fieldnames(T)
    for (i, arg) in enumerate(args)
        fname = fnames[i]
        if T.mutable
            if arg <: Wrapper{C}
                push!(fields, :(mut($fname::Any = args[$i].meta)))
                push!(_new_args, :(args[$i].value))
            else
                push!(fields, :(mut($fname::Any)))
                push!(_new_args, :(args[$i]))
            end
        else
            if arg <: Wrapper{C}
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
            Wrapper(ctx, _new(T, $(_new_args...)), Meta(nothing, @anon($(fields...))))
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

struct Uninitialized end

@inline wrapper_from_getfield(ctx, value, meta) = Wrapper(ctx, value, meta)
@inline wrapper_from_getfield(ctx, value, ::Uninitialized) = value

@generated function _getfield(w::Wrapper{C,U,V,S,M,F}, name::Name{n}) where {C,U,V,S,M,F,n}
    for fieldtype in F.parameters
        if nametype(fieldtype) === n
            return quote
                $(Expr(:meta, :inline))
                value = _getfield(w.value, name)
                meta = _getfield(w.meta.fields, name)
                wrapper_from_getfield(w.context, value, meta)
            end
        end
    end
    return quote
        $(Expr(:meta, :inline))
        _getfield(w.value, name)
    end
end

@generated function _setfield!(w::Wrapper{C,U,V,S,M,F}, name::Name{n}, x) where {C,U,V,S,M,F,n}
    if x <: Wrapper{C}
        return quote
            $(Expr(:meta, :inline))
            _setfield!(w.value, name, x.value)
            _setfield!(w.meta.fields, name, x.meta)
            return x
        end
    else
        for fieldtype in F.parameters
            if nametype(fieldtype) === n
                return quote
                    $(Expr(:meta, :inline))
                    _setfield!(w.value, name, x)
                    _setfield!(w.meta.fields, name, Uninitialized())
                    return x
                end
            end
        end
        return quote
            $(Expr(:meta, :inline))
            _setfield!(w.value, name, x)
        end
    end
end
