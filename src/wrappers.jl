###################
# AbstractWrapper #
###################

"""
    AbstractWrapper{C<:Context,U,V,M}
The supertype for a variety of metadata wrapper types.
- `C` is the type of the context with which the metadata is associated
- `U` is the type of the fully unwrapped underlying value, e.g. in the case of nested
contexts/metadata propagation. This is necessary to allow context creators to dispatch
transparently on the underlying value type, i.e. it ensures that nested metadata wrapping
doesn't interfere with contextual dispatch.
- `V` is the type of the directly wrapped value, i.e. `value(ctx::C, w::AbstractWrapper{C,U,V})::V`
- `M` is the type of the metadata contained in the wrapper
"""
abstract type AbstractWrapper{C<:Context,U,V,M} end

@inline unwrap(::Context, x) = x
@inline unwrap(::Type{C}, ::Type{Union{}}) where {C<:Context} = Union{}
@inline unwrap(::Type{C}, ::Type{<:AbstractWrapper{C,U,V}}) where {C<:Context,U,V} = V
@inline unwrap(::Type{Type{C}}, ::Type{Type{X}}) where {C<:Context,X} = unwrap(C, X)
@inline unwrap(::Type{Type{C}}, ::Type{X}) where {C<:Context,X} = unwrap(C, X)
@inline unwrap(::Type{C}, ::Type{Type{X}}) where {C<:Context,X} = unwrap(C, X)

@generated function unwrapcall(f, ctx::Context, args...)
    unwrapped_args = [:(unwrap(ctx, args[$i])) for i in 1:nfields(args)]
    return quote
        $(Expr(:meta, :inline))
        unwrap(ctx, f)($(unwrapped_args...))
    end
end

###########
# Wrapper #
###########
# TODO: Should this be made into a `WrapperStruct` type and then have a seperate `Wrapper`
# type that represents the "base" case (would be equivalent to the current `Wrapper`
# when `isempty(w.fields)`)? I feel like the answer is yes, but I'm not sure. This current
# `Wrapper` struct is a bit overpowered - the `meta` field should only be inflated at the
# base case anyway, and downstream authors should only be able to dispatch on base case
# `Wrapper`s...
#
# ...right?


# Field #
#-------#

# This only exists to work around the inability to specify self-referential parameter
# contraints in type definitions (i.e. in the `Field` definition below)
abstract type AbstractField end

struct Field{n,D,C<:Tuple{Vararg{AbstractField}}} <: AbstractField
    name::Val{n}
    data::D
    children::C
end

name(::Type{<:Field{n}}) where {n} = n

# Wrapper #
#---------#

struct Wrapper{C<:Context,U,V,M,F<:Tuple{Vararg{Field}}} <: AbstractWrapper{C,U,V,M}
    context::C
    value::V
    meta::M
    fields::F
    function Wrapper(context::C, value::V, meta::M, fields::F) where {C,V,M,F}
        return new{C,V,V,M,F}(context, value, meta, fields)
    end
    function Wrapper(context::C, value::Wrapper{<:Context,U}, meta::M, fields::F) where {C,U,M,F}
        return new{C,U,typeof(value),M,F}(context, value, meta, fields)
    end
end

@inline Wrapper(ctx::Context, value, meta = nothing) = Wrapper(ctx, value, meta, ())

@inline unwrap(::C, w::Wrapper{C}) where {C<:Context} = w.value

@inline meta(::C, w::Wrapper{C}) where {C<:Context} = w.meta

# Keep in mind this only works if `T`'s parameters (and `T` itself) are stripped of
# `Wrapper` types, which hopefully occurs naturally earlier in the call chain anyway.
@generated function wrapnew(ctx::C, meta, ::Type{T}, args...) where {C<:Context,T}
    _new_args = Any[]
    fields = Expr(:tuple)
    fnames = fieldnames(T)
    for (i, arg) in enumerate(args)
        if arg <: Wrapper{C}
            fname = :(Val($(Expr(:quote, fnames[i]))))
            push!(fields.args, :(Field($fname, args[$i].meta, args[$i].fields)))
            push!(_new_args, :(args[$i].value))
        else
            push!(_new_args, :(args[$i]))
        end
    end
    return quote
        $(Expr(:meta, :inline))
        Wrapper(ctx, _new(T, $(_new_args...)), meta, $fields)
    end
end

############################
# Core Method Replacements #
############################

@generated function _new(::Type{T}, args...) where {T}
    return quote
        $(Expr(:meta, :inline))
        $(Expr(:new, T, [:(args[$i]) for i in 1:nfields(args)]...))
    end
end

@inline _getfield(x, ::Val{fieldname}) where {fieldname} = getfield(x, fieldname)

@generated function _getfield(w::Wrapper{C,U,V,M,F}, ::Val{fieldname}) where {C,U,V,M,F,fieldname}
    for (i, fieldtype) in enumerate(F.parameters)
        if name(fieldtype) == fieldname
            return quote
                $(Expr(:meta, :inline))
                field = w.fields[$i]::$fieldtype
                Wrapper(getfield(w.value, $fieldname), field.data, field.children)
            end
        end
    end
    return quote
        $(Expr(:meta, :inline))
        getfield(w.value, $fieldname)
    end
end
