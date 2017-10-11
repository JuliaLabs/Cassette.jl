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
@inline unwrap(::Type{<:Context}, ::Type{X}) where {X} = X
@inline unwrap(::Type{<:Context}, ::Type{Union{}}) = Union{}
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

#################
# AbstractField #
#################

abstract type AbstractField{name} end

const FieldList = Tuple{Vararg{AbstractField}}

struct Field{name,D,C<:FieldList} <: AbstractField{name}
    data::D
    children::C
    @inline Field{name}(data::D, children::C = ()) where {name,D,C} = new{name,D,C}(data, children)
end

@inline name(::Type{<:AbstractField{name}}) where {name} = name

###########
# Wrapper #
###########

struct Wrapper{C<:Context,U,V,M,F<:FieldList} <: AbstractWrapper{C,U,V,M}
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
            fname = Expr(:quote, fnames[i])
            push!(fields.args, :(Field{$fname}(args[$i].meta, args[$i].fields)))
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

##################
# MutableWrapper #
##################

# struct MutableWrapper{V,M<:Tuple{Vararg{MutableField}}}
#     value::V
#     meta::M
#     @generated function MutableWrapper(value::V) where {V}
#         meta = Expr(:tuple)
#         for name in fieldnames(V)
#             quoted_name = Expr(:quote, name)
#             push!(meta.args, :(MutableField{$quoted_name}(Box{metatype(value.$name)}()), }()))
#         end
#         return quote
#             $(Expr(:meta, :inline))
#             meta = $meta
#             return $(Expr(:new, :(MutableWrapper{V,typeof(meta)}), :value, :meta))
#         end
#     end
# end
#
# metatype(x) = Float64

############################
# Core Method Replacements #
############################

@generated function _new(::Type{T}, args...) where {T}
    return quote
        $(Expr(:meta, :inline))
        $(Expr(:new, T, [:(args[$i]) for i in 1:nfields(args)]...))
    end
end

@inline _getfield(value, ::Val{fieldname}) where {fieldname} = getfield(value, fieldname)

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

@inline _setfield!(value, ::Val{fieldname}, x) where {fieldname} = setfield!(value, fieldname, x)

@generated function _setfield!(w::Wrapper{C,U,V,M,F}, ::Val{fieldname}, x) where {C,U,V,M,F,fieldname}
    if !(x <: Wrapper{C})
        for (i, fieldtype) in enumerate(F.parameters)
            if name(fieldtype) == fieldname
                return quote
                    $(Expr(:meta, :inline))
                    # TODO
                end
            end
        end
        return quote
            $(Expr(:meta, :inline))
            setfield!(w.value, $fieldname, x)
        end
    else

    end
end

##################
# Notes/Examples #
##################

#=

# TODO

Q: How to make `Wrapper` suitable for mutable structs?

A: `MutableField{name,D,C} = Field{name,Box{D},C}` + wrapper preallocation for non-isbits
types using an overloadable `metatype` function. This approach is nice because it could
also work for per-field mutability, if Julia ever gets that feature in the future.

Some fiddling around will be required to make the various constructors well-behaved; see the
mocked-up MutableWrapper above.

```
mutable struct Foo
    x::Union{String, Foo}
    y::Union{Int, Foo}
end

w = Wrapper(Foo("v", 1))
w.x = "v2"
w == Wrapper(Foo("v2", 1), nothing)
w.x = Wrapper("v", "m")
w == Wrapper(Foo("v", 1), nothing; x => "m")
```
=#

#=

Q: Should this be made into a `WrapperStruct` type and then have a seperate
`Wrapper` type that represents the "base" case (would be equivalent to the current `Wrapper`
when `isempty(w.fields)`)? I feel like the answer is yes, but I'm not sure. This current
`Wrapper` struct is a bit overpowered - the `meta` field should only be inflated at the
base case anyway, and downstream authors should only be able to dispatch on base case
`Wrapper`s.

A: See the below example. A single `Wrapper` approach is way simpler - this may be a
case of "make your data structures slightly more complicated in order to vastly simplify
your algorithms." The point about dispatch is still valid, though. We could just disallow
dispatch on a metatype of `Void` somehow - maybe add an 'isactive' field that can be
dispatched on?

```
mutable struct Foo
    x::Union{String, Foo}
    y::Union{Int, Foo}
end

# current way
w = Wrapper("v", "m")    # ≈ Wrapper("v", "m")
foo = Foo(w, 1)          # ≈ Wrapper(Foo("v", 1), nothing; x => "m")
foo2 = Foo("ha", foo)    # ≈ Wrapper(Foo("ha", Foo("v", 1)), nothing; y => x => "m")
w2 = Wrapper(foo2, "m2") # ≈ Wrapper(Foo("ha", Foo("v", 1)), "m2"; y => x => "m")
foo3 = Foo(w2, 3)        # ≈ Wrapper(Foo(Foo("ha", Foo("v", 1)), 3), nothing; x => y => x => "m", x => "m2")

# proposed way
w = Wrapper("v", "m")    # ≈ Wrapper("v", "m")
foo = Foo(w, 1)          # ≈ WrapperStruct(Foo("v", 1); x => "m")
foo2 = Foo("ha", foo)    # ≈ WrapperStruct(Foo("ha", Foo("v", 1)); y => x => "m")
w2 = Wrapper(foo2, "m2") # ≈ Wrapper(WrapperStruct(Foo("ha", Foo("v", 1)); y => x => "m"), "m2")
foo3 = Foo(w2, 3)        # ≈ WrapperStruct(Foo(Foo("ha", Foo("v", 1)), 3); x => y => x => "m", x => "m2")
```
=#
