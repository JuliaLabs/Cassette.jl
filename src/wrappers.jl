###########
# Wrapper #
###########

struct Uninitialized end

struct Meta{D,F}
    data::D
    fields::Anonymous{F}
end

Meta(u::Uninitialized, fields::Anonymous) = u

Base.show(io::IO, m::Meta) = print(io, "Meta(", repr(m.data), ", ", m.fields, ")")

struct Wrapper{C,U,V,M,F}
    context::C
    value::V
    meta::Meta{M,F}
    function Wrapper(context::C, value::V, meta::Meta{M,F}) where {C<:Context,V,M,F}
        return new{C,V,V,M,F}(context, value, meta)
    end
    function Wrapper(context::C, value::Wrapper{<:Context,U}, meta::Meta{M,F}) where {C<:Context,U,M,F}
        return new{C,U,typeof(value),M,F}(context, value, meta)
    end
end

Wrapper(context::Context, value, ::Uninitialized) = value
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

@inline meta(::C, x::Wrapper{C}) where {C<:Context} = x.meta.data

#########
# `new` #
#########

# TODO: better metadata types for mutable fields (instead of just `Any`)
@generated function wrapper_new(ctx::C, meta, ::Type{T}, args...) where {C<:Context,T}
    _new_args = Any[]
    fields = Any[]
    fnames = fieldnames(T)
    if T.mutable
        for (i, arg) in enumerate(args)
            fname = fnames[i]
            if arg <: Wrapper{C}
                push!(fields, :(mut($fname::Any = args[$i].meta)))
                push!(_new_args, :(args[$i].value))
            else
                push!(fields, :(mut($fname::Any)))
                push!(_new_args, :(args[$i]))
            end
        end
    else
        for (i, arg) in enumerate(args)
            if arg <: Wrapper{C}
                fname = fnames[i]
                push!(fields, :($fname = args[$i].meta))
                push!(_new_args, :(args[$i].value))
            else
                push!(_new_args, :(args[$i]))
            end
        end
    end
    return quote
        $(Expr(:meta, :inline))
        Wrapper(ctx, _new(T, $(_new_args...)), Meta(meta, @anon($(fields...))))
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

@generated function _getfield(w::Wrapper{C,U,V,M,F}, name::Name{n}) where {C,U,V,M,F,n}
    for fieldtype in F.parameters
        if nametype(fieldtype) === n
            return quote
                $(Expr(:meta, :inline))
                Wrapper(w.context, _getfield(w.value, name), _getfield(w.meta.fields, name))
            end
        end
    end
    return quote
        $(Expr(:meta, :inline))
        _getfield(w.value, name)
    end
end

@generated function _setfield!(w::Wrapper{C,U,V,M,F}, name::Name{n}, x) where {C,U,V,M,F,n}
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

##################
# Notes/Examples #
##################

#=
using Cassette: unwrap, @context, Wrapper, wrapper_new, @anon, Meta, @getfield, @setfield!

struct Bar{X,Y,Z}
    x::X
    y::Y
    z::Z
end

mutable struct Foo
    a::Bar{Int}
    b
end

@context Ctx

ctx = Ctx(nothing)
bar = Wrapper(ctx, Bar(1,2,3), "barmeta")
foo = wrapper_new(ctx, nothing, Foo, bar, :b)

@getfield(foo, a) === bar

@setfield!(foo, a, Bar(4, 5, 6))

@getfield(foo, a) === Bar(4, 5, 6)

@setfield!(foo, b, Wrapper(ctx, "ha", "laughmeta"))
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
