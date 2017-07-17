#######
# Tag #
#######
# Note that the code/comments here were originally used for ForwardDiff's tagging system,
# and have been modified from their original version to fit your TV - er, to fit Cassette's
# use case.

struct Tag{T} end

# Here, we could've just as easily used `hash`; however, this is unsafe/undefined behavior
# if `hash(::Type{V})` is overloaded in a module loaded after Cassette. Thus, we instead
# use `hash(Symbol(V))`, which is somewhat safer since it's far less likely that somebody
# would overwrite the Base definition for `Symbol(::DataType)` or `hash(::Symbol)`.
@generated function Tag(::V) where {V}
    T = hash(Symbol(V))
    return quote
        $(Expr(:meta, :inline))
        Tag{$T}()
    end
end

###################
# AbstractContext #
###################

abstract type AbstractContext{V0,T,C} end

macro context(C)
    return esc(quote
        struct $C{V0,T,V} <: $Cassette.AbstractContext{V0,T,$(Expr(:quote, C))}
            value::V
            tag::$Cassette.Tag{T}
            @inline function $C(value::V, tag::$Cassette.Tag{T}) where {V,T}
                return new{V,T,V}(value, tag)
            end
            @inline function $C(v::Type{V}, tag::$Cassette.Tag{T}) where {V,T}
                return new{Type{V},T,Type{V}}(v, tag)
            end
            @inline function $C(value::$Cassette.AbstractContext{V0},
                                tag::$Cassette.Tag{T}) where {V0,T}
                return new{V0,T,typeof(value)}(value, tag)
            end
        end
        @inline $C(value) = $C(value, $Tag(value))
        @inline $Cassette.box(c::$C, value) = $C(value, c.tag)
        @inline $Cassette.box(c::$C{<:Any,T}, x::$C{<:Any,T}) where {T} = x
    end)
end

@inline box() = error("this stub only exists to be extended by Cassette.@defcontext")

@inline unbox(x) = x
@inline unbox(c::AbstractContext) = c.value

@inline unbox(::AbstractContext, x) = x
@inline unbox(::AbstractContext{<:Any,T,C}, x::AbstractContext{<:Any,T,C}) where {T,C} = unbox(x)

@inline unboxcall(f::AbstractContext, args...) = call(x -> unbox(f, x), unbox(f), args...)
