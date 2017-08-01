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
@generated function Tag(::V, ::Val{C}) where {V,C}
    @assert isa(C, Symbol)
    T = hash(C, hash(Symbol(V)))
    return quote
        $(Expr(:meta, :inline))
        Tag{$T}()
    end
end

###################
# AbstractContext #
###################

abstract type AbstractContext{V0,V,T} end

macro context(C)
    return esc(quote
        struct $C{V0,V,T} <: $Cassette.AbstractContext{V0,V,T}
            value::V
            tag::$Cassette.Tag{T}
            @inline function $C(value::V, tag::$Cassette.Tag{T}) where {V,T}
                return new{V,V,T}(value, tag)
            end
            @inline function $C(v::Type{V}, tag::$Cassette.Tag{T}) where {V,T}
                return new{Type{V},Type{V},T}(v, tag)
            end
            @inline function $C(value::$Cassette.AbstractContext{V0},
                                tag::$Cassette.Tag{T}) where {V0,T}
                return new{V0,typeof(value),T}(value, tag)
            end
        end
        @inline $C(value) = $C(value, $Tag(value, Val{$(Expr(:quote, C))}()))
        @inline $Cassette.box(c::$C, value) = $C(value, c.tag)
        @inline $Cassette.box(c::$C{<:Any,<:Any,T}, x::$C{<:Any,<:Any,T}) where {T} = x
    end)
end

@inline box() = error("this stub only exists to be extended by Cassette.@defcontext")

@inline unbox(x) = x
@inline unbox(c::AbstractContext) = c.value
@inline unbox(::Type{C}) where {V0,V,C<:AbstractContext{V0,V}} = V

@inline unbox(::AbstractContext, x) = x
@inline unbox(::AbstractContext{<:Any,<:Any,T}, c::AbstractContext{<:Any,<:Any,T}) where {T} = unbox(c)
@inline unbox(::AbstractContext{<:Any,<:Any,T}, ::Type{C}) where {V0,V,T,C<:AbstractContext{V0,V,T}} = unbox(C)

@inline unboxcall(f::AbstractContext, args...) = call(x -> unbox(f, x), unbox(f), args...)
