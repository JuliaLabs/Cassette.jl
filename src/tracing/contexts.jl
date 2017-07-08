abstract type AbstractContext{V,L,C} end

# TODO: Need more unique tag; still vulnerable to context confusion, e.g. basic pertubation
# confusion examples would still fail with only the level tag below
macro defcontext(C)
    return esc(quote
        struct $C{V,L} <: $Cassette.AbstractContext{V,L,$(Expr(:quote, C))}
            value::V
        end
        Base.@inline $C(value::V) where {V} = $C{V,1}(value)
        Base.@inline $C(value::Type{T}) where {T} = $C{Type{T},1}(T)
        Base.@inline $C(value::$Cassette.AbstractContext) = $Cassette.construct_nested_context($C, value)
        Base.@inline $Cassette.box(c::$C, value) = $C(value)
        Base.@inline $Cassette.box(c::$C, ::Type{T}) where {T} = $C(T)
    end)
end

@generated function construct_nested_context(::Type{C}, value::AbstractContext{<:Any,L}) where {C,L}
    return quote
        $(Expr(:meta, :inline))
        C{typeof(value),$(L+1)}(value)
    end
end

@inline box() = error("this stub only exists to be extended by Cassette.@defcontext")

@inline unbox(x) = x
@inline unbox(c::AbstractContext) = c.value
@inline unbox(::Type{C}) where {V,C<:AbstractContext{V}} = V

@inline contextual_unbox(c, ::AbstractContext) = c
@inline contextual_unbox(c::AbstractContext{<:Any,L,C}, ::AbstractContext{<:Any,L,C}) where {L,C} = unbox(c)

@generated function unboxcall(f, args...)
    call = Expr(:call, :(unbox(f)), [:(contextual_unbox(args[$i], f)) for i in 1:nfields(args)]...)
    return quote
        $(Expr(:meta, :inline))
        $call
    end
end
