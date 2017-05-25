##############
# Node Types #
##############

# FunctionNode #
#--------------#

struct FunctionNode{F,I<:Tuple}
    func::F
    input::I
end

const ROOT = FunctionNode(nothing, ())

# RealNode #
#----------#

struct RealNode{G<:AbstractGenre,V<:Real,C} <: Real
    genre::G
    value::V
    cache::C
    parent::FunctionNode
    function RealNode(genre::G, value::V, parent::FunctionNode) where {G,V}
        cache = node_cache(genre, value)
        C = typeof(cache)
        return new{G,V,C}(genre, value, cache, parent)
    end
end

# ArrayNode #
#-----------#

struct ArrayNode{G<:AbstractGenre,V<:AbstractArray,C,T<:RealNode,N} <: AbstractArray{T,N}
    genre::G
    value::V
    cache::C
    parent::FunctionNode
    function ArrayNode(genre::G, value::V, parent::FunctionNode) where {G,N,V<:AbstractArray{<:Real,N}}
        cache = node_cache(genre, value)
        C = typeof(cache)
        T = node_eltype(genre, value)
        return new{G,V,C,T,N}(genre, value, cache, parent)
    end
end

#######################
# ValueNode Interface #
#######################

const ValueNode = Union{RealNode,ArrayNode}

@inline istrackable(::Union{AbstractArray,Real}) = True()
@inline istrackable(::Any) = False()

@inline function track(value::Union{AbstractArray,Real},
                       genre::AbstractGenre = ValueGenre(),
                       parent::FunctionNode = ROOT)
    if isa(value, Real)
        return RealNode(genre, value, parent)
    elseif isa(value, AbstractArray)
        return ArrayNode(genre, value, parent)
    end
end

@inline untrack(x) = x
@inline untrack(n::RealNode) = n.value
@inline untrack(n::ArrayNode) = n.value
@inline untrack(::Type{T}) where {T<:ValueNode} = valtype(T)

@inline isroot(n::ValueNode) = n.parent === ROOT

function walkback(f, n)
    hasparent = isa(n, ValueNode) && !(isroot(n))
    f(n, hasparent)
    if hasparent
        for ancestor in n.parent.input
            walkback(f, ancestor)
        end
    end
    return nothing
end

@inline valtype(x) = x
@inline valtype(x::RealNode{<:Any,V}) where {V} = V
@inline valtype(x::ArrayNode{<:Any,V}) where {V} = V
@inline valtype(x::Type{RealNode{G,V,C}}) where {G,V,C} = V
@inline valtype(x::Type{ArrayNode{G,V,C,T,N}}) where {G,V,C,T,N} = V

@inline genre(x) = ValueGenre()
@inline genre(g::AbstractGenre) = g
@inline genre(n::RealNode) = n.genre
@inline genre(n::ArrayNode) = n.genre

###################
# Pretty Printing #
###################

idstring(x) = base(62, object_id(x))

function Base.show(io::IO, n::FunctionNode)
    return print(io, "FunctionNode<$(n.func)>$(n.input)")
end

function Base.show(io::IO, n::RealNode)
    return print(io, "RealNode<$(idstring(n))>($(n.value))")
end

#########################
# Expression Generation #
#########################

interpolated_variable(x::ValueNode) = Symbol("x_" * idstring(untrack(x)))
interpolated_variable(x) = x

function toexpr(output::ValueNode)
    body = Expr(:block)
    args = Symbol[]
    walkback(output) do x, hasparent
        y = interpolated_variable(x)
        if hasparent
            p = x.parent
            push!(body.args, :($y = $(p.func)($(interpolated_variable.(p.input)...))))
        elseif isa(x, ValueNode)
            in(y, args) || push!(args, y)
        end
    end
    reverse!(body.args)
    return reverse(args), body
end
