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

struct Trackable end
struct NotTrackable end

@inline TrackableTrait(::Union{AbstractArray,Real}) = Trackable()
@inline TrackableTrait(::Any) = NotTrackable()

@inline function track(value::Union{AbstractArray,Real},
                       genre::AbstractGenre = ValueGenre(),
                       parent::FunctionNode = ROOT)
    if isa(value, Real)
        return RealNode(genre, value, parent)
    elseif isa(value, AbstractArray)
        return ArrayNode(genre, value, parent)
    end
end

@inline untrack(x::X) where {X} = x
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

@inline valtype(x::X) where {X} = x
@inline valtype(x::RealNode{<:Any,V}) where {V} = V
@inline valtype(x::ArrayNode{<:Any,V}) where {V} = V
@inline valtype(x::Type{RealNode{G,V,C}}) where {G,V,C} = V
@inline valtype(x::Type{ArrayNode{G,V,C,T,N}}) where {G,V,C,T,N} = V

@inline genre(x::X) where {X} = ValueGenre()
@inline genre(g::AbstractGenre) = g
@inline genre(n::RealNode) = n.genre
@inline genre(n::ArrayNode) = n.genre

###################
# Pretty Printing #
###################

function Base.show(io::IO, n::RealNode)
    return print(io, "RealNode<$(idstr(n));$(n.genre)>($(n.value))")
end

########
# Expr #
########

function Base.Expr(output::ValueNode)
    lines = Expr[]
    declared = Symbol[]
    walkback(output) do x, hasparent
        y = idsym(x)
        if hasparent
            p = x.parent
            push!(lines, :($y = $(p.func)($(idsym.(p.input)...))))
        else
            in(y, declared) || push!(declared, y)
        end
    end
    push!(lines, Expr(:local, reverse(declared)...))
    return Expr(:block, reverse(lines)...)
end
