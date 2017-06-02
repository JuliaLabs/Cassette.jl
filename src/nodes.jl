##############
# Node Types #
##############

# FunctionNode #
#--------------#

struct FunctionNode{G<:AbstractGenre,F,O,I<:Tuple,C}
    genre::G
    func::F
    output::O
    input::I
    cache::C
end

const ROOT = FunctionNode(ValueGenre(), nothing, nothing, tuple(), nothing)

# RealNode #
#----------#

mutable struct RealNode{G<:AbstractGenre,V<:Real,C} <: Real
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

mutable struct ArrayNode{G<:AbstractGenre,V<:AbstractArray,C,T<:RealNode,N} <: AbstractArray{T,N}
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

# trackability trait #
#--------------------#

abstract type TrackabilityTrait end

struct Trackable <: TrackabilityTrait end
struct NotTrackable <: TrackabilityTrait end
struct TrackableElementwise <: TrackabilityTrait end

@inline trackability(::Any) = NotTrackable()
@inline trackability(::Real) = Trackable()
@inline trackability(::AbstractArray{<:Real}) = Trackable()
@inline trackability(::AbstractArray) = TrackableElementwise()

# istracked trait #
#-----------------#

abstract type IstrackedTrait end

struct Tracked <: IstrackedTrait end
struct NotTracked <: IstrackedTrait end
struct TrackedElementwise <: IstrackedTrait end
struct MaybeTrackedElementwise <: IstrackedTrait end

@inline istracked(::Any) = NotTracked()
@inline istracked(::AbstractArray{T}) where {T} = isleaftype(T) ? NotTracked() : MaybeTrackedElementwise()
@inline istracked(::AbstractArray{<:RealNode}) = TrackedElementwise()
@inline istracked(::ArrayNode) = Tracked()
@inline istracked(::RealNode) = Tracked()

# track/untrack #
#---------------#

@inline track(value, genre::AbstractGenre = ValueGenre()) = _track(trackability(value), value, genre)

@inline _track(::Trackable,            value::Real,          genre) = RealNode(genre, value, ROOT)
@inline _track(::Trackable,            value::AbstractArray, genre) = ArrayNode(genre, value, ROOT)
@inline _track(::TrackableElementwise, value::AbstractArray, genre) = RealNode.(genre, value, ROOT)

@inline untrack(n::ArrayNode) = n.value
@inline untrack(n::RealNode) = n.value
@inline untrack(::Type{T}) where {T<:ValueNode} = valtype(T)
@inline untrack(x) = _untrack(istracked(x), x)

@inline _untrack(::Union{MaybeTrackedElementwise,TrackedElementwise}, x) = untrack.(x)
@inline _untrack(::NotTracked, x) = x

# walkback #
#----------#

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

# valtype #
#---------#

@inline valtype(x) = x
@inline valtype(x::RealNode{<:Any,V}) where {V} = V
@inline valtype(x::ArrayNode{<:Any,V}) where {V} = V
@inline valtype(x::Type{RealNode{G,V,C}}) where {G,V,C} = V
@inline valtype(x::Type{ArrayNode{G,V,C,T,N}}) where {G,V,C,T,N} = V

# genre #
#-------#

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
