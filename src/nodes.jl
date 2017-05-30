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

mutable struct RealNode{G<:AbstractGenre,V<:Real,C} <: Real
    genre::G
    value::V
    cache::C
    parent::FunctionNode
    function RealNode(genre::G, value::V, parent::FunctionNode) where {G,V,C}
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

struct Trackable end
struct NotTrackable end
struct TrackableElementwise end

@inline trackability(::Any) = NotTrackable()
@inline trackability(::Real) = Trackable()
@inline trackability(::AbstractArray{<:Real}) = Trackable()
@inline trackability(::AbstractArray) = TrackableElementwise()

# istracked trait #
#-----------------#

struct Tracked end
struct NotTracked end
struct TrackedElementwise end
struct MaybeTrackedElementwise end

@inline istracked(::Any) = NotTracked()
@inline istracked(::AbstractArray{T}) where {T} = isleaftype(T) ? NotTracked() : MaybeTrackedElementwise()
@inline istracked(::AbstractArray{<:RealNode}) = TrackedElementwise()
@inline istracked(::ArrayNode) = Tracked()
@inline istracked(::RealNode) = Tracked()

# track/untrack #
#---------------#

@inline function track(value,
                       genre::AbstractGenre = ValueGenre(),
                       parent::FunctionNode = ROOT)
    return _track(trackability(value), value, genre, parent)
end

@inline _track(::Trackable,            value::Real,          genre, parent) = RealNode(genre, value, parent)
@inline _track(::Trackable,            value::AbstractArray, genre, parent) = ArrayNode(genre, value, parent)
@inline _track(::TrackableElementwise, value::AbstractArray, genre, parent) = map(v -> RealNode(genre, v, parent), value)

@inline untrack(n::ArrayNode) = n.value
@inline untrack(n::RealNode) = n.value
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
