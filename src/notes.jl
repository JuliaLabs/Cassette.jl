##############
# Note Types #
##############

# FunctionNote #
#--------------#

mutable struct FunctionNote{G<:AbstractGenre,F,P<:Tuple,C}
    value::F
    parent::P
    cache::C
    FunctionNote{G}(value::F, parent::P, cache::C = nothing) where {G<:AbstractGenre,F,P<:Tuple,C} = new{G,F,P,C}(value, parent, cache)
end

# RealNote #
#----------#

mutable struct RealNote{G<:AbstractGenre,V<:Real,F,C} <: Real
    value::V
    parent::FunctionNote{G,F}
    cache::C
    function RealNote(value::V, parent::FunctionNote{G,F}) where {G,V,F}
        cache = note_cache(G(), value)
        C = eltype(cache)
        return new{G,V,F,C}(value, parent, cache)
    end
end

# ArrayNote #
#-----------#

struct ArrayNote{G<:AbstractGenre,V<:AbstractArray,F,C,T<:RealNote,N} <: AbstractArray{T,N}
    value::V
    parent::FunctionNote{G,F}
    cache::C
    function ArrayNote(value::V, parent::FunctionNote{G,F}) where {G,N,V<:AbstractArray{<:Real,N},F}
        cache = note_cache(G(), value)
        C = eltype(cache)
        TC = note_cache_eltype(G(), cache)
        T = RealNote{G,eltype(value),TC,typeof(Base.getindex)}
        return new{G,V,F,C,T,N}(value, parent, cache)
    end
end

# ValueNote #
#-----------#

const ValueNote = Union{RealNote,ArrayNote}

######################
# trackability trait #
######################

abstract type TrackabilityTrait end

struct Trackable <: TrackabilityTrait end
struct NotTrackable <: TrackabilityTrait end
struct TrackableElementwise <: TrackabilityTrait end

@inline trackability(x) = trackability(typeof(x))
@inline trackability(::Type{<:Any}) = NotTrackable()
@inline trackability(::Type{<:Real}) = Trackable()
@inline trackability(::Type{<:AbstractArray{<:Real}}) = Trackable()
@inline trackability(::Type{<:AbstractArray}) = TrackableElementwise()

###################
# istracked trait #
###################

abstract type IstrackedTrait end

struct Tracked <: IstrackedTrait end
struct NotTracked <: IstrackedTrait end
struct TrackedElementwise <: IstrackedTrait end
struct MaybeTrackedElementwise <: IstrackedTrait end

@inline istracked(x) = istracked(typeof(x))
@inline istracked(::Type{<:Any}) = NotTracked()
@inline istracked(::Type{<:AbstractArray{T}}) where {T} = isleaftype(T) ? NotTracked() : MaybeTrackedElementwise()
@inline istracked(::Type{<:AbstractArray{<:RealNote}}) = TrackedElementwise()
@inline istracked(::Type{<:ArrayNote}) = Tracked()
@inline istracked(::Type{<:RealNote}) = Tracked()

###############
# root/isroot #
###############

root(::G) where {G<:AbstractGenre} = FunctionNote{G}(nothing, tuple())

@inline isroot(x) = isroot(typeof(x))
@inline isroot(::Type{<:FunctionNote}) = false
@inline isroot(::Type{<:RealNote}) = false
@inline isroot(::Type{<:ArrayNote}) = false
@inline isroot(::Type{<:Any}) = true
@inline isroot(::Type{<:FunctionNote{<:AbstractGenre,Void}}) = true
@inline isroot(::Type{<:RealNote{<:AbstractGenre,<:Real,<:Any,Void}}) = true
@inline isroot(::Type{<:ArrayNote{<:AbstractGenre,<:AbstractArray,<:Any,<:RealNote,<:Any,Void}}) = true

#########
# track #
#########

@inline track(value, genre::AbstractGenre = ValueGenre()) = track(value, root(genre))
@inline track(value, parent::FunctionNote) = _track(trackability(value), value, parent)

@inline _track(::Trackable,            value::Real,          parent) = RealNote(value, parent)
@inline _track(::Trackable,            value::AbstractArray, parent) = ArrayNote(value, parent)
@inline _track(::TrackableElementwise, value::AbstractArray, parent) = RealNote.(value, parent)

###########
# rewind! #
###########

@inline rewind!(f!, n::ValueNote) = _rewind!(f!, n)

_rewind!(f!, n) = (f!(n); isroot(n) || _rewind_ancestors!(f!, parent(n)); nothing)

@generated function _rewind_ancestors!(f!, n::FunctionNote{G,F,P}) where {G<:AbstractGenre,F,P<:Tuple}
    rewind_calls = Expr(:block, Any[])
    for i in 1:nfields(P)
        push!(rewind_calls.args, :(_rewind!(f!, ancestors[$i]::$(P.parameters[i]))))
    end
    return quote
        $(Expr(:meta, :noinline))
        ancestors = parent(n)
        $(rewind_calls)
        return nothing
    end
end

###################
# getters/setters #
###################

# valuetype #
#-----------#

@inline valuetype(x) = valuetype(typeof(x))
@inline valuetype(::Type{V}) where {V} = V
@inline valuetype(::Type{RealNote{G,V,F,C}}) where {G,V,F,C} = V
@inline valuetype(::Type{ArrayNote{G,V,F,C,T,N}}) where {G,V,F,C,T,N} = V
@inline valuetype(::Type{FunctionNote{G,F,P,C}}) where {G,F,P,C} = F

# cachetype #
#-----------#

@inline cachetype(x) = cachetype(typeof(x))
@inline cachetype(::Type{V}) where {V} = Void
@inline cachetype(::Type{RealNote{G,V,F,C}}) where {G,V,F,C} = C
@inline cachetype(::Type{ArrayNote{G,V,F,C,T,N}}) where {G,V,F,C,T,N} = C
@inline cachetype(::Type{FunctionNote{G,F,P,C}}) where {G,F,P,C} = C

# value #
#-------#

@inline _value(::Union{MaybeTrackedElementwise,TrackedElementwise}, x) = value.(x)
@inline _value(::NotTracked, x) = x

@inline value(x) = _value(istracked(x), x)
@inline value(::Type{T}) where {T} = valuetype(T)
@inline value(n::RealNote) = n.value
@inline value(n::ArrayNote) = n.value
@inline value(n::FunctionNote) = n.value

# value! #
#--------#

@inline value!(n::RealNote, v::Real) = (n.value = v)
@inline value!(n::ArrayNote, v::AbstractArray) = (n.value = v)

# cache #
#-------#

@inline cache(n::RealNote) = n.cache
@inline cache(n::ArrayNote) = n.cache
@inline cache(n::FunctionNote) = n.cache

# cache! #
#--------#

@inline cache!(n::RealNote, c) = (n.cache = c)
@inline cache!(n::ArrayNote, c) = (n.cache = c)
@inline cache!(n::FunctionNote, c) = (n.cache = c)

# genre #
#-------#

@inline genre(x) = genre(typeof(x))
@inline genre(::Type{T}) where {T} = ValueGenre()
@inline genre(::Type{RealNote{G,V,F,C}}) where {G,V,F,C} = G()
@inline genre(::Type{ArrayNote{G,V,F,C,T,N}}) where {G,V,F,C,T,N} = G()
@inline genre(::Type{FunctionNote{G,F,P,C}}) where {G,F,P,C} = G()

# parent #
#--------#

@inline parent(n::RealNote) = n.parent
@inline parent(n::ArrayNote) = n.parent
@inline parent(n::FunctionNote) = n.parent

###################
# Pretty Printing #
###################

idstring(x) = base(62, object_id(x))

function Base.show(io::IO, n::FunctionNote{G,F}) where {G,F}
    return print(io, "FunctionNote{$G,$F}($(input(n)), $(cache(n))")
end

function Base.show(io::IO, n::RealNote{G}) where {G}
    return print(io, "RealNote{$G}<$(idstring(n))>($(value(n)), $(cache(n)))")
end
