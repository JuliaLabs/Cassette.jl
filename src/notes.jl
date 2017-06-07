##############
# Note Types #
##############

# FunctionNote #
#--------------#

struct FunctionNote{G<:AbstractGenre,F,I<:Tuple}
    genre::G
    func::F
    input::I
end

const ROOT = FunctionNote(ValueGenre(), nothing, tuple())

# RealNote #
#----------#

mutable struct RealNote{G<:AbstractGenre,V<:Real,C} <: Real
    genre::G
    value::V
    cache::C
    parent::FunctionNote
    function RealNote(genre::G, value::V, parent::FunctionNote) where {G,V}
        cache = note_cache(genre, value)
        C = typeof(cache)
        return new{G,V,C}(genre, value, cache, parent)
    end
end

# ArrayNote #
#-----------#

mutable struct ArrayNote{G<:AbstractGenre,V<:AbstractArray,C,T<:RealNote,N} <: AbstractArray{T,N}
    genre::G
    value::V
    cache::C
    parent::FunctionNote
    function ArrayNote(genre::G, value::V, parent::FunctionNote) where {G,N,V<:AbstractArray{<:Real,N}}
        cache = note_cache(genre, value)
        C = typeof(cache)
        T = note_eltype(genre, value)
        return new{G,V,C,T,N}(genre, value, cache, parent)
    end
end

#######################
# ValueNote Interface #
#######################

const ValueNote = Union{RealNote,ArrayNote}

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
@inline istracked(::AbstractArray{<:RealNote}) = TrackedElementwise()
@inline istracked(::ArrayNote) = Tracked()
@inline istracked(::RealNote) = Tracked()

# track/untrack #
#---------------#

@inline track(value, genre::AbstractGenre = ValueGenre()) = _track(trackability(value), value, genre)

@inline _track(::Trackable,            value::Real,          genre) = RealNote(genre, value, ROOT)
@inline _track(::Trackable,            value::AbstractArray, genre) = ArrayNote(genre, value, ROOT)
@inline _track(::TrackableElementwise, value::AbstractArray, genre) = RealNote.(genre, value, ROOT)

@inline untrack(n::ArrayNote) = n.value
@inline untrack(n::RealNote) = n.value
@inline untrack(::Type{T}) where {T<:ValueNote} = valtype(T)
@inline untrack(x) = _untrack(istracked(x), x)

@inline _untrack(::Union{MaybeTrackedElementwise,TrackedElementwise}, x) = untrack.(x)
@inline _untrack(::NotTracked, x) = x

# rewind! #
#---------#

@inline isroot(n::ValueNote) = n.parent === ROOT

rewind!(f, n::ValueNote) = _rewind!(f, n)

function _rewind!(f, n)
    hasparent = isa(n, ValueNote) && !(isroot(n))
    f(n, hasparent)
    if hasparent
        for ancestor in n.parent.input
            _rewind!(f, ancestor)
        end
    end
    return nothing
end

# valtype #
#---------#

@inline valtype(x) = x
@inline valtype(x::RealNote{<:Any,V}) where {V} = V
@inline valtype(x::ArrayNote{<:Any,V}) where {V} = V
@inline valtype(x::Type{RealNote{G,V,C}}) where {G,V,C} = V
@inline valtype(x::Type{ArrayNote{G,V,C,T,N}}) where {G,V,C,T,N} = V

# genre #
#-------#

@inline genre(n::RealNote) = n.genre
@inline genre(n::ArrayNote) = n.genre

###################
# Pretty Printing #
###################

idstring(x) = base(62, object_id(x))

function Base.show(io::IO, n::FunctionNote)
    return print(io, "FunctionNote<$(n.func)>$(n.input)")
end

function Base.show(io::IO, n::RealNote)
    return print(io, "RealNote<$(idstring(n))>($(n.value))")
end
