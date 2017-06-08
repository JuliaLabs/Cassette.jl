##############
# Note Types #
##############

# FunctionNote #
#--------------#

struct FunctionNote{G<:AbstractGenre,F,I<:Tuple,C}
    genre::G
    func::F
    input::I
    cache::C
end

# RealNote #
#----------#

mutable struct RealNote{G<:AbstractGenre,V<:Real,C,P} <: Real
    genre::G
    value::V
    cache::C
    parent::FunctionNote{G,P}
    function RealNote(genre::G, value::V, parent::FunctionNote{G,P}) where {G,V,P}
        cache = note_cache(genre, value)
        C = typeof(cache)
        return new{G,V,C,P}(genre, value, cache, parent)
    end
end

# ArrayNote #
#-----------#

mutable struct ArrayNote{G<:AbstractGenre,V<:AbstractArray,C,T<:RealNote,N,P} <: AbstractArray{T,N}
    genre::G
    value::V
    cache::C
    parent::FunctionNote{G,P}
    function ArrayNote(genre::G, value::V, parent::FunctionNote{G,P}) where {G,N,V<:AbstractArray{<:Real,N},P}
        cache = note_cache(genre, value)
        C = typeof(cache)
        T = RealNote{G,eltype(value),note_cache_eltype(genre, value),typeof(Base.getindex)}
        return new{G,V,C,T,N,P}(genre, value, cache, parent)
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

# root/isroot #
#-------------#

root(g::AbstractGenre) = FunctionNote(g, nothing, tuple(), nothing)

@inline isroot(::FunctionNote) = false
@inline isroot(::RealNote) = false
@inline isroot(::ArrayNote) = false

@inline isroot(::Any) = true
@inline isroot(::FunctionNote{<:AbstractGenre,Void}) = true
@inline isroot(::RealNote{<:AbstractGenre,<:Real,<:Any,Void}) = true
@inline isroot(::ArrayNote{<:AbstractGenre,<:AbstractArray,<:Any,<:RealNote,<:Any,Void}) = true

# track/untrack #
#---------------#

@inline track(value, genre::AbstractGenre = ValueGenre(), parent::FunctionNote = root(genre)) = _track(trackability(value), value, genre, parent)

@inline _track(::Trackable,            value::Real,          genre, parent) = RealNote(genre, value, parent)
@inline _track(::Trackable,            value::AbstractArray, genre, parent) = ArrayNote(genre, value, parent)
@inline _track(::TrackableElementwise, value::AbstractArray, genre, parent) = RealNote.(genre, value, parent)

@inline untrack(n::ArrayNote) = n.value
@inline untrack(n::RealNote) = n.value
@inline untrack(::Type{T}) where {T<:ValueNote} = valtype(T)
@inline untrack(x) = _untrack(istracked(x), x)

@inline _untrack(::Union{MaybeTrackedElementwise,TrackedElementwise}, x) = untrack.(x)
@inline _untrack(::NotTracked, x) = x

# rewind! #
#---------#

@inline rewind!(f!, n::ValueNote) = _rewind!(f!, n)

_rewind!(f!, n) = (f!(n); isroot(n) || _rewind_ancestors!(f!, n.parent); nothing)

@generated function _rewind_ancestors!(f!, n::FunctionNote{G,F,I}) where {G<:AbstractGenre,F,I<:Tuple}
    rewind_calls = Expr(:block, Any[])
    for i in 1:nfields(I)
        push!(rewind_calls.args, :(_rewind!(f!, ancestors[$i]::$(I.parameters[i]))))
    end
    return quote
        $(Expr(:meta, :noinline))
        ancestors = n.input
        $(rewind_calls)
        return nothing
    end
end

# valtype #
#---------#

@inline valtype(x) = x
@inline valtype(::RealNote{<:Any,V}) where {V} = V
@inline valtype(::ArrayNote{<:Any,V}) where {V} = V
@inline valtype(::Type{RealNote{G,V,C,P}}) where {G,V,C,P} = V
@inline valtype(::Type{ArrayNote{G,V,C,T,N,P}}) where {G,V,C,T,N,P} = V

# genre #
#-------#

@inline genre(n::RealNote) = n.genre
@inline genre(n::ArrayNote) = n.genre
@inline genre(::Type{RealNote{G,V,C,P}}) where {G,V,C,P} = G()
@inline genre(::Type{ArrayNote{G,V,C,T,N,P}}) where {G,V,C,T,N,P} = G()

###################
# Pretty Printing #
###################

idstring(x) = base(62, object_id(x))

function Base.show(io::IO, n::FunctionNote{G,F}) where {G,F}
    return print(io, "FunctionNote{$G,$F}($(n.input), $(n.cache))")
end

function Base.show(io::IO, n::RealNote{G}) where {G}
    return print(io, "RealNote{$G}<$(idstring(n))>($(n.value), $(n.cache))")
end
