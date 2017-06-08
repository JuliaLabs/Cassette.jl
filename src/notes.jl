#########################
# Mutable Utility Types #
#########################
# These are basically RefValues, but without
# all the historical baggage carried by Ref,
# and carrying additional semantic meaning.

mutable struct Cache{C}
    data::C
end

@inline Base.getindex(c::Cache) = c.data
@inline Base.setindex!(c::Cache, x) = (c.data = x; c)

mutable struct Value{V}
    data::V
end

@inline Base.getindex(v::Value) = v.data
@inline Base.setindex!(v::Value, x) = (v.data = x; v)

##############
# Note Types #
##############

# FunctionNote #
#--------------#

struct FunctionNote{G<:AbstractGenre,V,C,P<:Tuple}
    value::V
    cache::Cache{C}
    parent::P
    FunctionNote{G}(value::V, cache::Cache{C}, parent::P) where {G<:AbstractGenre,V,C,P<:Tuple} = new{G,V,C,P}(value, cache, parent)
    FunctionNote{G}(value, cache, parent) where {G<:AbstractGenre} = FunctionNote{G}(value, Cache(cache), parent)
end

# RealNote #
#----------#

struct RealNote{G<:AbstractGenre,V<:Real,C,P} <: Real
    value::Value{V}
    cache::Cache{C}
    parent::FunctionNote{G,P}
    function RealNote(value::V, parent::FunctionNote{G,P}) where {G,V,P}
        cache = note_cache(G(), value)
        C = typeof(cache)
        return new{G,V,C,P}(Value(value), Cache(cache), parent)
    end
end

# ArrayNote #
#-----------#

struct ArrayNote{G<:AbstractGenre,V<:AbstractArray,C,T<:RealNote,N,P} <: AbstractArray{T,N}
    value::Value{V}
    cache::Cache{C}
    parent::FunctionNote{G,P}
    function ArrayNote(value::V, parent::FunctionNote{G,P}) where {G,N,V<:AbstractArray{<:Real,N},P}
        cache = note_cache(G(), value)
        C = typeof(cache)
        T = RealNote{G,eltype(value),note_cache_eltype(G(), value),typeof(Base.getindex)}
        return new{G,V,C,T,N,P}(Value(value), Cache(cache), parent)
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

@inline trackability(::Any) = NotTrackable()
@inline trackability(::Real) = Trackable()
@inline trackability(::AbstractArray{<:Real}) = Trackable()
@inline trackability(::AbstractArray) = TrackableElementwise()

###################
# istracked trait #
###################

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

###############
# root/isroot #
###############

root(g::AbstractGenre) = FunctionNote(g, nothing, tuple(), nothing)

@inline isroot(::FunctionNote) = false
@inline isroot(::RealNote) = false
@inline isroot(::ArrayNote) = false

@inline isroot(::Any) = true
@inline isroot(::FunctionNote{<:AbstractGenre,Void}) = true
@inline isroot(::RealNote{<:AbstractGenre,<:Real,<:Any,Void}) = true
@inline isroot(::ArrayNote{<:AbstractGenre,<:AbstractArray,<:Any,<:RealNote,<:Any,Void}) = true

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

@generated function _rewind_ancestors!(f!, n::FunctionNote{G,F,I}) where {G<:AbstractGenre,F,I<:Tuple}
    rewind_calls = Expr(:block, Any[])
    for i in 1:nfields(I)
        push!(rewind_calls.args, :(_rewind!(f!, ancestors[$i]::$(I.parameters[i]))))
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

@inline valuetype(x) = typeof(x)
@inline valuetype(::Type{V}) where {V} = V
@inline valuetype(::RealNote{<:Any,V}) where {V} = V
@inline valuetype(::ArrayNote{<:Any,V}) where {V} = V
@inline valuetype(::Type{RealNote{G,V,C,P}}) where {G,V,C,P} = V
@inline valuetype(::Type{ArrayNote{G,V,C,T,N,P}}) where {G,V,C,T,N,P} = V

# cachetype #
#-----------#

@inline cachetype(x) = Void
@inline cachetype(::DataType) = Void
@inline cachetype(::RealNote{<:Any,<:Any,C}) where {C} = C
@inline cachetype(::ArrayNote{<:Any,<:Any,C}) where {C} = C
@inline cachetype(::Type{RealNote{G,V,C,P}}) where {G,V,C,P} = C
@inline cachetype(::Type{ArrayNote{G,V,C,T,N,P}}) where {G,V,C,T,N,P} = C

# value #
#-------#

@inline _value(::Union{MaybeTrackedElementwise,TrackedElementwise}, x) = value.(x)
@inline _value(::NotTracked, x) = x

@inline value(x) = _value(istracked(x), x)
@inline value(n::RealNote) = n.value[]
@inline value(n::ArrayNote) = n.value[]
@inline value(n::FunctionNote) = n.value[]
@inline value(::Type{T}) where {T<:ValueNote} = valuetype(T)

# value! #
#--------#

@inline value!(x::AbstractArray, v) = (for i in eachindex(x); value!(x[i], v[i]); end; v)
@inline value!(n::RealNote, v) = (n.value[] = v)
@inline value!(n::ArrayNote, v) = (n.value[] = v)
@inline value!(n::FunctionNote, v) = (n.value[] = v)

# cache #
#-------#

@inline _cache(::Union{MaybeTrackedElementwise,TrackedElementwise}, x) = cache.(x)
@inline _cache(::NotTracked, x) = nothing

@inline cache(x) = _cache(istracked(x), x)
@inline cache(n::RealNote) = n.cache[]
@inline cache(n::ArrayNote) = n.cache[]
@inline cache(n::FunctionNote) = n.cache[]

# cache! #
#--------#

@inline cache!(x::AbstractArray, c) = (for i in eachindex(x); cache!(x[i], c[i]); end; c)
@inline cache!(n::RealNote, c) = (n.cache[] = c)
@inline cache!(n::ArrayNote, c) = (n.cache[] = c)
@inline cache!(n::FunctionNote, c) = (n.cache[] = c)

# genre #
#-------#

@inline genre(x) = ValueGenre()
@inline genre(::RealNote{G}) = G()
@inline genre(::ArrayNote{G}) = G()
@inline genre(::FunctionNote{G}) = G()
@inline genre(::Type{RealNote{G,V,C,P}}) where {G,V,C,P} = G()
@inline genre(::Type{ArrayNote{G,V,C,T,N,P}}) where {G,V,C,T,N,P} = G()
@inline genre(::Type{FunctionNote{G,V,C,T,N,P}}) where {G,V,C,T,N,P} = G()

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
