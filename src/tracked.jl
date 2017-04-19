#################
# Tracked Types #
#################

mutable struct TrackedReal{G,V<:Real,C} <: Real
    tape::Tape{G}
    value::V
    cache::C
end

mutable struct TrackedArray{G,V,N,AV<:AbstractArray{V,N},C,AC} <: AbstractArray{TrackedReal{G,V,C},N}
    tape::Tape{G}
    value::AV
    cache::AC
end

const Tracked = Union{TrackedReal,TrackedArray}

#######################
# `Tracked` Interface #
#######################

track(tape::Tape, value::Real, cache = nothing) = TrackedReal(tape, value, cache)

track(tape::Tape, value::AbstractArray, cache = nothing) = TrackedArray(tape, value, cache)

value(x::Tracked) = x.value

tape(x::Tracked) = x.tape

###################
# Pretty Printing #
###################

function Base.show(io::IO, t::TrackedReal{G}) where G
    return print(io, "TrackedReal{$G}<$(idstr(t))>($(value(t)), $(t.cache))")
end
