struct TrackedArray{G,V,N,A<:AbstractArray{V,N}} <: AbstractArray{TrackedReal{G,V},N}
    tape::Tape{G}
    value::A
end

function TrackedArray(tape::Tape{G}, value::AbstractArray{V,N}) where {G,V,N}
    return TrackedArray{G,V,N,typeof(value)}(tape, value)
end
