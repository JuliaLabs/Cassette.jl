struct TrackedArray{E<:AbstractExecution,V<:Real,N,A<:AbstractArray{V,N}} <: AbstractArray{V,N}
    exec::E
    value::A
    tape::Tape
end

function TrackedArray(exec::E,
                      value::AbstractArray{V,N},
                      tape::Tape = DISABLED) where {E,V,N}
    return TrackedArray{E,V,N,typeof(value)}(exec, value, tape)
end

TrackedArray(value::AbstractArray, tape::Tape = DISABLED) = TrackedArray(BasicExecution(), value, tape)
