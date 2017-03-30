struct TrackedReal{E<:AbstractExecution,V<:Real} <: Real
    exec::E
    value::V
    tape::Tape
end

TrackedReal(exec::AbstractExecution, value::Real) = TrackedReal(exec, value, DISABLED)
TrackedReal(value::Real, tape::Tape = DISABLED) = TrackedReal(BasicExecution(), value, tape)
