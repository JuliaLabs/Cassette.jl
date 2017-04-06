###############
# TrackedReal #
###############

struct TrackedReal{G,V<:Real} <: Real
    tape::Tape{G}
    value::V
end

###############
# MutableReal #
###############

mutable struct MutableReal{V<:Real} <: Real
    value::V
end
