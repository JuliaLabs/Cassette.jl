###############
# TrackedReal #
###############

struct TrackedReal{G,V<:Real} <: Real
    tape::Tape{G}
    value::RefValue{V}
end
