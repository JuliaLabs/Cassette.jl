###############
# TrackedReal #
###############

mutable struct TrackedReal{G,V<:Real,C} <: Real
    tape::Tape{G}
    value::V
    cache::C
end

########################
# Recordable Execution #
########################

# unary #
#-------#

for f in UNARY_REAL_FUNCS
    @eval @inline Base.$(f)(x::TrackedReal) = record!(x.tape, $f, x)
end

for f in SKIPPED_UNARY_REAL_FUNCS
    @eval @inline Base.$(f)(x::TrackedReal) = Skip($(f))(x)
end

# binary #
#--------#

for f in BINARY_REAL_FUNCS
    @eval @inline Base.$(f)(a::TrackedReal, b::TrackedReal) = record!(merge(a.tape, b.tape), $f, a, b)
    for R in REAL_TYPES
        @eval begin
            @inline Base.$(f)(a::TrackedReal, b::$R) = record!(merge(a.tape, b.tape), $f, a, b)
            @inline Base.$(f)(a::$R, b::TrackedReal) = record!(merge(a.tape, b.tape), $f, a, b)
        end
    end
end

for f in SKIPPED_BINARY_REAL_FUNCS
    @eval @inline Base.$(f)(a::TrackedReal, b::TrackedReal) = Skip($(f))(a, b)
    for R in REAL_TYPES
        @eval begin
            @inline Base.$(f)(a::$R, b::TrackedReal) = Skip($(f))(a, b)
            @inline Base.$(f)(a::TrackedReal, b::$R) = Skip($(f))(a, b)
        end
    end
end
