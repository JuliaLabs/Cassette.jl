# unary #
#-------#

for f in UNARY_REAL_FUNCS
    @eval @inline Base.$(f)(x::TrackedReal) = Record($f)(x)
end

for f in SKIPPED_UNARY_REAL_FUNCS
    @eval @inline Base.$(f)(x::TrackedReal) = Skip($(f))(x)
end

# binary #
#--------#

for f in BINARY_REAL_FUNCS
    @eval @inline Base.$(f)(a::TrackedReal, b::TrackedReal) = Record($f)(a, b)
    for R in REAL_TYPES
        @eval begin
            @inline Base.$(f)(a::TrackedReal, b::$R) = Record($f)(a, b)
            @inline Base.$(f)(a::$R, b::TrackedReal) = Record($f)(a, b)
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
