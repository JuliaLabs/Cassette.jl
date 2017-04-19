module Cassette

#############
# utilities #
#############

const ARRAY_TYPES = [:AbstractArray, :AbstractVector, :AbstractMatrix, :Array,
                     :Vector, :Matrix]

const REAL_TYPES = [:Bool, :Integer, :Rational, :BigFloat, :BigInt,
                    :AbstractFloat, :Real]

const UNARY_REAL_FUNCS = [:-, :abs, :conj, :sqrt, :cbrt, :abs2, :inv, :log,
                          :log10, :log2, :log1p, :exp, :exp2, :expm1, :sin,
                          :cos, :tan, :sec, :csc, :cot, :sind, :cosd, :tand,
                          :secd, :cscd, :cotd, :asin, :acos, :atan, :asec,
                          :acsc, :acot, :asind, :acosd, :atand, :asecd,
                          :acscd, :acotd, :sinh, :cosh, :tanh, :sech, :csch,
                          :coth, :asinh, :acosh, :atanh, :asech, :acsch,
                          :acoth, :deg2rad, :rad2deg, :erf, :erfinv, :erfc,
                          :erfcinv, :erfi, :gamma, :lgamma, :digamma,
                          :invdigamma, :trigamma, :airyai, :airybi,
                          :airyaiprime, :airybiprime, :besselj0, :besselj1,
                          :bessely0, :bessely1, :erfcx, :dawson]

const BINARY_REAL_FUNCS = [:*, :/, :+, :-, :^, :atan2]

const SKIPPED_UNARY_REAL_FUNCS = [:isinf, :isnan, :isfinite, :iseven, :isodd,
                                  :isreal, :isinteger]

const SKIPPED_BINARY_REAL_FUNCS = [:isequal, :isless, :<, :>, :(==), :(!=),
                                   :(<=), :(>=)]

idstr(x) = string(base(62, object_id(x)))[1:3]

############
# includes #
############

include("instructions.jl")
include("genres.jl")
include("tapes.jl")
include("tracked.jl")
include("directives.jl")
include("operations/scalars.jl")

end # module
