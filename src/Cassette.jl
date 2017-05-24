module Cassette

using RealInterface
using SpecialFunctions

#####################
# basic definitions #
#####################

const ARRAY_TYPES = [:AbstractArray, :AbstractVector, :AbstractMatrix, :Array,
                     :Vector, :Matrix]

const REAL_TYPES = [:Bool, :Integer, :Rational, :BigFloat, :BigInt,
                    :AbstractFloat, :Real]

abstract type AbstractGenre end

############
# includes #
############

include("nodes.jl")
include("genres.jl")
include("primitives/primitives.jl")
include("primitives/scalars.jl")

end # module
