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

abstract BoolTrait

struct True <: BoolTrait end

struct False <: BoolTrait end

############
# includes #
############

include("nodes.jl")
include("genres.jl")
include("directives.jl")
include("operations/scalars.jl")
include("operations/arrays.jl")
include("operations/elementwise.jl")

end # module
