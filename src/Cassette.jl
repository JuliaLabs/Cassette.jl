module Cassette

using RealInterface
using SpecialFunctions
using StaticArrays

#####################
# basic definitions #
#####################

const ARRAY_TYPES = [:AbstractArray, :AbstractVector, :AbstractMatrix, :Array,
                     :Vector, :Matrix, :(StaticArrays.StaticArray),
                     :(StaticArrays.FieldVector), :(StaticArrays.MArray),
                     :(StaticArrays.SArray), :(StaticArrays.SUnitRange),
                     :(StaticArrays.SizedArray)]

const REAL_TYPES = [:Bool, :Integer, :Rational, :BigFloat, :BigInt,
                    :AbstractFloat, :Real]

abstract type AbstractGenre end

struct True end

struct False end

############
# includes #
############

include("nodes.jl")
include("genres.jl")
include("directives.jl")
include("operations/scalars.jl")
include("operations/arrays.jl")

end # module
