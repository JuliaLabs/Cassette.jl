module Cassette

using RealInterface
using SpecialFunctions
using StaticArrays
using FunctionWrappers

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

############
# includes #
############

include("genres.jl")
include("nodes.jl")
include("tapes.jl")
include("operations/scalars.jl")
include("operations/arrays.jl")

end # module
