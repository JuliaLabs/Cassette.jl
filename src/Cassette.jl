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

abstract type AbstractGenre end

############
# includes #
############

include("nodes.jl")
include("genres.jl")
include("dubbing.jl")
include("taping.jl")
include("operations/scalars.jl")
include("operations/arrays.jl")

end # module
