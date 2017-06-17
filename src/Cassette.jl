__precompile__(false)

module Cassette

using RealInterface
using SpecialFunctions
using StaticArrays
using FunctionWrappers

const REAL_TYPES = [:Bool, :Integer, :Rational, :BigFloat, :BigInt, :AbstractFloat, :Real]

const ARRAY_TYPES = [:AbstractArray, :AbstractVector, :AbstractMatrix, :Array,
                     :Vector, :Matrix, :(StaticArrays.StaticArray),
                     :(StaticArrays.FieldVector), :(StaticArrays.MArray),
                     :(StaticArrays.SArray), :(StaticArrays.SUnitRange),
                     :(StaticArrays.SizedArray)]

include("genres.jl")
include("notes.jl")
include("directives.jl")
include("intercept.jl")
include("tapes.jl")
include("utilities.jl")
include("primitives/reals.jl")
include("primitives/arrays.jl")

end # module
