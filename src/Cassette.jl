module Cassette

using RealInterface
using SpecialFunctions
using StaticArrays
using FunctionWrappers

include("genres.jl")
include("notes.jl")
include("recording.jl")
include("taping.jl")
include("utilities.jl")
include("operations/reals.jl")
include("operations/arrays.jl")

end # module
