__precompile__(false)

module Cassette

using Sugar
using FunctionWrappers

include("genres.jl")
include("directives.jl")
include("intercept.jl")

end # module
