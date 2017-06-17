__precompile__(false)

module Cassette

using FunctionWrappers

include("genres.jl")
include("notes.jl")
include("directives.jl")
include("intercept.jl")
include("tapes.jl")
include("utilities.jl")

end # module
