__precompile__(false)

module Cassette

const MAX_ARGS = 50

include("utilities.jl")
include("anonymous.jl")
include("contexts.jl")
include("wrappers.jl")
include("execute.jl")
include("macros.jl")

end # module
