__precompile__(false)

module Cassette

using Base.RefValue

const Box = Base.RefValue

const MAX_ARGS = 50

include("contexts.jl")
include("wrappers.jl")
include("execute.jl")
include("macros.jl")

end # module
