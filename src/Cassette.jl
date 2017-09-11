__precompile__(false)

module Cassette

using Base.RefValue

const MAX_ARGS = 50

include("context.jl")
include("meta.jl")
include("execute.jl")
include("macros.jl")

end # module
