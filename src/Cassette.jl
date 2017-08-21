__precompile__(false)

module Cassette

const MAX_ARGS = 50

include("tracing/metaprogramming.jl")
include("tracing/contexts.jl")
include("tracing/intercept.jl")

end # module
