__precompile__(false)

module Cassette

const MAX_ARGS = 20

include("tracing/metaprogramming.jl")
include("tracing/contexts.jl")
include("tracing/tracing.jl")

end # module
