__precompile__(false)

module Cassette

using FunctionWrappers

const MAX_ARGS = 15

include("tracing/codeinfo.jl")
include("tracing/contexts.jl")
include("tracing/tracing.jl")

end # module
