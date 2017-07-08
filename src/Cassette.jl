__precompile__(false)

module Cassette

using FunctionWrappers

include("tracing/codeinfo.jl")
include("tracing/contexts.jl")
include("tracing/tracing.jl")

end # module
