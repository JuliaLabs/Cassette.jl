__precompile__(false)

module Cassette

using FunctionWrappers

include("genres.jl")
include("directives.jl")
include("notes.jl")
include("tracing.jl")
include("tapes.jl")

end # module
