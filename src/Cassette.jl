__precompile__(false)

module Cassette

using FunctionWrappers

#=
Cassette contains a lot of functor-esque types which wrap callables/values in order
to hijack/record normal Julia function execution via multiple dispatch. All of these
wrapper types implement `unwrap`, whose default no-op behavior is defined here.
=#
@inline unwrap(x) = x

include("genres.jl")
include("directives.jl")
include("notes.jl")
include("tapes.jl")

# include `tracing.jl` after basically everything else, since the
# `@generated` functions within are sensitive to code load order.
include("tracing.jl")

include("genres/VoidGenre.jl")
include("genres/ValueGenre.jl")



end # module
