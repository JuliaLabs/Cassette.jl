__precompile__(false)

module Cassette

const MAX_ARGS = 15

@generated function call(g, f, args...)
    return quote
        $(Expr(:meta, :inline))
        $(Expr(:call, :f, [:(g(args[$i])) for i in 1:nfields(args)]...))
    end
end

include("tracing/codeinfo.jl")
include("tracing/contexts.jl")
include("tracing/tracing.jl")

end # module
