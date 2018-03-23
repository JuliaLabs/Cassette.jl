__precompile__(false)

module Cassette

using Core: CodeInfo, SlotNumber, NewvarNode, LabelNode, GotoNode, SSAValue, arrayref, arrayset

struct Unused end

abstract type Context end
abstract type Tag{T} end

@generated function Tag(::T) where {T}
    return quote
        $(Expr(:meta, :inline))
        Tag{$(objectid(T))}
    end
end

const unused = Unused()
const MAX_ARGS = 20

include("utilities.jl")
include("metadata.jl")
include("reflection.jl")
include("execution.jl")
include("macros.jl")
include("workarounds.jl")

end # module
