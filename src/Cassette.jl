__precompile__(false)

module Cassette

using Core: CodeInfo, SlotNumber, NewvarNode, LabelNode, GotoNode, SSAValue, arrayref, arrayset

using Logging

struct Unused end

abstract type Context end
abstract type Tag{C,T} end

@inline tagtype(x) = tagtype(Nothing, x)

@generated function tagtype(::Type{C}, ::T) where {C,T}
    return quote
        $(Expr(:meta, :inline))
        Tag{C,$(objectid(T))}
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

function __init__()
    # FIXME: Base should provide a mechanism for this (eg. Julia/julia#26265)
    DEBUG = parse(Bool, get(ENV, "DEBUG", "false"))
    if DEBUG
        global_logger(ConsoleLogger(global_logger().stream, Logging.Debug))
    end
end

end # module
