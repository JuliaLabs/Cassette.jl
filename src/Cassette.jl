__precompile__(false)

module Cassette

using Core: CodeInfo, SlotNumber, NewvarNode, LabelNode, GotoNode, SSAValue

using Logging

struct Unused end

abstract type AbstractPass end
abstract type AbstractTag end
abstract type AbstractContext{P<:Union{AbstractPass,Unused},T<:Union{AbstractTag,Nothing}} end

const UNUSED = Unused()
const MAX_ARGS = 20

include("utilities.jl")
include("overdub.jl")
include("macros.jl")

function __init__()
    # FIXME: Base should provide a mechanism for this (eg. Julia/julia#26265)
    DEBUG = parse(Bool, get(ENV, "DEBUG", "false"))
    if DEBUG
        global_logger(ConsoleLogger(global_logger().stream, Logging.Debug))
    end
end

end # module
