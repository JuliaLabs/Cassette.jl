__precompile__(false)

module Cassette

using Core: CodeInfo, SlotNumber, NewvarNode, LabelNode, GotoNode, SSAValue

using Logging

abstract type AbstractTag end
struct BottomTag <: AbstractTag end

abstract type AbstractPass end
struct NoPass <: AbstractPass end
(::Type{NoPass})(::Any, ::Any, code_info) = code_info

abstract type AbstractContext{T<:AbstractTag,P<:AbstractPass,B} end

include("utilities.jl")
include("tagged.jl")
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
