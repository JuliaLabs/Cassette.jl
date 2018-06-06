__precompile__(false)

module Cassette

using Core: CodeInfo, SlotNumber, NewvarNode, LabelNode, GotoNode, SSAValue

using Logging

abstract type AbstractPass end
struct UnusedPass <: AbstractPass end
(::Type{UnusedPass})(::Any, ::Any, code_info) = code_info

abstract type AbstractTag end
struct BottomTag <: AbstractTag end

abstract type AbstractContext{P<:AbstractPass,T<:AbstractTag} end

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
