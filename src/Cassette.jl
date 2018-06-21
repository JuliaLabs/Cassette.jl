__precompile__(false)

module Cassette

using Core: CodeInfo, SlotNumber, NewvarNode, GotoNode, SSAValue

using Logging

include("utilities.jl")
include("context.jl")
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
