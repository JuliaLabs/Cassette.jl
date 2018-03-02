__precompile__(false)

module Cassette

using Core: CodeInfo, SlotNumber, NewvarNode, LabelNode, GotoNode, SSAValue

const MAX_ARGS = 20

struct Unused end

include("utilities.jl")

include("contextual/contexts.jl")
include("contextual/anonymous.jl")
include("contextual/metadata.jl")

include("overdub/reflection.jl")
include("overdub/execution.jl")

include("api/api.jl")

include("workarounds.jl")

end # module
