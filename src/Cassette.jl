module Cassette

using Core: CodeInfo, SlotNumber, NewvarNode, GotoNode, SSAValue, Typeof

include("context.jl")
include("pass.jl")
include("tagging.jl")
include("overdub.jl")

end # module
