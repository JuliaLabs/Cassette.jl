module Cassette

using Core: CodeInfo, SlotNumber, NewvarNode, GotoNode, SSAValue

include("utilities.jl")
include("context.jl")
include("tagged.jl")
include("overdub.jl")
include("macros.jl")

end # module
