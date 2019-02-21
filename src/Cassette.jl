module Cassette

using Core: CodeInfo, SlotNumber, SSAValue, Typeof

include("context.jl")
include("pass.jl")
include("tagging.jl")
include("overdub.jl")
include("deprecations.jl")

const NO_PASS = @pass (_, r) -> r.code_info

end # module
