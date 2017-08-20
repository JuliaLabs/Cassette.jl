const TESTDIR = dirname(@__FILE__)

include(joinpath(TESTDIR, "MetaprogammingTests.jl"))
include(joinpath(TESTDIR, "ContextTests.jl"))
include(joinpath(TESTDIR, "TracingTests.jl"))
