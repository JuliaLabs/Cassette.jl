const TESTDIR = dirname(@__FILE__)

include(joinpath(TESTDIR, "ContextTests.jl"))
include(joinpath(TESTDIR, "ExecuteTests.jl"))
