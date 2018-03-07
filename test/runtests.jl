const TESTDIR = dirname(@__FILE__)

include(joinpath(TESTDIR, "AnonymousTests.jl"))
include(joinpath(TESTDIR, "ContextTests.jl"))
include(joinpath(TESTDIR, "APITests.jl"))
