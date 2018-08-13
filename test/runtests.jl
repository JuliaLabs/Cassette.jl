using Test, Cassette, LinearAlgebra
using Cassette: @context, @pass, @overdub, overdub, hasmetadata, metadata, hasmetameta,
                metameta, untag, tag, enabletagging, untagtype, istagged, istaggedtype,
                Tagged, fallback, canoverdub, similarcontext

println("running unit tests")
@time @testset "unit tests" begin include("unittests.jl") end

println("running misc. tests (w/o tagging)")
@time @testset "misc. tests (w/o tagging)" begin include("misctests.jl") end

println("running misc. tests (w/ tagging)")
@time @testset "misc. tests (w/ tagging)" begin include("misctaggingtests.jl") end
