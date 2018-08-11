using Test, Cassette, LinearAlgebra
using Cassette: @context, @pass, @overdub, overdub, hasmetadata, metadata, hasmetameta,
                metameta, untag, tag, enabletagging, untagtype, istagged, istaggedtype,
                Tagged, fallback, canoverdub, similarcontext

@time @testset "unit tests" begin include("unittests.jl") end
@time @testset "misc. tests (w/o tagging)" begin include("misctests.jl") end
@time @testset "misc. tests (w/ tagging)" begin include("misctaggingtests.jl") end
