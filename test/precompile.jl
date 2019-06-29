using Pkg
using Test

testcode = raw"""
module MyPkg

using Cassette

Cassette.@context Ctx
const mypass = Cassette.@pass (ctx, ref) -> ref.code_info

end # module
"""

mktempdir() do dir
# for debugging use: 
# let dir = mktempdir()
# @show dir
    cd(dir) do
        Pkg.generate("MyPkg")
        open(joinpath("MyPkg", "src", "MyPkg.jl"), "w") do io
            write(io, testcode)
        end
        Pkg.activate("MyPkg")
        Pkg.develop(PackageSpec(path=joinpath(@__DIR__, ".."))) # add Cassette

        run(pipeline(`$(Base.julia_cmd()) --project=./MyPkg -e "using MyPkg"`, stderr="errs.log"))
        errs = read("errs.log", String)
        @test !occursin("WARNING: Method definition overdub", errs)
    end
end


