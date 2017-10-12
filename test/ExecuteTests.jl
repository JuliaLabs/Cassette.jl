module ExecuteTests

using Test
using Cassette

function rosenbrock(x::Vector{Float64})
    a = 1.0
    b = 100.0
    result = 0.0
    for i in 1:length(x)-1
        result += (a - x[i])^2 + b*(x[i+1] - x[i]^2)^2
    end
    return result
end

x = rand(2)

Cassette.@context MyCtx
MESSAGES = String[]
Cassette.@hook MyCtx f(args...) = push!(MESSAGES, string("calling ", f, args))
Cassette.@execute MyCtx rosenbrock(x)
@test length(MESSAGES) == 83

Cassette.@hook MyCtx cfg f(args...) = push!(cfg, string("calling ", f, args))
cfg = String[]
Cassette.@execute MyCtx cfg rosenbrock(x)
@test MESSAGES == cfg

Cassette.@hook MyCtx cfg f(args...) = nothing
Cassette.@hook MyCtx cfg f(args::Number...) = push!(cfg, args)
cfg = Any[]
Cassette.@execute MyCtx cfg rosenbrock(x)
for args in cfg
    @test all(x -> isa(x, Number), args)
end

end # module InterceptTests
