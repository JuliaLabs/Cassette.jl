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
@test length(MESSAGES) == 126

Cassette.@hook MyCtx meta f(args...) = push!(meta, string("calling ", f, args))
meta = String[]
Cassette.@execute MyCtx meta rosenbrock(x)
@test MESSAGES == meta

Cassette.@hook MyCtx meta f(args...) = nothing
Cassette.@hook MyCtx meta f(args::Number...) = push!(meta, args)
meta = Any[]
Cassette.@execute MyCtx meta rosenbrock(x)
for args in meta
    @test all(x -> isa(x, Number), args)
end

############################################################################################

struct Bar{X,Y,Z}
    x::X
    y::Y
    z::Z
end

mutable struct Foo
    a::Bar{Int}
    b
end

function f(x)
    bar = Bar(x, x + 1, x + 2)
    foo = Foo(bar, "ha")
    foo.b = bar
    foo.a = Bar(4,5,6)
    foo2 = Foo(foo.a, foo.b)
    foo2.b.x
end

Cassette.@context MyCtx2
n = rand()
result = Cassette.@execute MyCtx2 f(@Wrapper(1, n))
@test result === Cassette.Wrapper(MyCtx2(f), 1, n)

end # module InterceptTests
