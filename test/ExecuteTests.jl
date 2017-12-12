module ExecuteTests

using Test, Cassette

############################################################################################

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

############################################################################################

Cassette.@context MyCtx
MESSAGES = String[]
Cassette.@hook MyCtx f(args...) = push!(MESSAGES, string("calling ", f, args))
Cassette.@execute MyCtx rosenbrock(x)
@test length(MESSAGES) == 125

############################################################################################

Cassette.@hook MyCtx meta f(args...) = push!(meta, string("calling ", f, args))
meta = String[]
Cassette.@execute MyCtx meta rosenbrock(x)
@test MESSAGES == meta

############################################################################################

Cassette.@hook MyCtx meta f(args...) = nothing
Cassette.@hook MyCtx meta f(args::Number...) = push!(meta, args)
meta = Any[]
Cassette.@execute MyCtx meta rosenbrock(x)
for args in meta
    @test all(x -> isa(x, Number), args)
end

############################################################################################

x = rand()
sin_plus_cos(x) = sin(x) + cos(x)
Cassette.@context SinCtx
@test Cassette.@execute(SinCtx, sin_plus_cos(x)) === sin_plus_cos(x)
Cassette.@primitive SinCtx (::typeof(sin))(x) = cos(x)
@test Cassette.@execute(SinCtx, sin_plus_cos(x)) === (2 * cos(x))

############################################################################################

x = 2
foldmul(x, args...) = Core._apply(Base.afoldl, (*, x), args...)
Cassette.@context FoldCtx
@test Cassette.@execute(FoldCtx, foldmul(x)) === foldmul(x)

############################################################################################

Cassette.@context CountCtx
count1 = Ref(0)
Cassette.@hook CountCtx count f(args::Number...) = (count[] += 1)
Cassette.@execute CountCtx count1 sin(1)
Cassette.@hook CountCtx count f(args::Number...) = (count[] += 2)
count2 = Ref(0)
Cassette.@execute CountCtx count2 sin(1)
@test (2 * count1[]) === count2[]

############################################################################################

struct Baz
    x::Int
    y::Float64
    z::String
end

baz_identity(x::Int) = Baz(x, float(x), "$x").x

Cassette.@context BazCtx
n = rand()
result = Cassette.@execute BazCtx baz_identity(@Wrapper(1, n))
@test result === Cassette.Wrapper(BazCtx(baz_identity), 1, n)

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

function foo_bar_identity(x)
    bar = Bar(x, x + 1, x + 2)
    foo = Foo(bar, "ha")
    foo.b = bar
    foo.a = Bar(4,5,6)
    foo2 = Foo(foo.a, foo.b)
    foo2.b.x
end

Cassette.@context FooBarCtx
n = rand()
result = Cassette.@execute FooBarCtx foo_bar_identity(@Wrapper(1, n))
@test result === Cassette.Wrapper(FooBarCtx(foo_bar_identity), 1, n)

end # module ExecuteTests
