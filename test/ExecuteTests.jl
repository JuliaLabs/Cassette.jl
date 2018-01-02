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

Cassette.@context RosCtx

MESSAGES = String[]
Cassette.@hook RosCtx f(args...) = push!(MESSAGES, string("calling ", f, args))
@test Cassette.overdub(RosCtx, rosenbrock)(x) == rosenbrock(x)
@test length(MESSAGES) > 100

Cassette.@hook RosCtx meta f(args...) = push!(meta, string("calling ", f, args))
meta = String[]
@test Cassette.overdub(RosCtx, rosenbrock, metadata = meta)(x) == rosenbrock(x)
@test MESSAGES == meta

Cassette.@hook RosCtx meta f(args...) = nothing
Cassette.@hook RosCtx meta f(args::Number...) = push!(meta, args)
meta = Any[]
@test Cassette.overdub(RosCtx, rosenbrock, metadata = meta)(x) == rosenbrock(x)
for args in meta
    @test all(x -> isa(x, Number), args)
end

############################################################################################

x = rand()
sin_plus_cos(x) = sin(x) + cos(x)
Cassette.@context SinCtx
@test Cassette.overdub(SinCtx, sin_plus_cos)(x) === sin_plus_cos(x)
Cassette.@primitive SinCtx (::typeof(sin))(x) = cos(x)
@test Cassette.overdub(SinCtx, sin_plus_cos)(x) === (2 * cos(x))

############################################################################################

x = 2
foldmul(x, args...) = Core._apply(Base.afoldl, (*, x), args...)
Cassette.@context FoldCtx
@test Cassette.overdub(FoldCtx, foldmul)(x) === foldmul(x)

############################################################################################

Cassette.@context CountCtx
count1 = Ref(0)
Cassette.@hook CountCtx count f(args::Number...) = (count[] += 1)
Cassette.overdub(CountCtx, sin, metadata = count1)(1)
Cassette.@hook CountCtx count f(args::Number...) = (count[] += 2)
count2 = Ref(0)
Cassette.overdub(CountCtx, sin, metadata = count2)(1)
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
ctx = BazCtx(baz_identity)
result = Cassette.overdub(ctx, baz_identity)(Cassette.Box(ctx, 1, n))
@test result === Cassette.Box(ctx, 1, n)

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
ctx = FooBarCtx(foo_bar_identity)
result = Cassette.overdub(ctx, foo_bar_identity)(Cassette.Box(ctx, 1, n))
@test result === Cassette.Box(ctx, 1, n)

end # module ExecuteTests
