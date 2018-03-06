module ExecuteTests

using Test, Cassette
using Cassette: @context, @prehook, @posthook, @primitive, @pass, overdub, Box

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

@context RosCtx

MESSAGES = String[]
@prehook (f::Any)(args...) where {__CONTEXT__<:RosCtx} = push!(MESSAGES, string("calling ", f, args))
@test overdub(RosCtx, rosenbrock)(x) == rosenbrock(x)
@test length(MESSAGES) > 100

@prehook (f::Any)(args...) where {__CONTEXT__<:RosCtx} = push!(__trace__.metadata, string("calling ", f, args))
meta = String[]
@test overdub(RosCtx, rosenbrock, metadata = meta)(x) == rosenbrock(x)
@test MESSAGES == meta

@prehook (f::Any)(args...) where {__CONTEXT__<:RosCtx} = nothing
@prehook (f::Any)(args::Number...) where {__CONTEXT__<:RosCtx} = push!(__trace__.metadata, args)
meta = Any[]
@test overdub(RosCtx, rosenbrock, metadata = meta)(x) == rosenbrock(x)
for args in meta
    @test all(x -> isa(x, Number), args)
end

############################################################################################

x = rand()
sin_plus_cos(x) = sin(x) + cos(x)
@context SinCtx
@test overdub(SinCtx, sin_plus_cos)(x) === sin_plus_cos(x)
@primitive sin(x) where {__CONTEXT__<:SinCtx} = cos(x)
@test overdub(SinCtx, sin_plus_cos)(x) === (2 * cos(x))

############################################################################################

x = 2
foldmul(x, args...) = Core._apply(Base.afoldl, (*, x), args...)
@context FoldCtx
@test overdub(FoldCtx, foldmul)(x) === foldmul(x)

############################################################################################

@context CountCtx
count1 = Ref(0)
@prehook (f::Any)(args::Number...) where {__CONTEXT__<:CountCtx} = (__trace__.metadata[] += 1)
overdub(CountCtx, sin, metadata = count1)(1)
@prehook (f::Any)(args::Number...) where {__CONTEXT__<:CountCtx} = (__trace__.metadata[] += 2)
count2 = Ref(0)
overdub(CountCtx, sin, metadata = count2)(1)
@test (2 * count1[]) === count2[]

############################################################################################

square_closure(x) = (y -> y * x)(x)
@context SqrCtx
x = rand()
@test square_closure(x) == overdub(SqrCtx, square_closure)(x)

############################################################################################

comprehension1(x) = [i for i in x]
comprehension2(f, x, y) = [f(x, i) for i in y]
@context CompCtx
f, x, y = hypot, rand(), rand(2)
@test comprehension1(x) == overdub(CompCtx, comprehension1)(x)
@test comprehension1(y) == overdub(CompCtx, comprehension1)(y)
@test comprehension2(f, x, y) == overdub(CompCtx, comprehension2)(f, x, y)

############################################################################################

struct Baz
    x::Int
    y::Float64
    z::String
end

baz_identity(x::Int) = Baz(x, float(x), "$x").x

@context BazCtx
n = rand()
ctx = BazCtx(baz_identity)
result = overdub(ctx, baz_identity)(Box(ctx, 1, n))
@test result === Box(ctx, 1, n)

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

@context FooBarCtx
n = rand()
ctx = FooBarCtx(foo_bar_identity)
result = overdub(ctx, foo_bar_identity)(Box(ctx, 1, n))
@test result === Box(ctx, 1, n)

############################################################################################

sig_collection = DataType[]
@context PassCtx
tapepass(sig, cinfo) = (push!(sig_collection, sig); cinfo)
@pass TapePass tapepass
overdub(PassCtx, sum; pass = TapePass())(rand(3))
@test !isempty(sig_collection) && all(T -> T <: Tuple, sig_collection)

end # module ExecuteTests
