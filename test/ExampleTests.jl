module ExampleTests

using Test, Cassette
using Cassette: @context, @prehook, @posthook, @primitive, @pass, overdub, Box, Tag

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
@test length(MESSAGES) > 90

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
Cassette.metatype(::Type{<:BazCtx}, ::Type{<:Integer}) = Float64
n = rand()
ctx = BazCtx(baz_identity)
result = overdub(ctx, baz_identity; boxes = Val(true))(Box(ctx, 1, n))
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
    foo2.a = foo2.b
    return foo2.a.x
end

@context FooBarCtx
Cassette.metatype(::Type{<:FooBarCtx}, ::Type{<:Integer}) = Float64
n = rand()
ctx = FooBarCtx(foo_bar_identity)
result = overdub(ctx, foo_bar_identity; boxes = Val(true))(Box(ctx, 1, n))
@test result === Box(ctx, 1, n)

############################################################################################

sig_collection = DataType[]
@context PassCtx
overdub(PassCtx, sum; pass = @pass((sig, cinfo) -> (push!(sig_collection, sig); cinfo)))(rand(3))
@test !isempty(sig_collection) && all(T -> T <: Tuple, sig_collection)

############################################################################################

mutable struct Count{T}
    count::Int
end

@context CountCtx2

@prehook function (::Any)(arg::T, args::T...) where {T,__CONTEXT__<:CountCtx2,__METADATA__<:Count{T}}
    __trace__.metadata.count += 1
end

mapstr(x) = map(string, x)
c = Count{Union{String,Int}}(0)
@test overdub(CountCtx2, mapstr, metadata = c)(1:10) == mapstr(1:10)
@test c.count > 1000

############################################################################################

@context NestedCtx

function nested_test(n, x)
    if n < 1
        return sin(x)
    else
        return overdub(NestedCtx, nested_test)(n - 1, x + x)
    end
end

x = rand()
ctxs = NestedCtx[]
tag_id = objectid(typeof(nested_test))
nested_ctx_type = NestedCtx{Tag{NestedCtx{Tag{NestedCtx{Tag{Nothing,tag_id}},tag_id}},tag_id}}

@prehook function (::Any)(args...) where {__CONTEXT__<:NestedCtx}
    !(in(__trace__.context, ctxs)) && push!(ctxs, __trace__.context)
end

@test overdub(NestedCtx, nested_test)(2, x) === sin(x + x + x + x)
@test any(x -> isa(x, nested_ctx_type), ctxs)

end # module
