using Test, Cassette
using Cassette: @context, @pass, @overdub, overdub, hasmetadata, metadata, hasmetameta,
                metameta, untag, tag, enabletagging, untagtype, istagged, istaggedtype,
                Tagged, fallback, canoverdub, similarcontext

const Typ = Core.Typeof

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

messages = String[]
Cassette.prehook(::RosCtx, f, args...) = push!(messages, string("calling ", f, args))
@test overdub(RosCtx(), rosenbrock, x) == rosenbrock(x)
@test length(messages) > 90

Cassette.prehook(ctx::RosCtx, f, args...) = push!(ctx.metadata, string("calling ", f, args))
messages2 = String[]
@test overdub(RosCtx(metadata=messages2), rosenbrock, x) == rosenbrock(x)
@test messages == messages2

Cassette.prehook(::RosCtx, f, args...) = nothing
Cassette.prehook(ctx::RosCtx, f, args::Number...) = push!(ctx.metadata, args)
argslog = Any[]
@test @overdub(RosCtx(metadata=argslog), rosenbrock(x)) == rosenbrock(x)
for args in argslog
    @test all(x -> isa(x, Number), args)
end

############################################################################################

@context HookCtx
Cassette.prehook(ctx::HookCtx, f, args...) = push!(ctx.metadata[1], (f, args))
Cassette.posthook(ctx::HookCtx, out, f, args...) = push!(ctx.metadata[2], (f, args))

ctx = HookCtx(metadata=(Any[], Any[]))
@overdub(ctx, 1 + 1 * 1)
# assumes the prehook trace is `[(*, ...), (mul_int, ...), (+, ...), (add_int, ...)]`
@test ctx.metadata[1][1] == ctx.metadata[2][2]
@test ctx.metadata[1][2] == ctx.metadata[2][1]
@test ctx.metadata[1][3] == ctx.metadata[2][4]
@test ctx.metadata[1][4] == ctx.metadata[2][3]

Cassette.execute(::HookCtx, ::typeof(+), args...) = +(args...)
empty!(ctx.metadata[1])
empty!(ctx.metadata[2])

@overdub(ctx, 1 + 1 * 1)
# assumes the prehook trace is `[(*, ...), (mul_int, ...), (+, ...)]`
@test ctx.metadata[1][1] == ctx.metadata[2][2]
@test ctx.metadata[1][2] == ctx.metadata[2][1]
@test ctx.metadata[1][3] == ctx.metadata[2][3]

############################################################################################

x = rand()
sin_plus_cos(x) = sin(x) + cos(x)
@context SinCtx
@test @overdub(SinCtx(), sin_plus_cos(x)) === sin_plus_cos(x)
Cassette.execute(::SinCtx, ::Typ(sin), x) = cos(x)
@test @overdub(SinCtx(), sin_plus_cos(x)) === (2 * cos(x))

############################################################################################

x = 2
foldmul(x, args...) = Core._apply(Base.afoldl, (*, x), args...)
@context FoldCtx
@test @overdub(FoldCtx(), foldmul(x)) === foldmul(x)

############################################################################################

@context CountCtx
count1 = Ref(0)
Cassette.prehook(ctx::CountCtx, f, args::Number...) = (ctx.metadata[] += 1)
@overdub(CountCtx(metadata=count1), sin(1))
Cassette.prehook(ctx::CountCtx, f, args::Number...) = (ctx.metadata[] += 2)
count2 = Ref(0)
@overdub(CountCtx(metadata=count2), sin(1))
@test (2 * count1[]) === count2[]

############################################################################################

square_closure(x) = (y -> y * x)(x)
@context SqrCtx
x = rand()
@test square_closure(x) == @overdub(SqrCtx(), square_closure(x))

############################################################################################

comprehension1(x) = [i for i in x]
comprehension2(f, x, y) = [f(x, i) for i in y]
@context CompCtx
f, x, y = hypot, rand(), rand(2)
@test comprehension1(x) == @overdub(CompCtx(), comprehension1(x))
@test comprehension1(y) == @overdub(CompCtx(), comprehension1(y))
@test comprehension2(f, x, y) == @overdub(CompCtx(), comprehension2(f, x, y))

############################################################################################

sig_collection = DataType[]
@context PassCtx
mypass = @pass (ctx, sig, cinfo) -> (push!(sig_collection, sig); cinfo)
@overdub(PassCtx(pass=mypass), sum(rand(3)))
@test !isempty(sig_collection) && all(T -> T <: Tuple, sig_collection)

############################################################################################

mutable struct Count{T}
    count::Int
end

@context CountCtx2

function Cassette.prehook(ctx::CountCtx2{Count{T}}, f, arg::T, args::T...) where {T}
    ctx.metadata.count += 1
end

mapstr(x) = map(string, x)
c = Count{Union{String,Int}}(0)
@test @overdub(CountCtx2(metadata=c), mapstr(1:10)) == mapstr(1:10)
@test c.count > 1000

############################################################################################

@context WorldCtx

worldtest = 0
oldctx = WorldCtx()
overdub(oldctx, sin, 1)

Cassette.prehook(::WorldCtx, args...) = (global worldtest += 1)
overdub(WorldCtx(), sin, 1)
@test worldtest > 100

tmp = worldtest
overdub(oldctx, sin, 1)
@test tmp < worldtest

tmp = worldtest
Cassette.prehook(::WorldCtx, args...) = nothing
overdub(WorldCtx(), sin, 1)
@test tmp === worldtest

############################################################################################

@context TraceCtx

function Cassette.execute(ctx::TraceCtx, args...)
    subtrace = Any[]
    push!(ctx.metadata, args => subtrace)
    if canoverdub(ctx, args...)
        newctx = similarcontext(ctx, metadata = subtrace)
        return overdub(newctx, args...)
    else
        return fallback(ctx, args...)
    end
end

trace = Any[]
x, y, z = rand(3)
trtest(x, y, z) = x*y + y*z
@test @overdub(TraceCtx(metadata = trace), trtest(x, y, z)) == trtest(x, y, z)
@test trace == Any[
    (trtest,x,y,z) => Any[
        (*,x,y) => Any[(Base.mul_float,x,y)=>Any[]]
        (*,y,z) => Any[(Base.mul_float,y,z)=>Any[]]
        (+,x*y,y*z) => Any[(Base.add_float,x*y,y*z)=>Any[]]
    ]
]

# jrevels/Cassette.jl#48
tracekw = Any[]
trkwtest(x; _y = 1.0, _z = 2.0) = trtest(x, _y, _z)
@overdub(TraceCtx(metadata = tracekw), trkwtest(x, _y = y, _z = z)) == trtest(x, y, z)
subtracekw = first(Iterators.filter(t -> t[1] === (Core.kwfunc(trkwtest), (_y = y, _z = z), trkwtest, x), tracekw))[2]
@test subtracekw == trace

############################################################################################

@context NestedReflectCtx
r_pre = Cassette.reflect((typeof(sin), Int))
r_post = Cassette.reflect((typeof(overdub), typeof(NestedReflectCtx()), typeof(sin), Int))
@test isa(r_pre, Cassette.Reflection) && isa(r_post, Cassette.Reflection)
Cassette.overdub_pass!(r_pre, typeof(NestedReflectCtx()))
@test r_pre.code_info.code == r_post.code_info.code

############################################################################################

struct Baz
    x::Int
    y::Float64
    z::String
end

baz_identity(x::Int) = Baz(x, float(x), "$x").x

@context BazCtx
Cassette.metadatatype(::Type{<:BazCtx}, ::Type{<:Number}) = Float64
x, n = rand(Int), rand()
ctx = enabletagging(BazCtx(), baz_identity)
result = overdub(ctx, baz_identity, tag(x, ctx, n))

@test untag(result, ctx) === x
@test untagtype(typeof(result), typeof(ctx)) === typeof(x)
@test istagged(result, ctx)
@test istaggedtype(typeof(result), typeof(ctx))

@test metadata(result, ctx) === n
@test metameta(result, ctx) === Cassette.NoMetaMeta()
@test hasmetadata(result, ctx)
@test !hasmetameta(result, ctx)

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
    array = Float64[]
    push!(array, foo2.a.x)
    return [array[1]][1]
end

@context FooBarCtx
Cassette.metadatatype(::Type{<:FooBarCtx}, ::Type{T}) where T<:Number = T
x, n = 1, 2
ctx = enabletagging(FooBarCtx(), foo_bar_identity)
result = overdub(ctx, foo_bar_identity, tag(x, ctx, n))

@test untag(result, ctx) === float(x)
@test untagtype(typeof(result), typeof(ctx)) === Float64
@test istagged(result, ctx)
@test istaggedtype(typeof(result), typeof(ctx))

@test metadata(result, ctx) === float(n)
@test metameta(result, ctx) === Cassette.NoMetaMeta()
@test hasmetadata(result, ctx)
@test !hasmetameta(result, ctx)

############################################################################################

@context TaggedTupleCtx
Cassette.metadatatype(::Type{<:TaggedTupleCtx}, ::DataType) = Float64
x = rand()
ctx = enabletagging(TaggedTupleCtx(), 1)
result = overdub(ctx, x -> (x, [x], 1), x)

@test untag(result, ctx) == (x, [x], 1)
@test untagtype(typeof(result), typeof(ctx)) === typeof((x, [x], 1))
@test istagged(result, ctx)
@test istaggedtype(typeof(result), typeof(ctx))

@test metadata(result, ctx) === Cassette.NoMetaData()
@test isa(metameta(result, ctx), Tuple{
    Cassette.Immutable{Cassette.Meta{Float64,Cassette.NoMetaMeta}},
    Cassette.Immutable{Cassette.Meta{Float64,Array{Cassette.Meta{Float64,Cassette.NoMetaMeta},1}}},
    Cassette.Immutable{Cassette.Meta{Float64,Cassette.NoMetaMeta}}
})
@test !hasmetadata(result, ctx)
@test hasmetameta(result, ctx)

############################################################################################

@context ApplyCtx
x = rand()
applytest(x) = Core._apply(hypot, (x,), (1,x), 1, x, (1,2))
ctx = enabletagging(ApplyCtx(), 1)
@test overdub(ctx, applytest, tag(x, ctx)) === applytest(x)

############################################################################################

@context VATupleCtx
x = rand(5)
ctx = enabletagging(VATupleCtx(), 1)
result = overdub(ctx, broadcast, sin, x)

@test untag(result, ctx) == sin.(x)
@test untagtype(typeof(result), typeof(ctx)) === typeof(sin.(x))
@test istagged(result, ctx)
@test istaggedtype(typeof(result), typeof(ctx))

@test metadata(result, ctx) === Cassette.NoMetaData()
@test metameta(result, ctx) === Cassette.NoMetaMeta()
@test !hasmetadata(result, ctx)
@test !hasmetameta(result, ctx)

############################################################################################

@context BroadcastCtx
v, m = rand(5), rand(5)
ctx = enabletagging(BroadcastCtx(), 1)
Cassette.metadatatype(::Type{<:BroadcastCtx}, ::Type{T}) where {T<:Number} = T

result = overdub(ctx, broadcast, (v, m) -> tag(v, ctx, m), v, m)
@test untag(result, ctx) == v
@test untagtype(typeof(result), typeof(ctx)) === typeof(v)
@test istagged(result, ctx)
@test istaggedtype(typeof(result), typeof(ctx))
@test metadata(result, ctx) === Cassette.NoMetaData()
@test m == map(metameta(result, ctx)) do x
    @test x.meta === Cassette.NoMetaMeta()
    return x.data
end
@test !hasmetadata(result, ctx)
@test hasmetameta(result, ctx)

result = @overdub(ctx, ((v, m) -> tag(v, ctx, m)).(v, m[1]))
@test untag(result, ctx) == v
@test untagtype(typeof(result), typeof(ctx)) === typeof(v)
@test istagged(result, ctx)
@test istaggedtype(typeof(result), typeof(ctx))
@test metadata(result, ctx) === Cassette.NoMetaData()
foreach(metameta(result, ctx)) do x
    @test x.meta === Cassette.NoMetaMeta()
    @test x.data === m[1]
end
@test !hasmetadata(result, ctx)
@test hasmetameta(result, ctx)

############################################################################################

@context BroadcastCtx2

Cassette.metadatatype(::Type{<:BroadcastCtx2{Int}}, ::Type{T}) where {T<:Number} = Vector{T}
Cassette.metadatatype(::Type{<:BroadcastCtx2{Val{N}}}, ::Type{T}) where {N,T<:Number} = NTuple{N,T}

v, m = rand(5), rand(5)
ctx = enabletagging(BroadcastCtx2(metadata=10), 1)
result = overdub(ctx, broadcast, (v, m) -> tag(v, ctx, m), v, [m])
@test untag(result, ctx) == v
@test untagtype(typeof(result), typeof(ctx)) === typeof(v)
@test istagged(result, ctx)
@test istaggedtype(typeof(result), typeof(ctx))
@test metadata(result, ctx) === Cassette.NoMetaData()
@test all(e -> e == m, map(metameta(result, ctx)) do x
    @test x.meta === Cassette.NoMetaMeta()
    return x.data
end)
@test !hasmetadata(result, ctx)
@test hasmetameta(result, ctx)

v, m = rand(5), (1.0,2.0,3.0)
ctx = enabletagging(BroadcastCtx2(metadata=Val(3)), 1)
result = overdub(ctx, broadcast, (v, m) -> tag(v, ctx, m), v, [m])
@test untag(result, ctx) == v
@test untagtype(typeof(result), typeof(ctx)) === typeof(v)
@test istagged(result, ctx)
@test istaggedtype(typeof(result), typeof(ctx))
@test metadata(result, ctx) === Cassette.NoMetaData()
foreach(metameta(result, ctx)) do x
    @test x.meta === Cassette.NoMetaMeta()
    @test x.data === m
end
@test !hasmetadata(result, ctx)
@test hasmetameta(result, ctx)

############################################################################################

@context MetaTypeCtx

@test Cassette.metatype(typeof(MetaTypeCtx()), DataType) === Cassette.Meta{Cassette.NoMetaData,Cassette.NoMetaMeta}

ctx = enabletagging(MetaTypeCtx(), 1)

@test overdub(ctx, T -> (T,T), Float64) === (Float64, Float64)

############################################################################################

# issue #51
@context GemvCtx
using LinearAlgebra

α, β = 1.0, 2.0
A, X, Y = rand(Float64, 10, 10), rand(Float64, 10), rand(Float64, 10)
Y_copy = copy(Y)
Y_out = overdub(GemvCtx(), LinearAlgebra.BLAS.gemv!, 'T', α, A, X, β, Y)
Y_copy_out = LinearAlgebra.BLAS.gemv!('T', α, A, X, β, Y_copy)
@test Y_out == Y_copy_out
@test Y_out === Y
@test Y_copy_out === Y_copy

############################################################################################

@context InferCtx

dispatchtupletest(::Type{T}) where {T} = Base.isdispatchtuple(Tuple{T}) ? T : Any
relu(x) = max(zero(x), x)
relulayer(W, x, b) = relu.(W*x .+ b)

@inferred(overdub(InferCtx(), typejoin, Float32, Float32, Float32))
@inferred(overdub(InferCtx(), dispatchtupletest, Float32))
@inferred(overdub(InferCtx(), (a, b) -> Core.apply_type(a, b), AbstractVector, Int))
@inferred(overdub(InferCtx(), eltype, rand(1)))
@inferred(overdub(InferCtx(), *, rand(1, 1), rand(1, 1)))
@inferred(overdub(InferCtx(), *, rand(Float32, 1, 1), rand(Float32, 1, 1)))
@inferred(overdub(InferCtx(), *, rand(Float32, 1, 1), rand(Float32, 1)))
@inferred(overdub(InferCtx(), rosenbrock, rand(1)))
@inferred(overdub(InferCtx(), rand, Float32, 1))
@inferred(overdub(InferCtx(), broadcast, +, rand(1), rand(1)))
@inferred(overdub(InferCtx(), relulayer, rand(Float64, 1, 1), rand(Float32, 1), rand(Float32, 1)))

############################################################################################

@context TagConditionalCtx
ctx = enabletagging(TagConditionalCtx(), 1)
@test overdub(ctx, x -> x ? 1 : 2, tag(true, ctx)) === 1

function condtest(b)
    i = 1
    while i > b
        i -= 1
    end
    if b end
    return i
end

@test overdub(ctx, condtest, tag(false, ctx)) === 0

############################################################################################

@context KwargCtx
ctx = enabletagging(KwargCtx(), 1)
kwargtest(x; y = 1) = x + y
@test overdub(ctx, _y -> kwargtest(3; y = _y), tag(2, ctx)) === 5

############################################################################################

@context RecurTagCtx

mutable struct RecurType
    r::RecurType
    RecurType() = new()
end

ctx = enabletagging(RecurTagCtx(), 1)
x = tag(RecurType(), ctx)

@overdub(ctx, x.r = RecurType())
@test istagged(x, ctx)
@test !hasmetadata(x, ctx)
@test hasmetameta(x, ctx)
@test isa(metameta(x, ctx), NamedTuple{(:r,),Tuple{Cassette.Mutable{Cassette.Meta}}})
@test isdefined(untag(x, ctx), :r)
@test !(isdefined(untag(x, ctx).r, :r))

############################################################################################

@context CrazyPropCtx

module CrazyPropModule
    const CONST_BINDING = Float64[]

    global GLOBAL_BINDING = 0.0

    struct Foo
        vector::Vector{Float64}
    end

    mutable struct FooContainer
        foo::Foo
    end

    mutable struct PlusFunc
        x::Float64
    end

    (f::PlusFunc)(x) = f.x + x

    const PLUSFUNC = PlusFunc(0.0)

    # implements a very convoluted `sum(x) * sum(y)`
    function crazy_sum_mul(x::Vector{Float64}, y::Vector{Float64})
        @assert length(x) === length(y)
        fooc = FooContainer(Foo(x))
        tmp = y

        # this loop sets:
        # `const_binding == x`
        # `global_binding == prod(y)`
        for i in 1:length(y)
            if iseven(i) # `fooc.foo.vector === y && tmp === x`
                v = fooc.foo.vector[i]
                push!(CONST_BINDING, tmp[i])
                global GLOBAL_BINDING = PLUSFUNC(v)
                PLUSFUNC.x = GLOBAL_BINDING
                fooc.foo = Foo(x)
                tmp = y
            else # `fooc.foo.vector === x && tmp === y`
                v = fooc.foo.vector[i]
                push!(CONST_BINDING, v)
                global GLOBAL_BINDING = PLUSFUNC(tmp[i])
                PLUSFUNC.x = GLOBAL_BINDING
                fooc.foo = Foo(y)
                tmp = x
            end
        end

        # accumulate result
        z = sum(CONST_BINDING) * GLOBAL_BINDING

        # reset global state
        empty!(CONST_BINDING)
        PLUSFUNC.x = 0.0
        global GLOBAL_BINDING = 0.0
        return z
    end
end

x, y = rand(100), rand(100)
primal_result = CrazyPropModule.crazy_sum_mul(x, y)
@test isapprox(primal_result, sum(x) * sum(y))

Cassette.metadatatype(::Type{<:CrazyPropCtx}, ::Type{T}) where T<:Number = T
function Cassette.execute(ctx::CrazyPropCtx, ::typeof(*), x, y)
    z = untag(x, ctx) * untag(y, ctx)
    if hasmetadata(x, ctx) && hasmetadata(y, ctx)
        return tag(z, ctx, metadata(x, ctx) * metadata(y, ctx))
    elseif hasmetadata(x, ctx)
        return tag(z, ctx, metadata(x, ctx))
    elseif hasmetadata(y, ctx)
        return tag(z, ctx, metadata(y, ctx))
    else
        return z
    end
end
function Cassette.execute(ctx::CrazyPropCtx, ::typeof(+), x, y)
    z = untag(x, ctx) + untag(y, ctx)
    if hasmetadata(x, ctx) && hasmetadata(y, ctx)
        return tag(z, ctx, metadata(x, ctx) + metadata(y, ctx))
    elseif hasmetadata(x, ctx)
        return tag(z, ctx, metadata(x, ctx))
    elseif hasmetadata(y, ctx)
        return tag(z, ctx, metadata(y, ctx))
    else
        return z
    end
end
ctx = enabletagging(CrazyPropCtx(), CrazyPropModule.crazy_sum_mul)
xm, ym = rand(100), rand(100)
tx = overdub(ctx, broadcast, (v, m) -> tag(v, ctx, m), x, xm)
ty = overdub(ctx, broadcast, (v, m) -> tag(v, ctx, m), y, ym)
tagged_result = overdub(ctx, CrazyPropModule.crazy_sum_mul, tx, ty)

@test isapprox(untag(tagged_result, ctx), primal_result)
@test isapprox(metadata(tagged_result, ctx), CrazyPropModule.crazy_sum_mul(xm, ym))

############################################################################################

@context DiffCtx

const DiffCtxWithTag{T} = DiffCtx{Nothing,T}

Cassette.metadatatype(::Type{<:DiffCtx}, ::Type{T}) where {T<:Real} = T

tangent(x, context) = hasmetadata(x, context) ? metadata(x, context) : zero(untag(x, context))

function D(f, x)
    ctx = enabletagging(DiffCtx(), f)
    result = overdub(ctx, f, tag(x, ctx, oftype(x, 1.0)))
    return tangent(result, ctx)
end

function Cassette.execute(ctx::DiffCtxWithTag{T}, ::Typ(sin), x::Tagged{T,<:Real}) where {T}
    vx, dx = untag(x, ctx), tangent(x, ctx)
    return tag(sin(vx), ctx, cos(vx) * dx)
end

function Cassette.execute(ctx::DiffCtxWithTag{T}, ::Typ(cos), x::Tagged{T,<:Real}) where {T}
    vx, dx = untag(x, ctx), tangent(x, ctx)
    return tag(cos(vx), ctx, -sin(vx) * dx)
end

function Cassette.execute(ctx::DiffCtxWithTag{T}, ::Typ(*), x::Tagged{T,<:Real}, y::Tagged{T,<:Real}) where {T}
    vx, dx = untag(x, ctx), tangent(x, ctx)
    vy, dy = untag(y, ctx), tangent(y, ctx)
    return tag(vx * vy, ctx, vy * dx + vx * dy)
end

function Cassette.execute(ctx::DiffCtxWithTag{T}, ::Typ(*), x::Tagged{T,<:Real}, y::Real) where {T}
    vx, dx = untag(x, ctx), tangent(x, ctx)
    return tag(vx * y, ctx, y * dx)
end

function Cassette.execute(ctx::DiffCtxWithTag{T}, ::Typ(*), x::Real, y::Tagged{T,<:Real}) where {T}
    vy, dy = untag(y, ctx), tangent(y, ctx)
    return tag(x * vy, ctx, x * dy)
end

function Cassette.execute(ctx::DiffCtxWithTag{T}, ::Typ(+), x::Tagged{T,<:Real}, y::Tagged{T,<:Real}) where {T}
    vx, dx = untag(x, ctx), tangent(x, ctx)
    vy, dy = untag(y, ctx), tangent(y, ctx)
    return tag(vx + vy, ctx, dx + dy)
end

function Cassette.execute(ctx::DiffCtxWithTag{T}, ::Typ(+), x::Tagged{T,<:Real}, y::Real) where {T}
    vx, dx = untag(x, ctx), tangent(x, ctx)
    return tag(vx + y, ctx, dx)
end

function Cassette.execute(ctx::DiffCtxWithTag{T}, ::Typ(+), x::Real, y::Tagged{T,<:Real}) where {T}
    vy, dy = untag(y, ctx), tangent(y, ctx)
    return tag(x + vy, ctx, dy)
end

Cassette.execute(ctx::DiffCtx, ::typeof(println), args...) = println(args...)

@test D(sin, 1) === cos(1)
@test D(x -> D(sin, x), 1) === -sin(1)
@test D(x -> sin(x) * cos(x), 1) === cos(1)^2 - sin(1)^2
@test D(x -> x * D(y -> x * y, 1), 2) === 4
@test D(x -> x * D(y -> x * y, 2), 1) === 2
@test D(x -> x * foo_bar_identity(x), 1) === 2.0

x = rand()
@test D(x -> (x + 2) * (3 + x), x) === 2x + 5
@test D(x -> CrazyPropModule.crazy_sum_mul([x], [x]), x) === (x + x)
@test D(x -> CrazyPropModule.crazy_sum_mul([x, 2], [3, x]), x) === 2x + 5
