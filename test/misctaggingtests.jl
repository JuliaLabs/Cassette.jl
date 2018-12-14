#############################################################################################

print("   running BazCtx test...")

before_time = time()

@context BazCtx

struct Baz
    x::Int
    y::Float64
    z::String
end

baz_identity(x::Int) = Baz(x, float(x), "$x").x

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

println("done (took ", time() - before_time, " seconds)")

#############################################################################################

print("   running FooBarCtx test...")

before_time = time()

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

println("done (took ", time() - before_time, " seconds)")

#############################################################################################

print("   running TaggedTupleCtx test...")

before_time = time()

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

println("done (took ", time() - before_time, " seconds)")

#############################################################################################

print("   running ApplyCtx test...")

before_time = time()

@context ApplyCtx
x = rand()
applytest(x) = Core._apply(hypot, (x,), (1, x), 1, x, (1, 2))
ctx = enabletagging(ApplyCtx(), 1)
@test overdub(ctx, applytest, tag(x, ctx)) === applytest(x)

println("done (took ", time() - before_time, " seconds)")

#############################################################################################

print("   running VATupleCtx test...")

before_time = time()

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

println("done (took ", time() - before_time, " seconds)")

#############################################################################################

print("   running BroadcastCtx test...")

before_time = time()

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

println("done (took ", time() - before_time, " seconds)")

#############################################################################################

print("   running BroadcastCtx2 test...")

before_time = time()

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

println("done (took ", time() - before_time, " seconds)")

#############################################################################################

print("   running MetaTypeCtx test...")

before_time = time()

@context MetaTypeCtx
@test Cassette.metatype(typeof(MetaTypeCtx()), DataType) === Cassette.Meta{Cassette.NoMetaData,Cassette.NoMetaMeta}
ctx = enabletagging(MetaTypeCtx(), 1)
@test overdub(ctx, T -> (T,T), Float64) === (Float64, Float64)

println("done (took ", time() - before_time, " seconds)")

#############################################################################################

print("   running TagConditionalCtx test...")

before_time = time()

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

println("done (took ", time() - before_time, " seconds)")

#############################################################################################

print("   running KwargCtx test...")

before_time = time()

@context KwargCtx
ctx = enabletagging(KwargCtx(), 1)
kwargtest(x; y = 1) = x + y
@test overdub(ctx, _y -> kwargtest(3; y = _y), tag(2, ctx)) === 5

println("done (took ", time() - before_time, " seconds)")

#############################################################################################

print("   running RecurTagCtx test...")

before_time = time()

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

println("done (took ", time() - before_time, " seconds)")

#############################################################################################

print("   running CrazyPropCtx test...")

before_time = time()

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
function Cassette.overdub(ctx::CrazyPropCtx, ::typeof(*), x, y)
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
function Cassette.overdub(ctx::CrazyPropCtx, ::typeof(+), x, y)
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

println("done (took ", time() - before_time, " seconds)")

#############################################################################################

print("   running DiffCtx test...")

before_time = time()

@context DiffCtx

const DiffCtxWithTag{T} = DiffCtx{Nothing,T}

Cassette.metadatatype(::Type{<:DiffCtx}, ::Type{T}) where {T<:Real} = T

tangent(x, context) = hasmetadata(x, context) ? metadata(x, context) : zero(untag(x, context))

function D(f, x)
    ctx = enabletagging(DiffCtx(), f)
    result = overdub(ctx, f, tag(x, ctx, oftype(x, 1.0)))
    return tangent(result, ctx)
end

function Cassette.overdub(ctx::DiffCtxWithTag{T}, ::typeof(sin), x::Tagged{T,<:Real}) where {T}
    vx, dx = untag(x, ctx), tangent(x, ctx)
    return tag(sin(vx), ctx, cos(vx) * dx)
end

function Cassette.overdub(ctx::DiffCtxWithTag{T}, ::typeof(cos), x::Tagged{T,<:Real}) where {T}
    vx, dx = untag(x, ctx), tangent(x, ctx)
    return tag(cos(vx), ctx, -sin(vx) * dx)
end

function Cassette.overdub(ctx::DiffCtxWithTag{T}, ::typeof(*), x::Tagged{T,<:Real}, y::Tagged{T,<:Real}) where {T}
    vx, dx = untag(x, ctx), tangent(x, ctx)
    vy, dy = untag(y, ctx), tangent(y, ctx)
    return tag(vx * vy, ctx, vy * dx + vx * dy)
end

function Cassette.overdub(ctx::DiffCtxWithTag{T}, ::typeof(*), x::Tagged{T,<:Real}, y::Real) where {T}
    vx, dx = untag(x, ctx), tangent(x, ctx)
    return tag(vx * y, ctx, y * dx)
end

function Cassette.overdub(ctx::DiffCtxWithTag{T}, ::typeof(*), x::Real, y::Tagged{T,<:Real}) where {T}
    vy, dy = untag(y, ctx), tangent(y, ctx)
    return tag(x * vy, ctx, x * dy)
end

function Cassette.overdub(ctx::DiffCtxWithTag{T}, ::typeof(+), x::Tagged{T,<:Real}, y::Tagged{T,<:Real}) where {T}
    vx, dx = untag(x, ctx), tangent(x, ctx)
    vy, dy = untag(y, ctx), tangent(y, ctx)
    return tag(vx + vy, ctx, dx + dy)
end

function Cassette.overdub(ctx::DiffCtxWithTag{T}, ::typeof(+), x::Tagged{T,<:Real}, y::Real) where {T}
    vx, dx = untag(x, ctx), tangent(x, ctx)
    return tag(vx + y, ctx, dx)
end

function Cassette.overdub(ctx::DiffCtxWithTag{T}, ::typeof(+), x::Real, y::Tagged{T,<:Real}) where {T}
    vy, dy = untag(y, ctx), tangent(y, ctx)
    return tag(x + vy, ctx, dy)
end

Cassette.overdub(ctx::DiffCtx, ::typeof(sin), x::Real) = sin(x)
Cassette.overdub(ctx::DiffCtx, ::typeof(cos), x::Real) = cos(x)
Cassette.overdub(ctx::DiffCtx, ::typeof(*), x::Real, y::Real) = x * y
Cassette.overdub(ctx::DiffCtx, ::typeof(+), x::Real, y::Real) = x + y
Cassette.overdub(ctx::DiffCtx, ::typeof(*), x, y, z) = Cassette.overdub(ctx, *, Cassette.overdub(ctx, *, x, y), z)
Cassette.overdub(ctx::DiffCtx, ::typeof(+), x, y, z) = Cassette.overdub(ctx, +, Cassette.overdub(ctx, +, x, y), z)

@test D(sin, 1) === cos(1)
@test D(x -> D(sin, x), 1) === -sin(1)
@test D(x -> sin(x) * cos(x), 1) === cos(1)^2 - sin(1)^2
@test D(x -> x * D(y -> x * y, 1), 2) === 4
@test D(x -> x * D(y -> x * y, 2), 1) === 2
@test D(x -> x  * D(y -> 5*x*y, 3), 2) === 20
@test D(x -> x * foo_bar_identity(x), 1) === 2.0

x = rand()
@test D(x -> (x + 2) * (3 + x), x) === 2x + 5
@test D(x -> CrazyPropModule.crazy_sum_mul([x], [x]), x) === (x + x)
@test D(x -> CrazyPropModule.crazy_sum_mul([x, 2], [3, x]), x) === 2x + 5

println("done (took ", time() - before_time, " seconds)")

#############################################################################################

print("   running ArrayIndexCtx test...")

before_time = time()

@context ArrayIndexCtx

matrixliteral(x) = Int[x x; x x]

Cassette.metadatatype(::Type{<:ArrayIndexCtx}, ::Type{Int}) = String

ctx = enabletagging(ArrayIndexCtx(), matrixliteral)
result = overdub(ctx, matrixliteral, tag(1, ctx, "hi"))

@test untag(result, ctx) == matrixliteral(1)
@test metameta(result, ctx) == fill(Cassette.Meta("hi", Cassette.NoMetaMeta()), 2, 2)

println("done (took ", time() - before_time, " seconds)")
