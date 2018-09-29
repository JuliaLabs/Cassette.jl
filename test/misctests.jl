#############################################################################################

print("   running RosCtx test...")

before_time = time()

@context RosCtx

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
@inferred(overdub(RosCtx(), rosenbrock, x))

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

println("done (took ", time() - before_time, " seconds)")

#############################################################################################

print("   running HookCtx test...")

before_time = time()

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

println("done (took ", time() - before_time, " seconds)")

#############################################################################################

print("   running SinCtx test...")

before_time = time()

@context SinCtx
x = rand()
sin_plus_cos(x) = sin(x) + cos(x)
@test @overdub(SinCtx(), sin_plus_cos(x)) === sin_plus_cos(x)
Cassette.execute(::SinCtx, ::typeof(sin), x) = cos(x)
@test @overdub(SinCtx(), sin_plus_cos(x)) === (2 * cos(x))

println("done (took ", time() - before_time, " seconds)")

#############################################################################################

print("   running FoldCtx test...")

before_time = time()

@context FoldCtx
x = 2
foldmul(x, args...) = Core._apply(Base.afoldl, (*, x), args...)
@test @overdub(FoldCtx(), foldmul(x)) === foldmul(x)

println("done (took ", time() - before_time, " seconds)")

#############################################################################################

print("   running CountCtx test...")

before_time = time()

@context CountCtx
count1 = Ref(0)
Cassette.prehook(ctx::CountCtx, f, args::Number...) = (ctx.metadata[] += 1)
@overdub(CountCtx(metadata=count1), sin(1))
Cassette.prehook(ctx::CountCtx, f, args::Number...) = (ctx.metadata[] += 2)
count2 = Ref(0)
@overdub(CountCtx(metadata=count2), sin(1))
@test (2 * count1[]) === count2[]

println("done (took ", time() - before_time, " seconds)")

#############################################################################################

print("   running CountCtx2 test...")

before_time = time()

@context CountCtx2
mutable struct Count{T}
    count::Int
end
function Cassette.prehook(ctx::CountCtx2{Count{T}}, f, arg::T, args::T...) where {T}
    ctx.metadata.count += 1
end
mapstr(x) = map(string, x)
c = Count{Union{String,Int}}(0)
@test @overdub(CountCtx2(metadata=c), mapstr(1:10)) == mapstr(1:10)
@test c.count > 1000

println("done (took ", time() - before_time, " seconds)")

#############################################################################################

print("   running SqrCtx test...")

before_time = time()

@context SqrCtx
square_closure(x) = (y -> y * x)(x)
x = rand()
@test square_closure(x) == @overdub(SqrCtx(), square_closure(x))

println("done (took ", time() - before_time, " seconds)")

#############################################################################################

print("   running CompCtx test...")

before_time = time()

@context CompCtx
comprehension1(x) = [i for i in x]
comprehension2(f, x, y) = [f(x, i) for i in y]
f, x, y = hypot, rand(), rand(2)
@test comprehension1(x) == @overdub(CompCtx(), comprehension1(x))
@test comprehension1(y) == @overdub(CompCtx(), comprehension1(y))
@test comprehension2(f, x, y) == @overdub(CompCtx(), comprehension2(f, x, y))

println("done (took ", time() - before_time, " seconds)")

#############################################################################################

print("   running PassCtx test...")

before_time = time()

@context PassCtx
sig_collection = DataType[]
mypass = @pass (ctx, sig, cinfo) -> begin
    # TODO: check !in(sig, sig_collection) first to mimic a caching technique that will be used in practice
    push!(sig_collection, sig)
    return cinfo
end
@overdub(PassCtx(pass=mypass), sum(rand(3)))
@test !isempty(sig_collection) && all(T -> T <: Tuple, sig_collection)

println("done (took ", time() - before_time, " seconds)")

#############################################################################################

print("   running WorldCtx test...")

before_time = time()

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

println("done (took ", time() - before_time, " seconds)")

#############################################################################################

print("   running TraceCtx test...")

before_time = time()

x, y, z = rand(3)
trtest(x, y, z) = x*y + y*z
trkwtest(x; _y = 1.0, _z = 2.0) = trtest(x, _y, _z)

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
@test @overdub(TraceCtx(metadata = trace), trtest(x, y, z)) == trtest(x, y, z)
@test trace == Any[
    (trtest,x,y,z) => Any[
        (*,x,y) => Any[(Base.mul_float,x,y)=>Any[]]
        (*,y,z) => Any[(Base.mul_float,y,z)=>Any[]]
        (+,x*y,y*z) => Any[(Base.add_float,x*y,y*z)=>Any[]]
    ]
]

# jrevels/Cassette.jl#48
@context HookTraceCtx

mutable struct HookTrace
    current::Vector{Any}
    stack::Vector{Any}
    HookTrace() = new(Any[], Any[])
end

tracekw = Any[]
@overdub(TraceCtx(metadata = tracekw), trkwtest(x, _y = y, _z = z)) == trtest(x, y, z)
subtracekw = first(Iterators.filter(t -> t[1] === (Core.kwfunc(trkwtest), (_y = y, _z = z), trkwtest, x), tracekw))[2]
@test subtracekw == trace

function enter!(t::HookTrace, args...)
    pair = args => Any[]
    push!(t.current, pair)
    push!(t.stack, t.current)
    t.current = pair.second
    return nothing
end

function exit!(t::HookTrace)
    t.current = pop!(t.stack)
    return nothing
end

Cassette.prehook(ctx::HookTraceCtx, args...) = enter!(ctx.metadata, args...)
Cassette.posthook(ctx::HookTraceCtx, args...) = exit!(ctx.metadata)

htrace = HookTrace()
@overdub(HookTraceCtx(metadata = htrace), trtest(x, y, z))
@test htrace.current == trace

println("done (took ", time() - before_time, " seconds)")

#############################################################################################

print("   running NestedReflectCtx test...")

before_time = time()

@context NestedReflectCtx
r_pre = Cassette.reflect((typeof(sin), Int))
r_post = Cassette.reflect((typeof(overdub), typeof(NestedReflectCtx()), typeof(sin), Int))
@test isa(r_pre, Cassette.Reflection) && isa(r_post, Cassette.Reflection)
Cassette.overdub_pass!(r_pre, typeof(NestedReflectCtx()))
@test r_pre.code_info.code == r_post.code_info.code

println("done (took ", time() - before_time, " seconds)")

#############################################################################################

print("   running GemvCtx test...")

before_time = time()

# issue #51
@context GemvCtx
α, β = 1.0, 2.0
A, X, Y = rand(Float64, 10, 10), rand(Float64, 10), rand(Float64, 10)
Y_copy = copy(Y)
Y_out = overdub(GemvCtx(), LinearAlgebra.BLAS.gemv!, 'T', α, A, X, β, Y)
Y_copy_out = LinearAlgebra.BLAS.gemv!('T', α, A, X, β, Y_copy)
@test Y_out == Y_copy_out
@test Y_out === Y
@test Y_copy_out === Y_copy

println("done (took ", time() - before_time, " seconds)")

#############################################################################################

print("   running InferCtx test...")

before_time = time()

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
@inferred(overdub(InferCtx(), rand, Float32, 1))
@inferred(overdub(InferCtx(), broadcast, +, rand(1), rand(1)))
@inferred(overdub(InferCtx(), relulayer, rand(Float64, 1, 1), rand(Float32, 1), rand(Float32, 1)))

println("done (took ", time() - before_time, " seconds)")

#############################################################################################

print("   running InModuleCtx test...")

before_time = time()

module DefineStuffInModule
    using Cassette
    Cassette.@context InModuleCtx
    x = 0
    incrpass = Cassette.@pass (ctx, sig, code) -> (global x += 1; code)
    f(x) = Cassette.overdub(InModuleCtx(pass = incrpass), sin, x)
end

@test DefineStuffInModule.f(1) === sin(1)
@test DefineStuffInModule.x > 0

println("done (took ", time() - before_time, " seconds)")

#############################################################################################

print("   running InvokeCtx test...")

before_time = time()

invoked(x::Int) = x + x
invoked(x::Number) = x^2
invoker(x) = invoke(invoked, Tuple{Number}, x)

@context InvokeCtx
Cassette.prehook(ctx::InvokeCtx, f, args...) = push!(ctx.metadata, f)
ctx = InvokeCtx(metadata=Any[])

@test overdub(ctx, invoker, 3) === 9
# This is kind of fragile and may break for unrelated reasons - the main thing
# we're testing here is that we properly trace through the `invoke` call.
@test ctx.metadata == Any[Core.apply_type, Core.invoke, Core.apply_type,
                          Val{2}, Core.apply_type, Base.literal_pow, *,
                          Base.mul_int]

println("done (took ", time() - before_time, " seconds)")
