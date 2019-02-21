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
Cassette.posthook(ctx::HookCtx, out, f, args...) = push!(ctx.metadata[2], (out, f, args))

pres, posts = Any[], Any[]
ctx = HookCtx(metadata=(pres, posts))
x1, x2, x3 = rand(Int), rand(Int), rand(Int)

@overdub(ctx, x1 + x2 * x3)
@test pres == [(*, (x2, x3)),
               (Base.mul_int, (x2, x3)),
               (+, (x1, x2*x3)),
               (Base.add_int, (x1, x2*x3))]
@test posts == [(Base.mul_int(x2, x3), Base.mul_int, (x2, x3)),
                (*(x2, x3), *, (x2, x3)),
                (Base.add_int(x1, x2*x3), Base.add_int, (x1, x2*x3)),
                (+(x1, x2*x3), +, (x1, x2*x3))]
empty!(pres)
empty!(posts)

Cassette.overdub(::HookCtx, ::typeof(+), args...) = +(args...)

@overdub(ctx, x1 + x2 * x3)
@test pres == [(*, (x2, x3)),
               (Base.mul_int, (x2, x3)),
               (+, (x1, x2*x3))]
@test posts == [(Base.mul_int(x2, x3), Base.mul_int, (x2, x3)),
                (*(x2, x3), *, (x2, x3)),
                (+(x1, x2*x3), +, (x1, x2*x3))]
empty!(pres)
empty!(posts)

@overdub(ctx, Core._apply(+, (x1, x2), (x2 * x3, x3)))
@test pres == [(tuple, (x1, x2)),
               (*, (x2, x3)),
               (Base.mul_int, (x2, x3)),
               (tuple, (x2*x3, x3)),
               (+, (x1, x2, x2*x3, x3))]
@test posts == [((x1, x2), tuple, (x1, x2)),
                (Base.mul_int(x2, x3), Base.mul_int, (x2, x3)),
                (*(x2, x3), *, (x2, x3)),
                ((x2*x3, x3), tuple, (x2*x3, x3)),
                (+(x1, x2, x2*x3, x3), +, (x1, x2, x2*x3, x3))]

println("done (took ", time() - before_time, " seconds)")

#############################################################################################

print("   running SinCtx test...")

before_time = time()

@context SinCtx
x = rand()
sin_plus_cos(x) = sin(x) + cos(x)
@test @overdub(SinCtx(), sin_plus_cos(x)) === sin_plus_cos(x)
Cassette.overdub(::SinCtx, ::typeof(sin), x) = cos(x)
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
mypass = @pass (ctx, ref) -> begin
    # TODO: check !in(sig, sig_collection) first to mimic a caching technique that will be used in practice
    push!(sig_collection, ref.signature)
    return ref.code_info
end
@overdub(PassCtx(pass=mypass), sum(rand(3)))
@test !isempty(sig_collection) && all(T -> T <: Tuple, sig_collection)

println("done (took ", time() - before_time, " seconds)")

#############################################################################################

print("   running PassFallbackCtx test...")

before_time = time()

@context PassFallbackCtx
fallbackpass = @pass (ctx, ref) -> begin
    if ref.signature <: Tuple{typeof(sin),Any}
        return :(cos($(Cassette.OVERDUB_ARGUMENTS_NAME)[2]))
    end
    return ref.code_info
end
x = rand(30)
sin_kernel(i) = i > 0.5 ? sin(i) : i
y = @inferred(overdub(PassFallbackCtx(pass=fallbackpass), sum, sin_kernel, x))
@test sum(i -> i > 0.5 ? cos(i) : i, x) === y

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

function Cassette.overdub(ctx::TraceCtx, args...)
    subtrace = Any[]
    push!(ctx.metadata, args => subtrace)
    if canrecurse(ctx, args...)
        newctx = similarcontext(ctx, metadata = subtrace)
        return recurse(newctx, args...)
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
kwargtest(foobar; foo = 1, bar = 2) = nothing

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
@inferred(overdub(InferCtx(), () -> kwargtest(42; foo = 1, bar = 2)))

println("done (took ", time() - before_time, " seconds)")

#############################################################################################

print("   running InModuleCtx test...")

before_time = time()

module DefineStuffInModule
    using Cassette
    Cassette.@context InModuleCtx
    x = 0
    incrpass = Cassette.@pass (ctx, ref) -> (global x += 1; ref.code_info)
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

#############################################################################################

# taken from https://stackoverflow.com/questions/52050262/how-to-do-memoization-or-memoisation-in-julia-1-0/52062639#52062639

print("   running MemoizeCtx test...")

before_time = time()

fib(x) = x < 3 ? 1 : fib(x - 2) + fib(x - 1)
fibtest(n) = fib(2 * n) + n

@context MemoizeCtx

function Cassette.overdub(ctx::MemoizeCtx, ::typeof(fib), x)
    result = get(ctx.metadata, x, 0)
    result === 0 && return recurse(ctx, fib, x)
    return result
end

Cassette.posthook(ctx::MemoizeCtx, fibx, ::typeof(fib), x) = (ctx.metadata[x] = fibx)

ctx = MemoizeCtx(metadata = Dict{Int,Int}())
n = 10
result = Cassette.overdub(ctx, fibtest, n)

@test result == fibtest(n)
@test length(ctx.metadata) == 2 * n
@test all(fib(k) == v for (k, v) in ctx.metadata)

println("done (took ", time() - before_time, " seconds)")

#############################################################################################

# ref https://github.com/jrevels/Cassette.jl/issues/73

if VERSION >= v"1.1-"
    print("   running NoOpCtx test...")

    before_time = time()

    @context NoOpCtx

    function loop73(x, n)
        r = x / x
        while n > 0
            r *= sin(x)
            n -= 1
        end
        return r
    end

    f73(x, n) = overdub(NoOpCtx(), loop73, x, n)
    ff73(x, n) = overdub(NoOpCtx(), f73, x, n)
    fff73(x, n) = overdub(NoOpCtx(), ff73, x, n)

    f73(2, 50) # warm up
    ff73(2, 50) # warm up
    fff73(2, 50) # warm up

    @test @allocated(f73(2, 50)) == 0
    @test @allocated(ff73(2, 50)) == 0
    @test_broken @allocated(fff73(2, 50)) == 0

    println("done (took ", time() - before_time, " seconds)")
end

#############################################################################################

print("   running DisableHooksCtx test...")

before_time = time()

@context DisableHooksCtx
Cassette.prehook(ctx::DisableHooksCtx, args...) = ctx.metadata[1] += 1
Cassette.posthook(ctx::DisableHooksCtx, args...) = ctx.metadata[2] += 1
ctx = disablehooks(DisableHooksCtx(metadata = [0, 0]))
@test overdub(ctx, sin, 1.0) == sin(1.0)
@test all(ctx.metadata .== 0)
@test overdub(ctx, overdub, ctx, sin, 1.0) == sin(1.0)
@test all(ctx.metadata .== 0)

println("done (took ", time() - before_time, " seconds)")

#############################################################################################

print("   running SliceCtx test...")

before_time = time()

using Core: CodeInfo, SlotNumber, SSAValue

@context SliceCtx

function Cassette.overdub(ctx::SliceCtx, f, callback, args...)
    if canrecurse(ctx, f, args...)
        _ctx = similarcontext(ctx, metadata = callback)
        return recurse(_ctx, f, args...) # return result, callback
    else
        return fallback(ctx, f, args...), callback
    end
end

const global_test_cache = Any[]

push_to_global_test_cache!(x) = push!(global_test_cache, x)

function Cassette.overdub(ctx::SliceCtx, ::typeof(push_to_global_test_cache!), callback, x)
    return nothing, () -> (callback(); push_to_global_test_cache!(x))
end

function sliceprintln(::Type{<:SliceCtx}, reflection::Cassette.Reflection)
    ir = reflection.code_info
    callbackslotname = gensym("callback")
    push!(ir.slotnames, callbackslotname)
    push!(ir.slotflags, 0x00)
    callbackslot = SlotNumber(length(ir.slotnames))
    getmetadata = Expr(:call, Expr(:nooverdub, GlobalRef(Core, :getfield)), Expr(:contextslot), QuoteNode(:metadata))

    # insert the initial `callbackslot` assignment into the IR.
    Cassette.insert_statements!(ir.code, ir.codelocs,
                                (stmt, i) -> i == 1 ? 2 : nothing,
                                (stmt, i) -> [Expr(:(=), callbackslot, getmetadata), stmt])

    # replace all calls of the form `f(args...)` with `f(callback, args...)`, taking care to
    # properly handle Core._apply calls and destructure the returned `(result, callback)`
    # into the appropriate statements
    Cassette.insert_statements!(ir.code, ir.codelocs,
                                (stmt, i) -> begin
                                    i > 1 || return nothing # don't slice the callback assignment
                                    stmt = Base.Meta.isexpr(stmt, :(=)) ? stmt.args[2] : stmt
                                    if Base.Meta.isexpr(stmt, :call)
                                        isapply = Cassette.is_ir_element(stmt.args[1], GlobalRef(Core, :_apply), ir.code)
                                        return 3 + isapply
                                    end
                                    return nothing
                                end,
                                (stmt, i) -> begin
                                    items = Any[]
                                    callstmt = Base.Meta.isexpr(stmt, :(=)) ? stmt.args[2] : stmt
                                    callssa = SSAValue(i)
                                    if Cassette.is_ir_element(callstmt.args[1], GlobalRef(Core, :_apply), ir.code)
                                        push!(items, Expr(:call, Expr(:nooverdub, GlobalRef(Core, :tuple)), callbackslot))
                                        push!(items, Expr(:call, callstmt.args[1], callstmt.args[2], SSAValue(i), callstmt.args[3:end]...))
                                        callssa = SSAValue(i + 1)
                                    else
                                        push!(items, Expr(:call, callstmt.args[1], callbackslot, callstmt.args[2:end]...))
                                    end
                                    push!(items, Expr(:(=), callbackslot, Expr(:call, Expr(:nooverdub, GlobalRef(Core, :getfield)), callssa, 2)))
                                    result = Expr(:call, Expr(:nooverdub, GlobalRef(Core, :getfield)), callssa, 1)
                                    if Base.Meta.isexpr(stmt, :(=))
                                        result = Expr(:(=), stmt.args[1], result)
                                    end
                                    push!(items, result)
                                    return items
                                end)

    # replace return statements of the form `return x` with `return (x, callback)`
    Cassette.insert_statements!(ir.code, ir.codelocs,
                                  (stmt, i) -> Base.Meta.isexpr(stmt, :return) ? 2 : nothing,
                                  (stmt, i) -> begin
                                      return [
                                          Expr(:call, Expr(:nooverdub, GlobalRef(Core, :tuple)), stmt.args[1], callbackslot)
                                          Expr(:return, SSAValue(i))
                                      ]
                                  end)
    return ir
end

const sliceprintlnpass = Cassette.@pass sliceprintln

a = rand(3)
b = rand(3)

function add(a, b)
    push_to_global_test_cache!(a)
    push_to_global_test_cache!(b)
    c = a + b
    push_to_global_test_cache!(c)
    return c
end

ctx = SliceCtx(pass=sliceprintlnpass, metadata = () -> nothing)

result, callback = Cassette.recurse(ctx, add, a, b)

@test result == a + b
@test isempty(global_test_cache)
callback()
@test global_test_cache == [a, b, result]

println("done (took ", time() - before_time, " seconds)")
