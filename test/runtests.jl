using Cassette
using Base.Test
using BenchmarkTools

########################
# temporary smoke test #
########################

function rosenbrock(x)
    a = one(eltype(x))
    b = 100 * a
    result = zero(eltype(x))
    for i in 1:length(x)-1
        result += (a - x[i])^2 + b*(x[i+1] - x[i]^2)^2
    end
    return result
end

# test ValueGenre (correctness) #
#-------------------------------#

f = Cassette.Trace{Cassette.ValueGenre}(rosenbrock)

x = rand(1000)

xv = Cassette.Note{Cassette.ValueGenre}.(x, nothing)
yv = f(xv)
@test isa(yv, Cassette.Note)
@test Cassette.value(yv) == rosenbrock(x)
t = Cassette.Tape(yv)

x = rand(1000)
foreach(Cassette.value!, xv, x)
Cassette.replay!(t)
@test Cassette.value(yv) == rosenbrock(x)

# test VoidGenre (performance) #
#------------------------------#

f = Cassette.Trace{Cassette.VoidGenre}(rosenbrock)

x = rand(1000)

trial1 = @benchmark $f($x) evals=10 samples=1000
trial2 = @benchmark rosenbrock($x) evals=10 samples=1000

@test BenchmarkTools.allocs(trial1) == BenchmarkTools.allocs(trial2)
@test BenchmarkTools.memory(trial1) == BenchmarkTools.memory(trial2)
@test isinvariant(judge(minimum(trial1), minimum(trial2); time_tolerance=0.03))

###################
# known segfaults #
###################

# Cassette.debug_trace(eltype, DataType)
