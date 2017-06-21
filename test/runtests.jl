using Cassette
using Base.Test

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

f = Cassette.Trace{Cassette.ValueGenre}(rosenbrock)

x = rand(1000)

xv = Cassette.ValueNote{Cassette.ValueGenre}.(x)
yv = f(xv)
@test isa(yv, Cassette.ValueNote)
@test Cassette.value(yv) == rosenbrock(x)
t = Cassette.Tape(yv)

x = rand(1000)
foreach(Cassette.value!, xv, x)
Cassette.replay!(t)
@test Cassette.value(yv) == rosenbrock(x)
