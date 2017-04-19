using Cassette
using Base.Test

# write your own tests here
@test 1 == 2

@barrier bob(x) = x > 1 ? sin(x) : cos(x)
