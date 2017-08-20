module ContextTests

using Base.Test
using Cassette

@test Cassette.iscontextdispatch(:(x:::C))
@test Cassette.iscontextdispatch(:(x::T:C))
@test Cassette.iscontextdispatch(:((x::T):C))
@test !Cassette.iscontextdispatch(:(x::T))
@test !Cassette.iscontextdispatch(:(x::T::S))
@test !Cassette.iscontextdispatch(:((x::T)::C))
@test !Cassette.iscontextdispatch(:x)
@test !Cassette.iscontextdispatch(:(a + b))

end # module ContextTests
