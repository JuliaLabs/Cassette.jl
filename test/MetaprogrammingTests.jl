module MetaprogammingTests

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

@test Cassette.isfuncdef(:(x:::C))
@test Cassette.isfuncdef(:(x::T:C))
@test Cassette.isfuncdef(:((x::T):C))
@test !Cassette.isfuncdef(:(x::T))
@test !Cassette.isfuncdef(:(x::T::S))
@test !Cassette.isfuncdef(:((x::T)::C))
@test !Cassette.isfuncdef(:x)
@test !Cassette.isfuncdef(:(a + b))


end # module MetaprogammingTests
