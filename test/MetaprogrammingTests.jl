module MetaprogammingTests

using Base.Test
using Cassette

@test Cassette.isfuncdef(:(x:::C))
@test Cassette.isfuncdef(:(x::T:C))
@test Cassette.isfuncdef(:((x::T):C))
@test !Cassette.isfuncdef(:(x::T))
@test !Cassette.isfuncdef(:(x::T::S))
@test !Cassette.isfuncdef(:((x::T)::C))
@test !Cassette.isfuncdef(:x)
@test !Cassette.isfuncdef(:(a + b))

end # module MetaprogammingTests
