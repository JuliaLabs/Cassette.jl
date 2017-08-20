module ContextTests

using Base.Test
using Cassette

@test Cassette.is_ctx_dispatch(:(x:::C))
@test Cassette.is_ctx_dispatch(:(x::T:C))
@test Cassette.is_ctx_dispatch(:(:::C))
@test Cassette.is_ctx_dispatch(:(::T:C))
@test !Cassette.is_ctx_dispatch(:(x::T))
@test !Cassette.is_ctx_dispatch(:(x::T::S))
@test !Cassette.is_ctx_dispatch(:(::T))
@test !Cassette.is_ctx_dispatch(:(::T::S))
@test !Cassette.is_ctx_dispatch(:x)
@test !Cassette.is_ctx_dispatch(:(a + b))

@test Cassette.parse_ctx_dispatch(:(x:::C)) === (:x, :Any, :C)
@test Cassette.parse_ctx_dispatch(:(x::T:C)) === (:x, :T, :C)
@test Cassette.parse_ctx_dispatch(:(:::C)) === (nothing, :Any, :C)
@test Cassette.parse_ctx_dispatch(:(::T:C)) === (nothing, :T, :C)

@test Cassette.is_non_ctx_dispatch(:(x::T))
@test Cassette.is_non_ctx_dispatch(:(x::T::S))
@test Cassette.is_non_ctx_dispatch(:(::T))
@test Cassette.is_non_ctx_dispatch(:(::T::S))
@test !Cassette.is_non_ctx_dispatch(:x)
@test !Cassette.is_non_ctx_dispatch(:(a + b))

end # module ContextTests
