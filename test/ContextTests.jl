module ContextTests

using Base.Test
using Cassette

@test Cassette.is_ctx_dispatch(:(@ctx(x,C)))
@test Cassette.is_ctx_dispatch(:(@ctx(x::T,C)))
@test Cassette.is_ctx_dispatch(:(@ctx(::T,C)))
@test Cassette.is_ctx_dispatch(:(@ctx(C)))
@test !Cassette.is_ctx_dispatch(:(x::T))
@test !Cassette.is_ctx_dispatch(:(x::T::S))
@test !Cassette.is_ctx_dispatch(:(::T))
@test !Cassette.is_ctx_dispatch(:(::T::S))

@test Cassette.is_non_ctx_dispatch(:(x::T))
@test Cassette.is_non_ctx_dispatch(:(x::T::S))
@test Cassette.is_non_ctx_dispatch(:(::T))
@test Cassette.is_non_ctx_dispatch(:(::T::S))
@test !Cassette.is_non_ctx_dispatch(:(@ctx(x,C)))
@test !Cassette.is_non_ctx_dispatch(:(@ctx(x::T,C)))
@test !Cassette.is_non_ctx_dispatch(:(@ctx(::T,C)))
@test !Cassette.is_non_ctx_dispatch(:(@ctx(C)))

@test Cassette.parse_ctx_dispatch(:(@ctx(x,C))) === (:x, :Any, :C)
@test Cassette.parse_ctx_dispatch(:(@ctx(x::T,C))) === (:x, :T, :C)
@test Cassette.parse_ctx_dispatch(:(@ctx(::T,C))) === (nothing, :T, :C)
@test Cassette.parse_ctx_dispatch(:(@ctx(C))) === (nothing, :Any, :C)

end # module ContextTests
