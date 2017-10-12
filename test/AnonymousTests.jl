module AnonymousTests

using Test
using Cassette

# using `string` here is silly but good enough
@test string(Cassette.translatefield(:(x = d))) == string(:($Cassette.Field($Cassette.Name{:x}(), $Cassette.Immutable(d))))
@test string(Cassette.translatefield(:(x::T = d))) == string(:($Cassette.Field($Cassette.Name{:x}(), $Cassette.Immutable{T}(d))))
@test string(Cassette.translatefield(:(mut(x = d)))) == string(:($Cassette.Field($Cassette.Name{:x}(), $Cassette.Mutable(d))))
@test string(Cassette.translatefield(:(mut(x::T = d)))) == string(:($Cassette.Field($Cassette.Name{:x}(), $Cassette.Mutable{T}(d))))
@test string(Cassette.translatefield(:(mut(x)))) == string(:($Cassette.Field($Cassette.Name{:x}(), $Cassette.Mutable{Any}())))
@test string(Cassette.translatefield(:(mut(x::T)))) == string(:($Cassette.Field($Cassette.Name{:x}(), $Cassette.Mutable{T}())))

end # module AnonymousTests
