#############
# Promotion #
#############

@inline promote_genre(a, b) = promote_genre(genre(a), genre(b))
@inline promote_genre(a, b, c) = promote_genre(promote_genre(a, b), genre(c))
@inline promote_genre(a, b, c, d) = promote_genre(promote_genre(a, b, c), genre(d))
@inline promote_genre(a, b, c, d, e) = promote_genre(promote_genre(a, b, c, d), genre(e))
@inline promote_genre(a, b, c, d, e, args...) = promote_genre(promote_genre(a, b, c, d, e), args...)
@inline promote_genre(a::AbstractGenre, b::AbstractGenre) = error("promote_genre not defined between $(a) and $(b)")

##############
# ValueGenre #
##############

struct ValueGenre <: AbstractGenre end

@inline node_eltype(::ValueGenre, value) = RealNode{ValueGenre,eltype(value),Void}
@inline node_cache(::ValueGenre, value) = nothing

@inline Base.similar(::Type{ValueGenre}) = ValueGenre()
@inline Base.similar(::ValueGenre) = ValueGenre()

@inline promote_genre(a::ValueGenre, b::ValueGenre) = a
