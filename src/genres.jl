#################
# AbstractGenre #
#################

abstract type AbstractGenre end

macro defgenre(G)
    return esc(:(struct $G <: $(AbstractGenre) end))
end

@inline genre(g::AbstractGenre) = g

#############
# VoidGenre #
#############

@defgenre VoidGenre

##############
# ValueGenre #
##############

@defgenre ValueGenre

#############
# Promotion #
#############
# TODO: Leverage normal promotion rules for this

@inline promote_genre(a::A) where {A} = genre(a)
@inline promote_genre(a::A, b::B) where {A,B} = promote_genre(genre(a), genre(b))
@inline promote_genre(a::A, b::B, c::C) where {A,B,C} = promote_genre(promote_genre(a, b), genre(c))
@inline promote_genre(a::A, b::B, c::C, d::D) where {A,B,C,D} = promote_genre(promote_genre(a, b, c), genre(d))
@inline promote_genre(a::A, b::B, c::C, d::D, e::E) where {A,B,C,D,E} = promote_genre(promote_genre(a, b, c, d), genre(e))
@inline promote_genre(a::A, b::B, c::C, d::D, e::E, others::O...) where {A,B,C,D,E,O} = promote_genre(promote_genre(a, b, c, d, e), others...)

@inline promote_genre(a::AbstractGenre, b::AbstractGenre) = error("promote_genre not defined between $(a) and $(b)")
@inline promote_genre(g::AbstractGenre) = g

@inline promote_genre(a::G, b::G) where {G<:AbstractGenre} = a

@inline promote_genre(a::ValueGenre, b::VoidGenre) = a
@inline promote_genre(a::VoidGenre, b::ValueGenre) = b
