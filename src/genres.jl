#################
# AbstractGenre #
#################

abstract type AbstractGenre end

macro defgenre(G)
    return esc(:(struct $G <: $(AbstractGenre) end))
end

#############
# VoidGenre #
#############

@defgenre VoidGenre

##############
# ValueGenre #
##############

@defgenre ValueGenre

#########
# genre #
#########

@inline genre(x) = genre(typeof(x))

@inline genre(::DataType) = VoidGenre()

@inline genre(g::AbstractGenre) = g

#############
# Promotion #
#############

@inline Base.promote_rule(::Type{ValueGenre}, ::Type{VoidGenre}) = ValueGenre

@inline promote_genre(::A, ::B) where {A<:AbstractGenre,B<:AbstractGenre} = promote_type(A, B)()

promote_genre(a::A) where {A} = genre(a)
promote_genre(a::A, b::B) where {A,B} = promote_genre(genre(a), genre(b))
promote_genre(a::A, b::B, c::C) where {A,B,C} = promote_genre(promote_genre(a, b), genre(c))
promote_genre(a::A, b::B, c::C, d::D) where {A,B,C,D} = promote_genre(promote_genre(a, b, c), genre(d))
promote_genre(a::A, b::B, c::C, d::D, e::E) where {A,B,C,D,E} = promote_genre(promote_genre(a, b, c, d), genre(e))
promote_genre(a::A, b::B, c::C, d::D, e::E, others::O...) where {A,B,C,D,E,O} = promote_genre(promote_genre(a, b, c, d, e), others...)
