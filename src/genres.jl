##############
# ValueGenre #
##############

struct ValueGenre <: AbstractGenre end

node_eltype(::ValueGenre, value) = RealNode{ValueGenre,eltype(value),Void}
node_cache(::ValueGenre, value) = nothing

Base.similar(::Type{ValueGenre}) = ValueGenre()
Base.similar(::ValueGenre) = ValueGenre()
