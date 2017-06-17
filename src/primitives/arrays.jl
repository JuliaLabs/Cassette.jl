###########################
# AbstractArray Interface #
###########################

Base.getindex(n::ArrayNote, i...) = Intercept(getindex)(n, i...)

Base.setindex!(n::ArrayNote, i...) = error("ArrayNotes do not support setindex!; load RealNotes into a normal Array instead")

Base.size(n::ArrayNote) = size(value(n))

Base.length(n::ArrayNote) = length(value(n))

Base.indices(n::ArrayNote) = indices(value(n))

Base.start(n::ArrayNote) = start(n)

Base.next(n::ArrayNote, state) = next(n, state)

Base.done(n::ArrayNote, state) = done(n, state)

Base.IndexStyle(::Type{T}) where {T<:ArrayNote} = IndexStyle(valuetype(T))
Base.IndexStyle(n::ArrayNote) = IndexStyle(value(n))

Base.similar(n::ArrayNote) = similar(value(n), eltype(n))
Base.similar(n::ArrayNote, ::Type{T}) where {T} = similar(value(n), T)
Base.similar(n::ArrayNote, ::Type{T}, dims) where {T} = similar(value(n), T, dims)

###########################
# Miscellaneous Functions #
###########################
# Some of these functions are not intercepted; see similar
# note in operations/scalars.jl for an explanation.

Base.copy(n::ArrayNote) = Intercept(copy)(n)

Base.ones(n::ArrayNote) = track(ones(value(n)), genre(n))
Base.ones(n::ArrayNote, ::Type{T}) where {T} = track(ones(value(n), T), genre(n))
Base.ones(n::ArrayNote, ::Type{T}, dims::Tuple) where {T} = track(ones(value(n), T, dims), genre(n))
Base.ones(n::ArrayNote, ::Type{T}, dims...) where {T} = track(ones(value(n), T, dims...), genre(n))

Base.zeros(n::ArrayNote) = track(zeros(value(n)), genre(n))
Base.zeros(n::ArrayNote, ::Type{T}) where {T} = track(zeros(value(n), T), genre(n))
Base.zeros(n::ArrayNote, ::Type{T}, dims::Tuple) where {T} = track(zeros(value(n), T, dims), genre(n))
Base.zeros(n::ArrayNote, ::Type{T}, dims...) where {T} = track(zeros(value(n), T, dims...), genre(n))

Base.reshape(n::ArrayNote, dims::Type{Val{N}}) where {N} = Intercept(reshape)(n, dims)
Base.reshape(n::ArrayNote, dims::Tuple{Vararg{Int,N}}) where {N} = Intercept(reshape)(n, dims)
Base.reshape(n::ArrayNote, dims::Int64...) = Intercept(reshape)(n, dims...)
Base.reshape(n::ArrayNote, dims::AbstractUnitRange...) = Intercept(reshape)(n, dims...)
Base.reshape(n::ArrayNote, dims::Union{AbstractUnitRange,Int64}...) = Intercept(reshape)(n, dims...)

########
# Math #
########

const UNARY_ARRAY_MATH = [:sum, :prod, :cumsum, :cumprod, :normalize, :norm, :vecnorm,
                          :cond, :rank, :trace, :det, :logdet, :logabsdet, :pinv, :nullspace,
                          :expm, :logm, :sqrtm, :istriu, :istril, :isposdef, :issymmetric,
                          :isdiag, :ishermitian, :transpose, :ctranspose] # (:inv was already added in reals.jl)

const BINARY_ARRAY_MATH = [:A_ldiv_Bc, :A_ldiv_Bt, :A_mul_Bc, :A_mul_Bt, :A_rdiv_Bc,
                           :A_rdiv_Bt, :Ac_ldiv_B, :Ac_ldiv_Bc, :Ac_mul_B, :Ac_mul_Bc,
                           :Ac_rdiv_B, :Ac_rdiv_Bc, :At_ldiv_B, :At_ldiv_Bt, :At_mul_B,
                           :At_mul_Bt, :At_rdiv_B, :At_rdiv_Bt, :kron, :dot, :cross,
                           :vecdot] # (:+, :-, :*, :/, :\, :^ were already added in reals.jl)

# Not all of these methods will be valid, but it won't matter
# because the underlying function will throw the appropriate
# MethodError if needed.
for f in BINARY_ARRAY_MATH
    @eval @intercept Base.$(f)(x, y)
end

for f in UNARY_ARRAY_MATH
    @eval @intercept Base.$(f)(x)
end

Base.sum(n::ArrayNote, dims) = Intercept(sum)(n, dims)

Base.prod(n::ArrayNote, dims) = Intercept(prod)(n, dims)

Base.cumsum(n::ArrayNote, dim) = Intercept(cumsum)(n, dim)

Base.cumprod(n::ArrayNote, dim) = Intercept(cumprod)(n, dim)

Base.normalize(n::ArrayNote, p) = Intercept(normalize)(n, p)

Base.norm(n::ArrayNote, p) = Intercept(norm)(n, p)

Base.vecnorm(n::ArrayNote, p) = Intercept(vecnorm)(n, p)

Base.cond(n::ArrayNote, p) = Intercept(cond)(n, p)

#############
# Broadcast #
#############

@inline Base.Broadcast._containertype(::Type{<:ArrayNote}) = ArrayNote

@inline Base.Broadcast.promote_containertype(::Type{ArrayNote}, ::Type{ArrayNote}) = ArrayNote
@inline Base.Broadcast.promote_containertype(::Type{ArrayNote}, _) = ArrayNote
@inline Base.Broadcast.promote_containertype(_, ::Type{ArrayNote}) = ArrayNote

for A in ARRAY_TYPES
    @eval @inline Base.Broadcast.promote_containertype(::Type{ArrayNote}, ::Type{$A}) = ArrayNote
    @eval @inline Base.Broadcast.promote_containertype(::Type{$A}, ::Type{ArrayNote}) = ArrayNote
end

@inline Base.Broadcast.broadcast_c(f, ::Type{ArrayNote}, args...) = Intercept(broadcast)(f, args...)
