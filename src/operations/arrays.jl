####################################
# External `<:AbstractArray` Types #
####################################

const ARRAY_TYPES = [:AbstractArray, :AbstractVector, :AbstractMatrix, :Array,
                     :Vector, :Matrix, :(StaticArrays.StaticArray),
                     :(StaticArrays.FieldVector), :(StaticArrays.MArray),
                     :(StaticArrays.SArray), :(StaticArrays.SUnitRange),
                     :(StaticArrays.SizedArray)]

###########################
# AbstractArray Interface #
###########################

Base.getindex(n::ArrayNote, i...) = @intercept(getindex)(n, i...)

Base.setindex!(n::ArrayNote, i...) = error("ArrayNotes do not support setindex!; load RealNotes into a normal Array instead")

Base.size(n::ArrayNote) = size(untrack(n))

Base.length(n::ArrayNote) = length(untrack(n))

Base.indices(n::ArrayNote) = indices(untrack(n))

Base.start(n::ArrayNote) = start(n)

Base.next(n::ArrayNote, state) = next(n, state)

Base.done(n::ArrayNote, state) = done(n, state)

Base.IndexStyle(::Type{T}) where {T<:ArrayNote} = IndexStyle(valtype(T))
Base.IndexStyle(n::ArrayNote) = IndexStyle(untrack(n))

Base.similar(n::ArrayNote) = similar(untrack(n), eltype(n))
Base.similar(n::ArrayNote, ::Type{T}) where {T} = similar(untrack(n), T)
Base.similar(n::ArrayNote, ::Type{T}, dims) where {T} = similar(untrack(n), T, dims)

###########################
# Miscellaneous Functions #
###########################

Base.copy(n::ArrayNote) = @intercept(copy)(n)

Base.ones(n::ArrayNote) = @intercept(ones)(n)
Base.ones(n::ArrayNote, ::Type{T}) where {T} = @intercept(ones)(n, T)
Base.ones(n::ArrayNote, ::Type{T}, dims::Tuple) where {T} = @intercept(ones)(n, T, dims)
Base.ones(n::ArrayNote, ::Type{T}, dims...) where {T} = @intercept(ones)(n, T, dims...)

Base.zeros(n::ArrayNote) = @intercept(zeros)(n)
Base.zeros(n::ArrayNote, ::Type{T}) where {T} = @intercept(zeros)(n, T)
Base.zeros(n::ArrayNote, ::Type{T}, dims::Tuple) where {T} = @intercept(zeros)(n, T, dims)
Base.zeros(n::ArrayNote, ::Type{T}, dims...) where {T} = @intercept(zeros)(n, T, dims...)

Base.reshape(n::ArrayNote, dims::Type{Val{N}}) where {N} = @intercept(reshape)(n, dims)
Base.reshape(n::ArrayNote, dims::Tuple{Vararg{Int,N}}) where {N} = @intercept(reshape)(n, dims)
Base.reshape(n::ArrayNote, dims::Int64...) = @intercept(reshape)(n, dims...)
Base.reshape(n::ArrayNote, dims::AbstractUnitRange...) = @intercept(reshape)(n, dims...)
Base.reshape(n::ArrayNote, dims::Union{AbstractUnitRange,Int64}...) = @intercept(reshape)(n, dims...)

########
# Math #
########

const UNARY_ARRAY_MATH = [:sum, :prod, :cumsum, :cumprod, :normalize, :norm, :vecnorm,
                          :cond, :rank, :trace, :det, :logdet, :logabsdet, :inv, :pinv,
                          :nullspace, :expm, :logm, :sqrtm, :istriu, :istril, :isposdef,
                          :issymmetric, :isdiag, :ishermitian, :transpose, :ctranspose]

const BINARY_ARRAY_MATH = [:A_ldiv_Bc, :A_ldiv_Bt, :A_mul_Bc, :A_mul_Bt, :A_rdiv_Bc,
                           :A_rdiv_Bt, :Ac_ldiv_B, :Ac_ldiv_Bc, :Ac_mul_B, :Ac_mul_Bc,
                           :Ac_rdiv_B, :Ac_rdiv_Bc, :At_ldiv_B, :At_ldiv_Bt, :At_mul_B,
                           :At_mul_Bt, :At_rdiv_B, :At_rdiv_Bt, :kron, :dot, :cross,
                           :vecdot, :+, :-, :*, :/, :\, :^]

# Not all of these methods will be valid, but it won't matter
# because the underlying function will throw the appropriate
# MethodError if needed.
for f in BINARY_ARRAY_MATH
    @eval @inline Base.$(f)(a::ArrayNote, b::ArrayNote) = @intercept($(f))(a, b)
    @eval @inline Base.$(f)(a::ArrayNote, b::RealNote) = @intercept($(f))(a, b)
    @eval @inline Base.$(f)(a::RealNote, b::ArrayNote) = @intercept($(f))(a, b)
    for T in vcat(ARRAY_TYPES, REAL_TYPES)
        @eval @inline Base.$(f)(a::ArrayNote, b::$T) = @intercept($(f))(a, b)
        @eval @inline Base.$(f)(a::$T, b::ArrayNote) = @intercept($(f))(a, b)
    end
end

for f in UNARY_ARRAY_MATH
    @eval @inline Base.$(f)(n::ArrayNote) = @intercept($(f))(n)
end

Base.sum(n::ArrayNote, dims) = @intercept(sum)(n, dims)

Base.prod(n::ArrayNote, dims) = @intercept(prod)(n, dims)

Base.cumsum(n::ArrayNote, dim) = @intercept(cumsum)(n, dim)

Base.cumprod(n::ArrayNote, dim) = @intercept(cumprod)(n, dim)

Base.normalize(n::ArrayNote, p) = @intercept(normalize)(n, p)

Base.norm(n::ArrayNote, p) = @intercept(norm)(n, p)

Base.vecnorm(n::ArrayNote, p) = @intercept(vecnorm)(n, p)

Base.cond(n::ArrayNote, p) = @intercept(cond)(n, p)

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

@inline Base.Broadcast.broadcast_c(f, ::Type{ArrayNote}, args...) = @intercept(broadcast)(f, args...)
