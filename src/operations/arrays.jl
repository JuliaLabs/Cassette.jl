###########################
# AbstractArray Interface #
###########################

Base.getindex(n::ArrayNode, i...) = @intercept(getindex)(n, i...)

Base.setindex!(n::ArrayNode, i...) = error("ArrayNodes do not support setindex!; load RealNodes into a normal Array instead")

Base.size(n::ArrayNode) = size(untrack(n))

Base.length(n::ArrayNode) = length(untrack(n))

Base.indices(n::ArrayNode) = indices(untrack(n))

Base.start(n::ArrayNode) = @intercept(start)(untrack(n))

Base.next(n::ArrayNode, state) = @intercept(next)(untrack(n), state)

Base.done(n::ArrayNode, state) = @intercept(done)(untrack(n), state)

Base.IndexStyle(::Type{T}) where {T<:ArrayNode} = IndexStyle(valtype(T))
Base.IndexStyle(n::ArrayNode) = IndexStyle(untrack(n))

Base.similar(n::ArrayNode) = similar(untrack(n), eltype(n))
Base.similar(n::ArrayNode, ::Type{T}) where {T} = similar(untrack(n), T)
Base.similar(n::ArrayNode, ::Type{T}, dims) where {T} = similar(untrack(n), T, dims)

###########################
# Miscellaneous Functions #
###########################

Base.copy(n::ArrayNode) = @intercept(copy)(n)

Base.ones(n::ArrayNode) = @intercept(ones)(n)
Base.ones(n::ArrayNode, ::Type{T}) where {T} = @intercept(ones)(n, T)
Base.ones(n::ArrayNode, ::Type{T}, dims::Tuple) where {T} = @intercept(ones)(n, T, dims)
Base.ones(n::ArrayNode, ::Type{T}, dims...) where {T} = @intercept(ones)(n, T, dims...)

Base.zeros(n::ArrayNode) = @intercept(zeros)(n)
Base.zeros(n::ArrayNode, ::Type{T}) where {T} = @intercept(zeros)(n, T)
Base.zeros(n::ArrayNode, ::Type{T}, dims::Tuple) where {T} = @intercept(zeros)(n, T, dims)
Base.zeros(n::ArrayNode, ::Type{T}, dims...) where {T} = @intercept(zeros)(n, T, dims...)

Base.reshape(n::ArrayNode, dims::Type{Val{N}}) where {N} = @intercept(reshape)(n, dims)
Base.reshape(n::ArrayNode, dims::Tuple{Vararg{Int,N}}) where {N} = @intercept(reshape)(n, dims)
Base.reshape(n::ArrayNode, dims::Int64...) = @intercept(reshape)(n, dims...)
Base.reshape(n::ArrayNode, dims::AbstractUnitRange...) = @intercept(reshape)(n, dims...)
Base.reshape(n::ArrayNode, dims::Union{AbstractUnitRange,Int64}...) = @intercept(reshape)(n, dims...)

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
    @eval @inline Base.$(f)(a::ArrayNode, b::ArrayNode) = @intercept($(f))(a, b)
    @eval @inline Base.$(f)(a::ArrayNode, b::RealNode) = @intercept($(f))(a, b)
    @eval @inline Base.$(f)(a::RealNode, b::ArrayNode) = @intercept($(f))(a, b)
    for T in vcat(ARRAY_TYPES, REAL_TYPES)
        @eval @inline Base.$(f)(a::ArrayNode, b::$T) = @intercept($(f))(a, b)
        @eval @inline Base.$(f)(a::$T, b::ArrayNode) = @intercept($(f))(a, b)
    end
end

for f in UNARY_ARRAY_MATH
    @eval @inline Base.$(f)(n::ArrayNode) = @intercept($(f))(n)
end

Base.sum(n::ArrayNode, dims) = @intercept(sum)(n, dims)

Base.prod(n::ArrayNode, dims) = @intercept(prod)(n, dims)

Base.cumsum(n::ArrayNode, dim) = @intercept(cumsum)(n, dim)

Base.cumprod(n::ArrayNode, dim) = @intercept(cumprod)(n, dim)

Base.normalize(n::ArrayNode, p) = @intercept(normalize)(n, p)

Base.norm(n::ArrayNode, p) = @intercept(norm)(n, p)

Base.vecnorm(n::ArrayNode, p) = @intercept(vecnorm)(n, p)

Base.cond(n::ArrayNode, p) = @intercept(cond)(n, p)
