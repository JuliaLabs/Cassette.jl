###########################
# External `<:Real` Types #
###########################

const REAL_TYPES = [:Bool, :Integer, :Rational, :BigFloat, :BigInt,
                    :AbstractFloat, :Real]

########
# Math #
########

for f in vcat(RealInterface.UNARY_MATH, RealInterface.UNARY_ARITHMETIC)
    @eval @inline Base.$(f)(x::RealNode) = @intercept($f)(x)
end

for f in vcat(RealInterface.BINARY_MATH, RealInterface.BINARY_ARITHMETIC)
    @eval @inline Base.$(f)(a::RealNode, b::RealNode) = @intercept($f)(a, b)
    for R in REAL_TYPES
        @eval begin
            @inline Base.$(f)(a::RealNode, b::$R) = @intercept($f)(a, b)
            @inline Base.$(f)(a::$R, b::RealNode) = @intercept($f)(a, b)
        end
    end
end

for f in RealInterface.UNARY_SPECIAL_MATH
    @eval @inline SpecialFunctions.$(f)(x::RealNode) = @intercept($f)(x)
end

for f in RealInterface.BINARY_SPECIAL_MATH
    @eval @inline SpecialFunctions.$(f)(a::RealNode, b::RealNode) = @intercept($f)(a, b)
    for R in REAL_TYPES
        @eval begin
            @inline SpecialFunctions.$(f)(a::RealNode, b::$R) = @intercept($f)(a, b)
            @inline SpecialFunctions.$(f)(a::$R, b::RealNode) = @intercept($f)(a, b)
        end
    end
end

##############
# Predicates #
##############

for f in RealInterface.UNARY_PREDICATES
    @eval @inline Base.$(f)(x::RealNode) = @intercept($(f))(x)
end

for f in RealInterface.BINARY_PREDICATES
    @eval @inline Base.$(f)(a::RealNode, b::RealNode) = @intercept($(f))(a, b)
    for R in REAL_TYPES
        @eval begin
            @inline Base.$(f)(a::$R, b::RealNode) = @intercept($(f))(a, b)
            @inline Base.$(f)(a::RealNode, b::$R) = @intercept($(f))(a, b)
        end
    end
end

###########################
# Miscellaneous Functions #
###########################

@inline Base.copy(n::RealNode) = n

@inline Base.hash(n::RealNode) = hash(n)

@inline Base.hash(n::RealNode, hsh::UInt) = hash(n, hsh)

@inline Base.float(n::RealNode) = @intercept(float)(n)

@inline Base.one(T::Type{<:RealNode}) = @intercept(one)(T)

@inline Base.zero(T::Type{<:RealNode}) = @intercept(zero)(T)

@inline Base.rand(T::Type{<:RealNode}) = @intercept(rand)(T)
@inline Base.rand(rng::AbstractRNG, T::Type{<:RealNode}) = @intercept(rand)(rng, T)

@inline Base.eps(n::RealNode) = @intercept(eps)(n)
@inline Base.eps(T::Type{<:RealNode}) = @intercept(eps)(T)

@inline Base.floor(n::RealNode) = @intercept(floor)(n)
@inline Base.floor(T::Type{<:Real}, n::RealNode) = @intercept(floor)(T, n)

@inline Base.ceil(n::RealNode) = @intercept(ceil)(n)
@inline Base.ceil(T::Type{<:Real}, n::RealNode) =  @intercept(ceil)(T, n)

@inline Base.trunc(n::RealNode) = @intercept(trunc)(n)
@inline Base.trunc(T::Type{<:Real}, n::RealNode) = @intercept(trunc)(T, n)

@inline Base.round(n::RealNode) = @intercept(round)(n)
@inline Base.round(T::Type{<:Real}, n::RealNode) = @intercept(round)(T, n)

@inline Base.rtoldefault(T::Type{<:RealNode}) = @intercept(rtoldefault)(T)

########################
# Conversion/Promotion #
########################

Base.convert(::Type{RealNode{G,V,C}}, x::Real) where {G<:AbstractGenre,V<:Real,C} = track(V(x), G())
Base.convert(::Type{RealNode{G,V,C}}, n::RealNode) where {G<:AbstractGenre,V<:Real,C} = track(V(untrack(n)), G())

Base.convert(::Type{T}, n::RealNode) where {T<:Real} = error("""
                                                             Attempted to convert a value of type $(typeof(n)) to type $T.
                                                             Cassette does not allow such lossy conversions, since they
                                                             disrupt the flow of metadata through the computation, often
                                                             resulting in silent/difficult-to-track-down bugs.
                                                             """)
Base.convert(::Type{T}, n::T) where {T<:RealNode} = n

Base.promote_rule(::Type{T}, ::Type{RealNode{G,V,C}}) where {T<:Real,G,V,C} = RealNode{G,promote_type(T,V),C}

for T in REAL_TYPES
    @eval Base.promote_rule(::Type{$T}, ::Type{RealNode{G,V,C}}) where {G,V,C} = RealNode{G,promote_type($T,V),C}
end

Base.promote_rule(::Type{RealNode{GA,A}}, ::Type{RealNode{GB,B}}) where {GA,A,GB,B} = RealNode{promote_type(GA,GB),promote_type(A,B)}

Base.promote_array_type(_, ::Type{T}, ::Type{F}) where {T<:RealNode,F<:AbstractFloat} = promote_type(T, F)
Base.promote_array_type(_, ::Type{F}, ::Type{T}) where {F<:AbstractFloat,T<:RealNode} = promote_type(T, F)
Base.promote_array_type(_, ::Type{T}, ::Type{F}, ::Type{S}) where {T<:RealNode,F<:AbstractFloat,S} = S
Base.promote_array_type(_, ::Type{F}, ::Type{T}, ::Type{S}) where {F<:AbstractFloat,T<:RealNode,S} = S

Base.r_promote(::typeof(+), x::RealNode) = x
Base.r_promote(::typeof(*), x::RealNode) = x
