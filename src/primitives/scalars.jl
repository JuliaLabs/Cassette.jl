########################
# Conversion/Promotion #
########################

Base.convert(T::Type{<:Real}, n::RealNode) = @primitive(convert)(T, n)
Base.convert(T::Type{<:RealNode}, x::Real) = @primitive(convert)(T, x)
Base.convert(::Type{T}, n::T) where {T<:RealNode} = n

Base.promote_rule(A::Type{<:Real}, B::Type{<:RealNode}) = @primitive(promote_rule)(A, B)
Base.promote_rule(::Type{RealNode{GA,A}}, ::Type{RealNode{GB,B}}) where {A<:Real,B<:Real} = RealNode{promote_genre(GA,GB),promote_type(A,B)}

for R in REAL_TYPES
    @eval Base.promote_rule(::Type{$R}, ::Type{RealNode{V}}) where {V<:Real} = RealNode{promote_type($R,V)}
end

Base.promote_array_type(_, ::Type{T}, ::Type{F}) where {T<:RealNode,F<:AbstractFloat} = promote_type(T, F)
Base.promote_array_type(_, ::Type{F}, ::Type{T}) where {F<:AbstractFloat,T<:RealNode} = promote_type(T, F)
Base.promote_array_type(_, ::Type{T}, ::Type{F}, ::Type{S}) where {T<:RealNode,F<:AbstractFloat,S} = S
Base.promote_array_type(_, ::Type{F}, ::Type{T}, ::Type{S}) where {F<:AbstractFloat,T<:RealNode,S} = S

Base.r_promote(::typeof(+), x::RealNode) = x
Base.r_promote(::typeof(*), x::RealNode) = x

########
# Math #
########

for f in RealInterface.UNARY_MATH
    @eval @inline Base.$(f)(x::RealNode) = @primitive($f)(x)
end

for f in RealInterface.UNARY_SPECIAL_MATH
    @eval @inline SpecialFunctions.$(f)(x::RealNode) = @primitive($f)(x)
end

for f in RealInterface.BINARY_MATH
    @eval @inline Base.$(f)(a::RealNode, b::RealNode) = @primitive($f)(a, b)
    for R in REAL_TYPES
        @eval begin
            @inline Base.$(f)(a::RealNode, b::$R) = @primitive($f)(a, b)
            @inline Base.$(f)(a::$R, b::RealNode) = @primitive($f)(a, b)
        end
    end
end

for f in RealInterface.BINARY_SPECIAL_MATH
    @eval @inline SpecialFunctions.$(f)(a::RealNode, b::RealNode) = @primitive($f)(a, b)
    for R in REAL_TYPES
        @eval begin
            @inline SpecialFunctions.$(f)(a::RealNode, b::$R) = @primitive($f)(a, b)
            @inline SpecialFunctions.$(f)(a::$R, b::RealNode) = @primitive($f)(a, b)
        end
    end
end

##############
# Predicates #
##############

for f in RealInterface.UNARY_PREDICATES
    @eval @inline Base.$(f)(x::RealNode) = @primitive($(f))(x)
end

for f in RealInterface.BINARY_PREDICATES
    @eval @inline Base.$(f)(a::RealNode, b::RealNode) = @primitive($(f))(a, b)
    for R in REAL_TYPES
        @eval begin
            @inline Base.$(f)(a::$R, b::RealNode) = @primitive($(f))(a, b)
            @inline Base.$(f)(a::RealNode, b::$R) = @primitive($(f))(a, b)
        end
    end
end

#################
# Miscellaneous #
#################

Base.copy(n::RealNode) = @primitive(copy)(n)

Base.hash(n::RealNode) = @primitive(hash)(n)
Base.hash(n::RealNode, hsh::UInt) = @primitive(hash)(n, hsh)

Base.float(n::RealNode) = @primitive(float)(n)

Base.one(T::Type{<:RealNode}) = @primitive(one)(T)

Base.zero(T::Type{<:RealNode}) = @primitive(zero)(T)

Base.rand(T::Type{<:RealNode}) = @primitive(rand)(T)
Base.rand(rng::AbstractRNG, T::Type{<:RealNode}) = @primitive(rand)(rng, T)

Base.eps(n::RealNode) = @primitive(eps)(n)
Base.eps(T::Type{<:RealNode}) = @primitive(eps)(T)

Base.floor(n::RealNode) = @primitive(floor)(n)
Base.floor(T::Type{<:Real}, n::RealNode) = @primitive(floor)(T, n)

Base.ceil(n::RealNode) = @primitive(ceil)(n)
Base.ceil(T::Type{<:Real}, n::RealNode) =  @primitive(ceil)(T, n)

Base.trunc(n::RealNode) = @primitive(trunc)(n)
Base.trunc(T::Type{<:Real}, n::RealNode) = @primitive(trunc)(T, n)

Base.round(n::RealNode) = @primitive(round)(n)
Base.round(T::Type{<:Real}, n::RealNode) = @primitive(round)(T, n)

Base.rtoldefault(T::Type{<:RealNode}) = @primitive(rtoldefault)(T)
