###########################
# External `<:Real` Types #
###########################

const REAL_TYPES = [:Bool, :Integer, :Rational, :BigFloat, :BigInt,
                    :AbstractFloat, :Real]

########
# Math #
########

for f in vcat(RealInterface.UNARY_MATH, RealInterface.UNARY_ARITHMETIC)
    @eval @inline Base.$(f)(x::RealNote) = @intercept($f)(x)
end

for f in vcat(RealInterface.BINARY_MATH, RealInterface.BINARY_ARITHMETIC)
    @eval @inline Base.$(f)(a::RealNote, b::RealNote) = @intercept($f)(a, b)
    for R in REAL_TYPES
        @eval begin
            @inline Base.$(f)(a::RealNote, b::$R) = @intercept($f)(a, b)
            @inline Base.$(f)(a::$R, b::RealNote) = @intercept($f)(a, b)
        end
    end
end

for f in RealInterface.UNARY_SPECIAL_MATH
    @eval @inline SpecialFunctions.$(f)(x::RealNote) = @intercept($f)(x)
end

for f in RealInterface.BINARY_SPECIAL_MATH
    @eval @inline SpecialFunctions.$(f)(a::RealNote, b::RealNote) = @intercept($f)(a, b)
    for R in REAL_TYPES
        @eval begin
            @inline SpecialFunctions.$(f)(a::RealNote, b::$R) = @intercept($f)(a, b)
            @inline SpecialFunctions.$(f)(a::$R, b::RealNote) = @intercept($f)(a, b)
        end
    end
end

##############
# Predicates #
##############

for f in RealInterface.UNARY_PREDICATES
    @eval @inline Base.$(f)(x::RealNote) = @intercept($(f))(x)
end

for f in RealInterface.BINARY_PREDICATES
    @eval @inline Base.$(f)(a::RealNote, b::RealNote) = @intercept($(f))(a, b)
    for R in REAL_TYPES
        @eval begin
            @inline Base.$(f)(a::$R, b::RealNote) = @intercept($(f))(a, b)
            @inline Base.$(f)(a::RealNote, b::$R) = @intercept($(f))(a, b)
        end
    end
end

###########################
# Miscellaneous Functions #
###########################
# Note that some of these functions are not intercepted; this is because they
# inherently return type-based constants (which can be treated as root notes),
# or are equivalent to convert methods, or return output that really shouldn't
# be tracked.

@inline Base.copy(n::RealNote) = n

@inline Base.hash(n::RealNote) = hash(n)

@inline Base.hash(n::RealNote, hsh::UInt) = hash(n, hsh)

@inline Base.float(n::RealNote) = track(float(n), genre(n))

@inline Base.one(::Type{T}) where {T<:RealNote} = track(one(valuetype(T)), genre(T))

@inline Base.zero(::Type{T}) where {T<:RealNote} = track(zero(valuetype(T)), genre(T))

@inline Base.rand(T::Type{<:RealNote}) = @intercept(rand)(T)

@inline Base.rand(rng::AbstractRNG, T::Type{<:RealNote}) = @intercept(rand)(rng, T)

@inline Base.eps(n::RealNote) = @intercept(eps)(n)
@inline Base.eps(::Type{T}) where {T<:RealNote} = track(eps(valuetype(T)), genre(T))

@inline Base.floor(n::RealNote) = @intercept(floor)(n)
@inline Base.floor(T::Type{<:Real}, n::RealNote) = @intercept(floor)(T, n)

@inline Base.ceil(n::RealNote) = @intercept(ceil)(n)
@inline Base.ceil(T::Type{<:Real}, n::RealNote) =  @intercept(ceil)(T, n)

@inline Base.trunc(n::RealNote) = @intercept(trunc)(n)
@inline Base.trunc(T::Type{<:Real}, n::RealNote) = @intercept(trunc)(T, n)

@inline Base.round(n::RealNote) = @intercept(round)(n)
@inline Base.round(T::Type{<:Real}, n::RealNote) = @intercept(round)(T, n)

@inline Base.rtoldefault(T::Type{<:RealNote}) = @intercept(rtoldefault)(T)

########################
# Conversion/Promotion #
########################

Base.convert(::Type{RealNote{G,V,C}}, x::Real) where {G<:AbstractGenre,V<:Real,C} = track(V(x), G())
Base.convert(::Type{RealNote{G,V,C}}, n::RealNote) where {G<:AbstractGenre,V<:Real,C} = track(V(value(n)), G())

Base.convert(::Type{T}, n::RealNote) where {T<:Real} = error("""
                                                             Attempted to convert a value of type $(typeof(n)) to type $T.
                                                             Cassette does not allow such lossy conversions, since they
                                                             disrupt the flow of metadata through the computation, often
                                                             resulting in silent/difficult-to-track-down bugs.
                                                             """)
Base.convert(::Type{T}, n::T) where {T<:RealNote} = n

Base.promote_rule(::Type{T}, ::Type{RealNote{G,V,C}}) where {T<:Real,G,V,C} = RealNote{G,promote_type(T,V),C}

for T in REAL_TYPES
    @eval Base.promote_rule(::Type{$T}, ::Type{RealNote{G,V,C}}) where {G,V,C} = RealNote{G,promote_type($T,V),C}
end

Base.promote_rule(::Type{RealNote{GA,A}}, ::Type{RealNote{GB,B}}) where {GA,A,GB,B} = RealNote{promote_type(GA,GB),promote_type(A,B)}

Base.promote_array_type(_, ::Type{T}, ::Type{F}) where {T<:RealNote,F<:AbstractFloat} = promote_type(T, F)
Base.promote_array_type(_, ::Type{F}, ::Type{T}) where {F<:AbstractFloat,T<:RealNote} = promote_type(T, F)
Base.promote_array_type(_, ::Type{T}, ::Type{F}, ::Type{S}) where {T<:RealNote,F<:AbstractFloat,S} = S
Base.promote_array_type(_, ::Type{F}, ::Type{T}, ::Type{S}) where {F<:AbstractFloat,T<:RealNote,S} = S

Base.r_promote(::typeof(+), x::RealNote) = x
Base.r_promote(::typeof(*), x::RealNote) = x
