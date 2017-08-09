#######
# Tag #
#######
# Note that the code/comments here were originally used for ForwardDiff's tagging system,
# and have been modified from their original version to fit your TV - er, to fit Cassette's
# use case.

struct Tag{T} end

@inline tagtype(::Tag{T}) where {T} = T

# Here, we could've just as easily used `hash`; however, this is unsafe/undefined behavior
# if `hash(::Type{F})` is overloaded in a module loaded after Cassette. Thus, we instead
# use `hash(Symbol(F))`, which is somewhat safer since it's far less likely that somebody
# would overwrite the Base definition for `Symbol(::DataType)` or `hash(::Symbol)`.
@generated function Tag(::F, ::Val{C}) where {F,C}
    @assert isa(C, Symbol)
    T = hash(C, hash(Symbol(F)))
    return quote
        $(Expr(:meta, :inline))
        Tag{$T}()
    end
end

################################
# AbstractContext/AbstractMeta #
################################

abstract type AbstractContext{T,F} end
abstract type AbstractMeta{T,V} end

macro context(Ctx, Meta)
    return esc(quote
        struct $Ctx{F,T} <: $Cassette.AbstractContext{T,F}
            func::F
            tag::$Cassette.Tag{T}
            @inline function $Ctx(func::F) where {F}
                tag = $Tag(func, Val($(Expr(:quote, Ctx))))
                return new{F,tagtype(tag)}(func, tag)
            end
            @inline function $Ctx(::AbstractContext)
                error("cannot nest contexts directly; they must be separated via a Trace barrier")
            end
        end
        struct $Meta{V0,M,T,V} <: $Cassette.AbstractMeta{T,V}
            value::V
            meta::M
            tag::Tag{T}
            @inline function $Meta(ctx::$Ctx{C,T}, value::V, meta::M = nothing) where {C,T,V,M}
                new{V,M,T,V}(value, meta, ctx.tag)
            end
            @inline function $Meta(ctx::$Ctx{C,T}, value::Type{V}, meta::M = nothing) where {C,T,V,M}
                new{Type{V},M,T,Type{V}}(value, meta, ctx.tag)
            end
            @inline function $Meta(ctx::$Ctx{C,T}, value::Meta{V0}, meta::M = nothing) where {C,T,V0,V,M}
                new{V0,M,T,typeof(value)}(value, meta, ctx.tag)
            end
        end
    end)
end

@inline unbox(x) = x
@inline unbox(ctx::AbstractContext) = ctx.func
@inline unbox(::Type{C}) where {F,C<:AbstractContext{<:Any,F}} = F

@inline unbox(::AbstractContext, x) = x
@inline unbox(::AbstractContext{T}, m::AbstractMeta{T}) where {T} = m.value
@inline unbox(::Type{C}, ::Type{M}) where {T,V,C<:AbstractContext{T},M<:AbstractMeta{T,V}} = V

@inline metacall(f, ::AbstractContext, m) = m
@inline metacall(f, ::AbstractContext{T}, m::AbstractMeta{T}) where {T} = f(m.value, m.meta)

@inline unboxcall(ctx::AbstractContext, args...) = call(x -> unbox(ctx, x), unbox(ctx), args...)
