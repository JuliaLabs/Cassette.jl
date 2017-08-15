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

abstract type AbstractContext{F,T} end
abstract type AbstractMeta{V,M,T,U} end

macro context(Ctx, Meta = nothing)
    expr = Expr(:block)
    push!(expr.args, quote
        struct $Ctx{F,T} <: $Cassette.AbstractContext{F,T}
            func::F
            tag::$Cassette.Tag{T}
            @inline $Ctx(func::F, tag::$Cassette.Tag{T}) where {F,T} = new{F,T}(func, tag)
            @inline $Ctx(func::$Cassette.AbstractContext, tag::$Cassette.Tag{T}) where {T} = error("cannot nest contexts without a Trace barrier")
        end
        @inline $Ctx(f) = $Ctx(f, $Cassette.Tag(f, Val($(Expr(:quote, Ctx)))))
        @inline $Cassette.wrap(ctx::$Ctx, f) = $Ctx(f, ctx.tag)
    end)
    Meta !== nothing && push!(expr.args, quote
        struct $Meta{V,M,T,U} <: $Cassette.AbstractMeta{V,M,T,U}
            value::U
            meta::M
            tag::$Cassette.Tag{T}
            @inline function $Meta(ctx::$Ctx{C,T}, value::V, meta::M = nothing) where {C,T,V,M}
                new{V,M,T,V}(value, meta, ctx.tag)
            end
            @inline function $Meta(ctx::$Ctx{C,T}, value::Type{V}, meta::M = nothing) where {C,T,V,M}
                new{Type{V},M,T,Type{V}}(value, meta, ctx.tag)
            end
            @inline function $Meta(ctx::$Ctx{C,T}, value::$Cassette.AbstractMeta{V,M,T}, meta::M = nothing) where {C,V,M,T}
                new{V,M,T,typeof(value)}(value, meta, ctx.tag)
            end
        end
    end)
    return esc(expr)
end

# this stub only exists to be extended by Cassette.@context
function wrap end

@inline unwrap(x) = x
@inline unwrap(ctx::AbstractContext) = ctx.func
@inline unwrap(::Type{C}) where {F,C<:AbstractContext{F}} = F

@inline unwrap(::AbstractContext, x) = x
@inline unwrap(::Type{C},         x) where {C<:AbstractContext} = x
@inline unwrap(::AbstractContext{<:Any,T}, m::AbstractMeta{<:Any,<:Any,T}) where {T} = m.value
@inline unwrap(::Type{C}, ::Type{M}) where {F,V,VM,T,U,C<:AbstractContext{F,T},M<:AbstractMeta{V,VM,T,U}} = U

@inline unwrapcall(ctx::AbstractContext, args...) = mapcall(x -> unwrap(ctx, x), unwrap(ctx), args...)

@inline contextcall(f, ctx::AbstractContext, arg) = unwrap(ctx)(arg)
@inline contextcall(f, ctx::AbstractContext{T}, arg::AbstractMeta{T}) where {T} = f(arg.value, arg.meta)

@inline hascontext(::AbstractContext, ::Any) = false
@inline hascontext(::Type{<:AbstractContext}, ::Any) = false
@inline hascontext(::AbstractContext{<:Any,T}, ::AbstractMeta{<:Any,<:Any,T}) where {T} = true
@inline hascontext(::Type{C}, ::Type{M}) where {F,V,VM,T,U,C<:AbstractContext{F,T},M<:AbstractMeta{V,VM,T,U}} = true
