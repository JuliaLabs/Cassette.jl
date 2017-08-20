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

##################################
# AbstractContext/AbstractCtxArg #
##################################

abstract type AbstractContext{T,F} end
abstract type AbstractCtxArg{T,V,M,U} end

function wrap end # this stub gets overloaded by Cassette.@context

@inline value(ctx, arg) = error("cannot extract `value` field: tag of context $(ctx) does not match tag of argument $(arg)")
@inline value(::AbstractContext{T}, arg::AbstractCtxArg{T}) where {T} = arg.value
@inline value(::Type{C}, ::Type{A}) where {T,V,M,U,C<:AbstractContext{T},A<:AbstractCtxArg{T,V,M,U}} = U

@inline meta(ctx, arg) = error("cannot extract `meta` field: tag of context $(ctx) does not match tag of argument $(arg)")
@inline meta(::AbstractContext{T}, arg::AbstractCtxArg{T}) where {T} = arg.meta
@inline meta(::Type{C}, ::Type{A}) where {T,V,M,C<:AbstractContext{T},A<:AbstractCtxArg{T,V,M}} = M

@inline unwrap(x) = x
@inline unwrap(ctx::AbstractContext) = ctx.func
@inline unwrap(::Type{C}) where {T,F,C<:AbstractContext{T,F}} = F

@inline unwrap(::AbstractContext, arg) = arg
@inline unwrap(ctx::AbstractContext{T}, arg::AbstractCtxArg{T}) where {T} = value(ctx, arg)
@inline unwrap(::Type{C}, ::Type{A}) where {C<:AbstractContext,A} = A
@inline unwrap(::Type{C}, ::Type{A}) where {T,C<:AbstractContext{T},A<:AbstractCtxArg{T}} = value(C, A)

@generated function unwrapcall(ctx::AbstractContext, args...)
    args = [:(unwrap(ctx, args[$i])) for i in 1:nfields(args)]
    return quote
        $(Expr(:meta, :inline))
        unwrap(ctx)($(args...))
    end
end

@inline contextcall(f, g, ::AbstractContext, arg::Any) = g(arg)
@inline contextcall(f, g, ctx::AbstractContext{T}, arg::AbstractCtxArg{T}) where {T} = f(value(ctx, arg), meta(ctx, arg))

@inline hascontext(::AbstractContext, ::Any) = false
@inline hascontext(::Type{<:AbstractContext}, ::Type{<:Any}) = false
@inline hascontext(::AbstractContext{T}, ::AbstractCtxArg{T}) where {T} = true
@inline hascontext(::Type{C}, ::Type{A}) where {T,C<:AbstractContext{T},A<:AbstractCtxArg{T}} = true

############
# @context #
############

macro context(Ctx, CtxArg = nothing)
    expr = Expr(:block)
    push!(expr.args, quote
        struct $Ctx{T,F} <: $Cassette.AbstractContext{T,F}
            tag::$Cassette.Tag{T}
            func::F
            @inline $Ctx(tag::$Cassette.Tag{T}, func::F) where {T,F} = new{T,F}(func, tag)
            @inline $Ctx(tag::$Cassette.Tag{T}, func::$Cassette.AbstractContext) where {T} = error("cannot nest contexts without a Trace barrier")
        end
        @inline $Ctx(f) = $Ctx($Cassette.Tag(f, Val($(Expr(:quote, Ctx)))), f)
        @inline $Cassette.wrap(ctx::$Ctx, f) = $Ctx(ctx.tag, f)
    end)
    CtxArg !== nothing && push!(expr.args, quote
        struct $CtxArg{T,V,M,U} <: $Cassette.AbstractCtxArg{T,V,M,U}
            tag::$Cassette.Tag{T}
            value::U
            meta::M
            @inline function $CtxArg(ctx::$Ctx{T}, value::V, meta::M = nothing) where {T,V,M}
                new{T,V,M,V}(ctx.tag, value, meta)
            end
            @inline function $CtxArg(ctx::$Ctx{T}, value::Type{V}, meta::M = nothing) where {T,V,M}
                new{T,Type{V},M,Type{V}}(ctx.tag, value, meta)
            end
            @inline function $CtxArg(ctx::$Ctx{T}, value::$Cassette.AbstractCtxArg{<:Any,V}, meta::M = nothing) where {T,V,M}
                new{T,V,M,typeof(value)}(ctx.tag, value, meta)
            end
        end
    end)
    return esc(expr)
end

###############
# @contextual #
###############
