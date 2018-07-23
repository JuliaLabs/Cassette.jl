############
# @context #
############

"""
    Cassette.@context Ctx

Define a new Cassette context type with the name `Ctx`.
"""
macro context(Ctx)
    @assert isa(Ctx, Symbol) "context name must be a Symbol"
    CtxName = gensym(string(Ctx, "Name"))
    TaggedCtx = gensym(string(Ctx, "Tagged"))
    Typ = :(Core.Typeof)
    return esc(quote
        struct $CtxName <: $Cassette.AbstractContextName end

        Base.show(io::IO, ::Type{$CtxName}) = print(io, "nametype(", $(string(Ctx)), ")")

        const $Ctx{M,T<:Union{Nothing,$Cassette.Tag},P<:$Cassette.AbstractPass} = $Cassette.Context{$CtxName,M,P,T}
        const $TaggedCtx = $Ctx{<:Any,<:$Cassette.Tag}

        $Ctx(; kwargs...) = $Cassette.Context($CtxName(); kwargs...)

        @inline $Cassette.execute(::C, ::$Typ($Cassette.Tag), ::Type{N}, ::Type{X}) where {C<:$Ctx,N,X} = $Cassette.Tag(N, X, $Cassette.tagtype(C))
        @inline $Cassette.execute(ctx::$Ctx{<:Any,Nothing}, ::$Typ(Core._apply), f, args...) = $Cassette.overdub_apply(ctx, f, args...)
        @inline $Cassette.execute(ctx::C, f::$Cassette.Fallback{F,C}, args...) where {F,C<:$Ctx} = Cassette.fallback(ctx, f.func, args...)

        # TODO: There are certain non-`Core.Builtin` functions which the compiler often
        # relies upon constant propagation to infer, such as `isdispatchtuple`. Such
        # functions should generally be contextual primitives by default for the sake of
        # performance, and we should add more of them here as we encounter them.
        @inline $Cassette.execute(ctx::$Ctx, f::$Typ(Base.isdispatchtuple), T::Type) = Cassette.fallback(ctx, f, T)
        @inline $Cassette.execute(ctx::$Ctx, f::$Typ(Base.eltype), T::Type) = Cassette.fallback(ctx, f, T)
        @inline $Cassette.execute(ctx::$Ctx, f::$Typ(Base.convert), T::Type, t::Tuple) = Cassette.fallback(ctx, f, T, t)
        @inline $Cassette.execute(ctx::$Ctx{<:Any,Nothing}, f::$Typ(Base.getproperty), x::Any, s::Symbol) = Cassette.fallback(ctx, f, x, s)

        # the below primitives are only active when the tagging system is enabled (`typeof(ctx) <: TaggedCtx`)

        @inline $Cassette.execute(ctx::C, f::$Typ($Cassette.tag), value, ::C, metadata) where {C<:$TaggedCtx} = $Cassette.fallback(ctx, f, value, ctx, metadata)
        @inline $Cassette.execute(ctx::$TaggedCtx, ::$Typ(Array{T,N}), undef::UndefInitializer, args...) where {T,N} = $Cassette.tagged_new_array(ctx, Array{T,N}, undef, args...)
        @inline $Cassette.execute(ctx::$TaggedCtx, ::$Typ(Core.Module), args...) = $Cassette.tagged_new_module(ctx, args...)
        @inline $Cassette.execute(ctx::$TaggedCtx, ::$Typ(Core.tuple), args...) = $Cassette.tagged_new_tuple(ctx, args...)
        @inline $Cassette.execute(ctx::$TaggedCtx, ::$Typ(Core._apply), f, args...) = $Cassette.tagged_apply(ctx, f, args...)
        @inline $Cassette.execute(ctx::$TaggedCtx, ::$Typ(Base.nameof), args...) = $Cassette.tagged_nameof(ctx, m)
        @inline $Cassette.execute(ctx::$TaggedCtx, ::$Typ(Core.getfield), args...) = $Cassette.tagged_getfield(ctx, args...)
        @inline $Cassette.execute(ctx::$TaggedCtx, ::$Typ(Core.setfield!), args...) = $Cassette.tagged_setfield!(ctx, args...)
        @inline $Cassette.execute(ctx::$TaggedCtx, ::$Typ(Core.arrayref), args...) = $Cassette.tagged_arrayref(ctx, args...)
        @inline $Cassette.execute(ctx::$TaggedCtx, ::$Typ(Core.arrayset), args...) = $Cassette.tagged_arrayset(ctx, args...)
        @inline $Cassette.execute(ctx::$TaggedCtx, ::$Typ(Base._growbeg!), args...) = $Cassette.tagged_growbeg!(ctx, args...)
        @inline $Cassette.execute(ctx::$TaggedCtx, ::$Typ(Base._growend!), args...) = $Cassette.tagged_growend!(ctx, args...)
        @inline $Cassette.execute(ctx::$TaggedCtx, ::$Typ(Base._growat!), args...) = $Cassette.tagged_growat!(ctx, args...)
        @inline $Cassette.execute(ctx::$TaggedCtx, ::$Typ(Base._deletebeg!), args...) = $Cassette.tagged_deletebeg!(ctx, args...)
        @inline $Cassette.execute(ctx::$TaggedCtx, ::$Typ(Base._deleteend!), args...) = $Cassette.tagged_deleteend!(ctx, args...)
        @inline $Cassette.execute(ctx::$TaggedCtx, ::$Typ(Base._deleteat!), args...) = $Cassette.tagged_deleteat!(ctx, args...)
        @inline $Cassette.execute(ctx::$TaggedCtx, ::$Typ(Core.typeassert), args...) = $Cassette.tagged_typeassert(ctx, args...)

        @inline function $Cassette.execute(ctx::$TaggedCtx, f::Core.IntrinsicFunction, args...)
            if f === Base.sitofp
                return $Cassette.tagged_sitofp(ctx, args...)
            elseif f === Base.sle_int
                return $Cassette.tagged_sle_int(ctx, args...)
            else # TODO: add more cases
                return $Cassette.fallback(ctx, f, args...)
            end
        end
    end)
end

############
# @overdub #
############

"""
    Cassette.@overdub(ctx, expression)

A convenience macro for executing `expression` within the context `ctx`. This macro roughly
expands to `Cassette.recurse(ctx, () -> expression)`.
"""
macro overdub(ctx, expr)
    return :($Cassette.recurse($(esc(ctx)), () -> $(esc(expr))))
end

#########
# @pass #
#########

"""
    Cassette.@pass transform

Return a Cassette pass that applies `transform` to all overdubbed method bodies. `transform`
must be callable with the following signature:

    transform(ctxtype::Type{<:AbstractContext} signature::Type{Tuple{...}}, method_body::CodeInfo)::CodeInfo

Note that this macro expands to an `eval` call and thus should only be called at top-level.
Furthermore, to avoid world-age issues, `transform` should not be overloaded after it has
been registered with `@pass`.

Note also that `transform` should be "relatively pure." More specifically, Julia's compiler
has license to apply `transform` multiple times, even if only compiling a single method
invocation once. Thus, it is required that `transform` always return a generically equivalent
`CodeInfo` for a given context, method body, and signature ("generically equivalent" meaning
`==`, not necessarily `===`).
"""
macro pass(transform)
    Pass = gensym("PassType")
    name = Expr(:quote, :($__module__.$Pass))
    line = Expr(:quote, __source__.line)
    file = Expr(:quote, __source__.file)
    return esc(quote
        struct $Pass <: $Cassette.AbstractPass end
        (::Type{$Pass})(ctxtype, signature, codeinfo) = $transform(ctxtype, signature, codeinfo)
        Core.eval($Cassette, $Cassette.recurse_definition($name, $line, $file))
        $Pass()
    end)
end
