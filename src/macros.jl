const CONTEXT_TYPE_BINDING = Symbol("__CONTEXT__")
const CONTEXT_BINDING = Symbol("__context__")

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
    return esc(quote
        struct $CtxName <: $Cassette.AbstractContextName end

        Base.show(io::IO, ::Type{$CtxName}) = print(io, "nametype(", $(string(Ctx)), ")")

        const $Ctx{M,T<:Union{Nothing,$Cassette.Tag},P<:$Cassette.AbstractPass} = $Cassette.Context{$CtxName,M,P,T}

        $Ctx(; kwargs...) = $Cassette.Context($CtxName(); kwargs...)

        $Cassette.invnametype(::Type{$CtxName}) = $Ctx

        $Cassette.@primitive function $Cassette.Tag(::Type{N}, ::Type{X}) where {__CONTEXT__<:$Ctx,N,X}
            return Tag(N, X, $Cassette.tagtype(__CONTEXT__))
        end

        $Cassette.@primitive function Core._apply(f, args...) where {__CONTEXT__<:$Ctx}
            return $Cassette.overdub(__context__, f, Core._apply(tuple, args...)...)
        end

        # enforce `T<:Cassette.Tag` to ensure that we only call the below primitive functions
        # if the context has the tagging system enabled

        $Cassette.@primitive function $Cassette.tag(value, ::__CONTEXT__, metadata) where {__CONTEXT__<:$Ctx{<:Any,<:$Cassette.Tag}}
            return $Cassette.tag(value, __context__, metadata)
        end

        $Cassette.@primitive function Array{T,N}(undef::UndefInitializer, args...) where {T,N,__CONTEXT__<:$Ctx{<:Any,<:$Cassette.Tag}}
            return $Cassette.tagged_new_array(__context__, Array{T,N}, undef, args...)
        end

        $Cassette.@primitive function Core.Module(args...) where {__CONTEXT__<:$Ctx{<:Any,<:$Cassette.Tag}}
            return $Cassette.tagged_new_module(__context__, args...)
        end

        $Cassette.@primitive function Core.tuple(args...) where {__CONTEXT__<:$Ctx{<:Any,<:$Cassette.Tag}}
            return $Cassette.tagged_new_tuple(__context__, args...)
        end

        $Cassette.@primitive function Core._apply(f, args...) where {__CONTEXT__<:$Ctx{<:Any,<:$Cassette.Tag}}
            return $Cassette.tagged_apply(__context__, f, args...)
        end

        $Cassette.@primitive function Base.nameof(m) where {__CONTEXT__<:$Ctx{<:Any,<:$Cassette.Tag}}
            return $Cassette.tagged_nameof(__context__, m)
        end

        $Cassette.@primitive function Core.getfield(x, name) where {__CONTEXT__<:$Ctx{<:Any,<:$Cassette.Tag}}
            return $Cassette.tagged_getfield(__context__, x, name)
        end

        $Cassette.@primitive function Core.setfield!(x, name, y) where {__CONTEXT__<:$Ctx{<:Any,<:$Cassette.Tag}}
            return $Cassette.tagged_setfield!(__context__, x, name, y)
        end

        $Cassette.@primitive function Core.arrayref(boundscheck, x, i) where {__CONTEXT__<:$Ctx{<:Any,<:$Cassette.Tag}}
            return $Cassette.tagged_arrayref(__context__, boundscheck, x, i)
        end

        $Cassette.@primitive function Core.arrayset(boundscheck, x, y, i) where {__CONTEXT__<:$Ctx{<:Any,<:$Cassette.Tag}}
            return $Cassette.tagged_arrayset(__context__, boundscheck, x, y, i)
        end

        $Cassette.@primitive function Base._growbeg!(x, delta) where {__CONTEXT__<:$Ctx{<:Any,<:$Cassette.Tag}}
            return $Cassette.tagged_growbeg!(__context__, x, delta)
        end

        $Cassette.@primitive function Base._growend!(x, delta) where {__CONTEXT__<:$Ctx{<:Any,<:$Cassette.Tag}}
            return $Cassette.tagged_growend!(__context__, x, delta)
        end

        $Cassette.@primitive function Base._growat!(x, i, delta) where {__CONTEXT__<:$Ctx{<:Any,<:$Cassette.Tag}}
            return $Cassette.tagged_growat!(__context__, x, i, delta)
        end

        $Cassette.@primitive function Base._deletebeg!(x, delta) where {__CONTEXT__<:$Ctx{<:Any,<:$Cassette.Tag}}
            return $Cassette.tagged_deletebeg!(__context__, x, delta)
        end

        $Cassette.@primitive function Base._deleteend!(x, delta) where {__CONTEXT__<:$Ctx{<:Any,<:$Cassette.Tag}}
            return $Cassette.tagged_deleteend!(__context__, x, delta)
        end

        $Cassette.@primitive function Base._deleteat!(x, i, delta) where {__CONTEXT__<:$Ctx{<:Any,<:$Cassette.Tag}}
            return $Cassette.tagged_deleteat!(__context__, x, i, delta)
        end

        $Cassette.@primitive function Core.typeassert(x, typ) where {__CONTEXT__<:$Ctx{<:Any,<:$Cassette.Tag}}
            return $Cassette.tagged_typeassert(__context__, x, typ)
        end

        $Cassette.@primitive function (f::Core.IntrinsicFunction)(args...) where {__CONTEXT__<:$Ctx{<:Any,<:$Cassette.Tag}}
            if f === Base.sitofp
                return $Cassette.tagged_sitofp(__context__, args...)
            elseif f === Base.sle_int
                return $Cassette.tagged_sle_int(__context__, args...)
            else # TODO: add more cases
                return $Cassette.call(__context__, f, args...)
            end
        end
    end)
end

############
# @overdub #
############

"""
    Cassette.@overdub(Ctx(...), expression)
A convenience macro for overdubbing and executing `expression` within the context `Ctx`.
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

#####################
# @prehook/posthook #
#####################

"""
    Cassette.@prehook contextual_method_definition

Place in front of a contextual method definition to define a callback that Cassette
executes before method calls that match the contextual method definition's signature.

For example, the following code uses `@prehook` to increment a counter stored in
`__context__.metadata` every time a method is called with one or more arguments matching a
given type:

    using Cassette: @context, @prehook

    @context CountCtx

    mutable struct Count{T}
        count::Int
    end

    @prehook function (f::Any)(input::T, ::T...) where {T,__CONTEXT__<:CountCtx{Count{T}}}
        __context__.metadata.count += 1
    end

Prehooks are generally useful for inserting side-effects that do not ultimately affect
the trace's execution path. However, prehooks are allowed to (for example) mutate input
argument state.

For details regarding the format of `contextual_method_definition`, see the contextual
dispatch documentation.
"""
macro prehook(method)
    return contextual_definition!(:($Cassette.prehook), method)
end

"""
    Cassette.@posthook contextual_method_definition

Place in front of a contextual method definition to define a callback that Cassette executes
after method calls that match the contextual method definition's signature. Note that the
contextual method definition's signature differs from those of the matched method calls in
that the first argument is the method calls' output.

For example, the following code uses `@posthook` to increment a counter stored in
`__context__.metadata` every time a method call's output type matches the type of its
input:

    using Cassette: @context, @posthook

    @context CountCtx

    mutable struct Count
        count::Int
    end

    @posthook function (f::Any)(output::T, input::T, ::T...) where {T,__CONTEXT__<:CountCtx{Count}}
        __context__.metadata.count += 1
    end

Posthooks are generally useful for inserting side-effects that do not ultimately affect
the trace's execution path. However, posthooks are allowed to (for example) mutate
input/output argument state.

For details regarding the format of `contextual_method_definition`, see the contextual
dispatch documentation.
"""
macro posthook(method)
    return contextual_definition!(:($Cassette.posthook), method)
end

##############
# @execution #
##############

"""
    Cassette.@execution contextual_method_definition

Place in front of a contextual method definition to overload Cassette's primitive execution
behavior for method calls matching the contextual method definition's signature. Note that
this execution behavior does not apply to non-primitives, which, by definition, Cassette
will simply trace through.

For details regarding the format of `contextual_method_definition`, see the contextual
dispatch documentation.
"""
macro execution(method)
    return contextual_definition!(:($Cassette.execute), method)
end

################
# @isprimitive #
################

"""
    Cassette.@isprimitive contextual_method_signature

Place in front of a contextual method signature to mark matching method calls as Cassette
primitives. Cassette primitives are executed using the contextual method definitions
provided via `Cassette.@execution`.

For details regarding the format of `contextual_method_signature`, see the contextual
dispatch documentation.
"""
macro isprimitive(signature)
    body = Expr(:block)
    push!(body.args, :(return true))
    return contextual_definition!(:($Cassette.isprimitive), signature, body)
end

##############
# @primitive #
##############

"""
    Cassette.@primitive contextual_method_definition

A convenience macro for simultaneously applying `Cassette.@execution` and
`Cassette.@isprimitive` to the provided contextual method definition.
"""
macro primitive(method)
    @assert is_method_definition(method)
    signature = first(method.args)
    return esc(quote
        $Cassette.@execution $method
        $Cassette.@isprimitive $signature
    end)
end

#############
# utilities #
#############

function typify_signature(signature)
    if isa(signature, Expr)
        f, args = signature.args[1], signature.args[2:end]
        if signature.head == :where
            return Expr(:where, typify_signature(f), args...)
        elseif signature.head == :call
            if !(isa(f, Expr) && f.head == :(::))
                # Use Core.Typeof here instead of typeof so that we don't, for example,
                # overload UnionAll when users attempt to overload type constructors
                return Expr(:call, :(::Core.Typeof($f)), args...)
            end
            return signature
        end
    end
    error("malformed signature: $signature")
end

function contextual_definition!(f, method)
    @assert is_method_definition(method)
    signature, body = method.args
    return contextual_definition!(f, signature, body)
end

function contextual_definition!(f, signature::Expr, body::Expr)
    @assert(signature.head == :where,
            "method signature missing `where` clause; `$(CONTEXT_TYPE_BINDING) <: ContextType` "*
            "must be defined in your method definition's `where` clause")
    signature = typify_signature(signature)
    signature.args[1] = Expr(:call, f,
                             :($CONTEXT_BINDING::$CONTEXT_TYPE_BINDING),
                             signature.args[1].args...)
    pushfirst!(body.args, Expr(:meta, :inline))
    return esc(Expr(:function, signature, body))
end
