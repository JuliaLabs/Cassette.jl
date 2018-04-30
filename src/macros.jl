#######################
# unhygeniec bindings #
#######################

const CONTEXT_TYPE_BINDING = Symbol("__CONTEXT__")
const CONTEXT_BINDING = Symbol("__context__")

############
# @context #
############

function tag end # only overloaded on a per-context basis

"""
    Cassette.@context Ctx

Define a new Cassette context type with the name `Ctx`.
"""
macro context(Ctx)
    @assert isa(Ctx, Symbol) "context name must be a Symbol"
    CtxTag = gensym(string(Ctx, "Tag"))
    return esc(quote
        struct $CtxTag{E,H} <: $Cassette.AbstractTag end
        Base.@pure $CtxTag(x) = $CtxTag(nothing, x)
        Base.@pure $CtxTag(::E, ::X) where {E,X} = $CtxTag{E,objectid(X)}()

        struct $Ctx{M,w,P<:Union{$Cassette.AbstractPass,$Cassette.Unused},T<:Union{$CtxTag,Nothing}} <: $Cassette.AbstractContext{w,P,T}
            metadata::M
            world::Val{w}
            pass::P
            tag::T
        end

        function $Ctx(;
                      metadata = $Cassette.UNUSED,
                      world::Val = Val($Cassette.get_world_age()),
                      pass::Union{$Cassette.AbstractPass,$Cassette.Unused} = $Cassette.UNUSED)
            return $Ctx(metadata, world, pass, nothing)
        end

        function $Cassette.tag(ctx::$Ctx, f)
            return $Ctx(ctx.metadata, ctx.world, ctx.pass, $CtxTag(f))
        end

        # default primitives/execution definitions
        $Cassette.@primitive function $CtxTag(x) where {__CONTEXT__<:$Ctx}
            return $CtxTag(__context__.tag, x)
        end
        $Cassette.@execution function Core._apply(f, args...) where {__CONTEXT__<:$Ctx}
            flattened_args = Core._apply(tuple, args...)
            return $Cassette.overdub_execute(__context__, f, flattened_args...)
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
    return quote
        func = $(esc(CONTEXT_BINDING)) -> $(esc(expr))
        ctx = $Cassette.tag($(esc(ctx)), func)
        $Cassette.overdub_recurse(ctx, func, ctx)
    end
end

#########
# @pass #
#########

"""
    Cassette.@pass transform

Return a Cassette pass that applies `transform` to all overdubbed method bodies. `transform`
must be callable with the following signature:

    transform(signature::Type{Tuple{...}}, method_body::CodeInfo)::CodeInfo

Note that this macro expands to an `eval` call and thus should only be called at top-level.
Furthermore, to avoid world-age issues, `transform` should not be overloaded after it has
been registered with `@pass`.
"""
macro pass(transform)
    Pass = gensym("PassType")
    name = Expr(:quote, :($__module__.$Pass))
    line = Expr(:quote, __source__.line)
    file = Expr(:quote, __source__.file)
    return esc(quote
        struct $Pass <: $Cassette.AbstractPass end
        (::Type{$Pass})(signature, codeinfo) = $transform(signature, codeinfo)
        eval($Cassette, $Cassette.overdub_recurse_definition($name, $line, $file))
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

    mutable struct Count{T}
        count::Int
    end

    @posthook function (f::Any)(output::T, input::T, ::T...) where {T,__CONTEXT__<:CountCtx{Count{T}}}
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
    return contextual_definition!(:($Cassette.execution), method)
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
    return contextual_definition!(:($Cassette.is_user_primitive), signature, body)
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
    signature = deepcopy(first(method.args))
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
            "method signature missing `where` clause; `$(CONTEXT_BINDING) <: ContextType` "*
            "must be defined in your method definition's `where` clause")
    signature = typify_signature(signature)
    world_binding = gensym("world")
    push!(signature.args, world_binding)
    signature.args[1] = Expr(:call, f,
                             :($CONTEXT_BINDING::$CONTEXT_TYPE_BINDING),
                             :(::Val{$world_binding}),
                             signature.args[1].args...)
    pushfirst!(body.args, Expr(:meta, :inline))
    return esc(Expr(:function, signature, body))
end
