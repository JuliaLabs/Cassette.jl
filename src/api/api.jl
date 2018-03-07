############
# bindings #
############

const CONTEXT_BINDING = Symbol("__CONTEXT__")
const METADATA_BINDING = Symbol("__METADATA__")
const CONFIG_BINDING = Symbol("__trace__")

############
# @context #
############

"""
    Cassette.@context Ctx

Defined and return a new Cassette context type with the name `Ctx`.
"""
macro context(Ctx)
    @assert isa(Ctx, Symbol) "context name must be a Symbol"
    name = Expr(:quote, Ctx)
    return esc(quote
        struct $Ctx{T} <: $Cassette.Context{$name,T}
            tag::$Cassette.Tag{T}
        end
        @inline $Ctx(x) = $Ctx($Cassette.Tag(x))
        $Cassette.@execution function Core.getfield(x, field) where {__CONTEXT__<:$Ctx}
            return $Cassette._getfield(x, $Cassette.Name{field}())
        end
        $Cassette.@execution function Core.setfield!(x, field, y) where {__CONTEXT__<:$Ctx}
            return $Cassette._setfield!(x, $Cassette.Name{field}(), y)
        end
        $Ctx
    end)
end

#########
# @pass #
#########

"""
    Cassette.@pass transform

Register `transform` as a Cassette pass. `transform` should be callable with the following
signature:

    transform(signature::Type{Tuple{...}}, method_body::CodeInfo)::CodeInfo

Note that this macro expands to an `eval` call and thus should only be called at top-level.

Furthermore, to avoid world-age issues, `transform` should not be overloaded after it has
been marked with `@pass`.
"""
macro pass(transform)
    name = Expr(:quote, :($__module__.$transform))
    line = Expr(:quote, __source__.line)
    file = Expr(:quote, __source__.file)
    return esc(quote
        @inline $Cassette.ispass(::typeof($transform)) = true
        eval($Cassette, $Cassette.overdub_transform_call_definition($name, $line, $file))
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
`__trace__.metadata` every time a method is called with one or more arguments matching a
given type:

    using Cassette: @context, @prehook

    @context CountCtx

    mutable struct Count{T}
        count::Int
    end

    @prehook function (f::Any)(input::T, ::T...) where {T,__CONTEXT__<:CountCtx,__METADATA__<:Count{T}}
        __trace__.metadata.count += 1
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
`__trace__.metadata` every time a method call's output type matches the type of its
input:

    using Cassette: @context, @posthook

    @context CountCtx

    mutable struct Count{T}
        count::Int
    end

    @posthook function (f::Any)(output::T, input::T, ::T...) where {T,__CONTEXT__<:CountCtx,__METADATA__<:Count{T}}
        __trace__.metadata.count += 1
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

############
# @overdub #
############

"""
    Cassette.@overdub Ctx expression

A convenience macro for overdubbing and executing `expression` within the context `Ctx`.
"""
macro overdub(ctx, ex)
  :(overdub($(esc(ctx)), () -> $(esc(ex)))())
end

########
# @Box #
########

"""
    Cassette.@Box([V, M])

Used within contextual method signatures to indicate the type of a value `V` wrapped in a
`Cassette.Box` with metadata of type `M`. `V` and `M` default to `Any` if unspecified.

For example:

    using Cassette: @context, @prehook, @Box

    @context BoxCtx

    @prehook function (f::Any)(x::@Box) where {__CONTEXT__<:BoxCtx}
        println("The value ", unbox(__trace__.context, x),
                " was wrapped in a Cassette.Box and passed to ", f)
    end

    @prehook function (f::Any)(x::@Box(Number)) where {__CONTEXT__<:BoxCtx}
        println("The numeric value ", unbox(__trace__.context, x),
                " was wrapped in a Cassette.Box and passed to ", f)
    end

    @prehook function (f::Any)(x::@Box(Number,String)) where {__CONTEXT__<:BoxCtx}
        println("The numeric value ", unbox(__trace__.context, x),
                " was wrapped in a Cassette.Box and passed to ", f,
                " and carries the message: ", meta(__trace__.context, x))
    end

Note that `Cassette.@Box` can only used within the scope of a contextual method signature.

For further details, see the contextual metadata propagation documentation.
"""
macro Box(args...)
    error("`Cassette.@Box` can only used within the scope of a contextual method signature")
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
                return Expr(:call, :(::typeof($f)), args...)
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

    # add metadata typevar (if not already present)
    has_metadata_typevar = any(signature.args) do x
        x == METADATA_BINDING || isa(x, Expr) && x.head == :(<:) && x.args[1] == METADATA_BINDING
    end
    !has_metadata_typevar && push!(signature.args, METADATA_BINDING)

    # add world age typevar
    world_binding = gensym("world")
    push!(signature.args, world_binding)

    config_arg = :($CONFIG_BINDING::$Cassette.TraceConfig{$CONTEXT_BINDING,$METADATA_BINDING,$world_binding})

    call_args = signature.args[1].args
    for i in 1:length(call_args)
        x = call_args[i]
        if isa(x, Expr) && x.head == :(::)
            xtype = last(x.args)
            if isa(xtype, Expr) && xtype.head == :macrocall && xtype.args[1] == Symbol("@Box")
                box_args = xtype.args[3:end]
                if isempty(box_args)
                    U, M = :Any, :Any
                elseif length(box_args) == 1
                    U, M = first(box_args), :Any
                elseif length(box_args) == 2
                    U, M = box_args
                else
                    error("incorrect usage of `@Box`: $(xtype)")
                end
                new_xtype = :($Cassette.Box{$CONTEXT_BINDING,<:$U,<:Any,<:$M,true})
            else
                new_xtype = :(Union{$Cassette.Box{<:Any,<:$xtype},$xtype})
            end
            x.args[end] = new_xtype
        end
    end

    signature.args[1] = Expr(:call, f, config_arg, call_args...)

    pushfirst!(body.args, Expr(:meta, :inline))

    return esc(Expr(:function, signature, body))
end
