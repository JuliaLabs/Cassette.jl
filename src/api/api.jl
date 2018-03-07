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
    Cassette.@context CtxType

Defined and return a new Cassette context type with the name `CtxType`.
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

Mark the given `transform` function is as a valid Cassette pass. When used as a Cassette
pass, `transform` will be called as:

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
    Cassette.@prehook

Place in front of a contextual method definition to overload the callback that Cassette
executes before every method call in the target trace. The signature of the given method
definition matches the method calls for which the prehook is executed.

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
the trace's execution path, but are allowed to (for example) mutate input argument state.

For details on the contextual method definition format accepted by `@prehook`, see the
contextual dispatch documentation.
"""
macro prehook(method)
    return contextual_definition!(:($Cassette.prehook), method)
end

"""
    Cassette.@posthook

Place in front of a contextual method definition to overload the callback that Cassette
executes after every method call in the target trace. The signature of the given method
definition matches the method calls for which the prehook is executed, except that the
first argument is the method call's output.

For example, the following code uses `@posthook` to increment a counter stored in
`__trace__.metadata` every time a method call's output type matches the type of its
input:

    using Cassette: @context, @prehook

    @context CountCtx

    mutable struct Count{T}
        count::Int
    end

    @prehook function (f::Any)(output::T, input::T, ::T...) where {T,__CONTEXT__<:CountCtx,__METADATA__<:Count{T}}
        __trace__.metadata.count += 1
    end

Posthooks are generally useful for inserting side-effects that do not ultimately affect
the trace's execution path, but are allowed to (for example) mutate input/output argument
state.

For details on the contextual method definition format accepted by `@posthook`, see the
contextual dispatch documentation.
"""
macro posthook(method)
    return contextual_definition!(:($Cassette.posthook), method)
end

##############
# @execution #
##############

macro execution(method)
    return contextual_definition!(:($Cassette.execution), method)
end

################
# @isprimitive #
################

macro isprimitive(signature)
    body = Expr(:block)
    push!(body.args, :(return true))
    return contextual_definition!(:($Cassette.is_user_primitive), signature, body)
end

##############
# @primitive #
##############

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

macro overdub(ctx, ex)
  :(overdub($(esc(ctx)), () -> $(esc(ex)))())
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

macro Box(args...)
    error("cannot use @Box macro outside of the scope of Cassette's contextual macros (@execution, @isprimitive, @primitive, @prehook, @posthook)")
end

isboxmacrocall(x) = isa(x, Expr) && x.head == :macrocall && x.args[1] == Symbol("@Box")

function is_metadata_typevar(x)
    return (x == METADATA_BINDING || isa(x, Expr) && x.head == :(<:) && x.args[1] == METADATA_BINDING)
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
    !(any(is_metadata_typevar, signature.args)) && push!(signature.args, METADATA_BINDING)

    # add world age typevar
    world_binding = gensym("world")
    push!(signature.args, world_binding)

    config_arg = :($CONFIG_BINDING::$Cassette.TraceConfig{$CONTEXT_BINDING,$METADATA_BINDING,$world_binding})

    call_args = signature.args[1].args
    for i in 1:length(call_args)
        x = call_args[i]
        if isa(x, Expr) && x.head == :(::)
            xtype = last(x.args)
            if isboxmacrocall(xtype)
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
