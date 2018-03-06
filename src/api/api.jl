############
# bindings #
############

const CONTEXT_BINDING = Symbol("__CONTEXT__")
const METADATA_BINDING = Symbol("__METADATA__")
const CONFIG_BINDING = Symbol("__trace__")

############
# @context #
############

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
    end)
end

#########
# @pass #
#########

macro pass(Pass, transform)
    @assert isa(Pass, Symbol) "pass name must be a Symbol"
    name = Expr(:quote, :($__module__.$Pass))
    line = Expr(:quote, __source__.line)
    file = Expr(:quote, __source__.file)
    return esc(quote
        struct $Pass <: $Cassette.AbstractPass end
        (::Type{$Pass})(signature, codeinfo) = $transform(signature, codeinfo)
        eval($Cassette, $Cassette.overdub_transform_call_definition($name, $line, $file))
    end)
end

#####################
# @prehook/posthook #
#####################

macro prehook(method)
    return contextual_definition!(:($Cassette.prehook), method)
end

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
    push!(body.args, :(return Val(true)))
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
