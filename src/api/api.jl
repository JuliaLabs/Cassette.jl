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
        $Cassette.@execution ctx::$Ctx (::typeof(Core.getfield))(x, field) = $Cassette._getfield(x, $Cassette.Name{field}())
        $Cassette.@execution ctx::$Ctx (::typeof(Core.setfield!))(x, field, y) = $Cassette._setfield!(x, $Cassette.Name{field}(), y)
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

macro prehook(args...)
    ctx, meta, def = unpack_contextual_macro_args(:(::Any), args...)
    return contextual_transform!(ctx, meta, :($Cassette._prehook), def)
end

macro posthook(args...)
    ctx, meta, def = unpack_contextual_macro_args(:(::Any), args...)
    return contextual_transform!(ctx, meta, :($Cassette._posthook), def)
end

##############
# @execution #
##############

macro execution(args...)
    ctx, meta, def = unpack_contextual_macro_args(:(::Any), args...)
    return contextual_transform!(ctx, meta, :($Cassette._execution), def)
end

################
# @isprimitive #
################

macro isprimitive(args...)
    ctx, meta, signature = unpack_contextual_macro_args(:(::Any), args...)
    body = Expr(:block)
    push!(body.args, :(return Val(true)))
    return contextual_transform!(ctx, meta, :($Cassette._isprimitive), signature, body)
end

##############
# @primitive #
##############

macro primitive(args...)
    ctx, meta, def = unpack_contextual_macro_args(:(::Any), args...)
    @assert is_method_definition(def)
    signature = deepcopy(first(def.args))
    return esc(quote
        $Cassette.@execution $ctx $meta $def
        $Cassette.@isprimitive $ctx $meta $signature
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

# returns ctx, meta, def
function unpack_contextual_macro_args(meta_default, args...)
    if length(args) == 2
        return args[1], meta_default, args[2]
    elseif length(args) == 3
        return args
    else
        error("incorrect number of arguments to a contextual definition macros")
    end
end

macro Box(args...)
    error("cannot use @Box macro outside of the scope of Cassette's contextual macros (@execute, @execution, @isprimitive, @primitive, @prehook)")
end

isboxmacrocall(x) = isa(x, Expr) && x.head == :macrocall && x.args[1] == Symbol("@Box")

function typify_sig(ex)
    ex isa Expr && ex.head == :where &&
      return Expr(:where, typify_sig(ex.args[1]), ex.args[2:end]...)
    @assert ex.head == :call
    f = ex.args[1]
    if !(f isa Expr && f.head == :(::))
        f = :(::typeof($f))
    end
    return :($f($(ex.args[2:end]...)))
end

function contextual_transform!(ctx, meta, f, method)
    @assert is_method_definition(method)
    signature, body = method.args
    return contextual_transform!(ctx, meta, f, signature, body)
end

function contextual_transform!(ctx, meta, f, signature::Expr, body::Expr)
    @assert is_valid_ctx_specification(ctx) "invalid context specifier: $ctx. Valid syntax is `ContextType` or `context_name::ContextType`."

    signature = typify_sig(signature)

    if signature.head != :where
        signature = Expr(:where, signature)
    end

    ctxtypevar = gensym("ContextTypeVar")
    if isa(ctx, Expr) && ctx.head == :(::)
        ctxtype = last(ctx.args)
        ctx.args[end] = ctxtypevar
    else
        ctxtype = ctx
        ctx = :(::$(ctxtypevar))
    end
    push!(signature.args, :($ctxtypevar <: $ctxtype))

    worldtypevar = gensym("world")
    push!(signature.args, :($worldtypevar))
    world = :(::Val{$worldtypevar})

    callargs = signature.args[1].args
    for i in 1:length(callargs)
        x = callargs[i]
        if isa(x, Expr) && x.head == :(::)
            xtype = last(x.args)
            if isboxmacrocall(xtype)
                boxargs = xtype.args[3:end]
                if isempty(boxargs)
                    U, M = :Any, :Any
                elseif length(boxargs) == 1
                    U, M = first(boxargs), :Any
                elseif length(boxargs) == 2
                    U, M = boxargs
                else
                    error("incorrect usage of `@Box`: $(xtype)")
                end
                new_xtype = :($Cassette.Box{$ctxtypevar,<:$U,<:Any,<:$M,true})
            else
                new_xtype = :(Union{$Cassette.Box{<:Any,<:$xtype},$xtype})
            end
            x.args[end] = new_xtype
        end
    end

    signature.args[1] = Expr(:call, f, world, ctx, meta, callargs...)

    pushfirst!(body.args, Expr(:meta, :inline))

    return esc(Expr(:function, signature, body))
end

function is_valid_ctx_specification(x)
    if isa(x, Expr)
        T = x.head == :(::) ? last(x.args) : x
        return is_valid_ctx_type(T)
    end
    return isa(x, Symbol)
end

is_valid_ctx_type(x::Symbol) = true
is_valid_ctx_type(x) = isa(x, Expr) && x.head == :(.) && is_valid_ctx_type(unquote(last(x.args)))
