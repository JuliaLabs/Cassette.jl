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

############
# @execute #
############

macro execute(args...)
    isdebug = last(args) == :debug
    args = isdebug ? args[1:end-1] : args
    ctx, meta, call = unpack_contextual_macro_args(nothing, args...)
    @assert isa(call, Expr) && call.head == :call
    ctxsym = gensym("context")
    f = call.args[1]
    settings = isdebug ? :($Cassette.Settings($ctxsym, $meta, Cassette.World(), Val(true))) : :($Cassette.Settings($ctxsym, $meta))
    call.args[1] = :($Cassette.Overdub($(Execute()), $f, $settings))
    replace_match!(x -> :($Cassette.Wrapper($ctxsym, $(x.args[3:end]...))), iswrappermacrocall, call.args)
    return esc(:($ctxsym = $ctx($f); $call))
end

#########
# @hook #
#########

macro hook(args...)
    ctx, meta, def = unpack_contextual_macro_args(:(::Any), args...)
    return contextual_transform!(ctx, meta, :($Cassette._hook), def)
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

macro Wrapper(args...)
    error("cannot use @Wrapper macro outside of the scope of Cassette's contextual macros (@execute, @execution, @isprimitive, @primitive, @hook)")
end

iswrappermacrocall(x) = isa(x, Expr) && x.head == :macrocall && x.args[1] == Symbol("@Wrapper")

function contextual_transform!(ctx, meta, f, method)
    @assert is_method_definition(method)
    signature, body = method.args
    return contextual_transform!(ctx, meta, f, signature, body)
end

function contextual_transform!(ctx, meta, f, signature::Expr, body::Expr)
    @assert is_valid_ctx_specification(ctx) "invalid context specifier: $ctx. Valid syntax is `ContextType` or `context_name::ContextType`."

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
    world = :(::$Cassette.World{$worldtypevar})

    callargs = signature.args[1].args
    for i in 1:length(callargs)
        x = callargs[i]
        if isa(x, Expr) && x.head == :(::)
            xtype = last(x.args)
            if iswrappermacrocall(xtype)
                wrapperargs = xtype.args[3:end]
                if isempty(wrapperargs)
                    U, M = :Any, :Any
                elseif length(wrapperargs) == 1
                    U, M = first(wrapperargs), :Any
                elseif length(wrapperargs) == 2
                    U, M = wrapperargs
                else
                    error("incorrect usage of `@Wrapper`: $(xtype)")
                end
                new_xtype = :($Cassette.Wrapper{$ctxtypevar,<:$U,<:Any,$Cassette.Active,<:$M})
            else
                new_xtype = :(Union{$Cassette.Wrapper{<:Any,<:$xtype},$xtype})
            end
            x.args[end] = new_xtype
        end
    end

    signature.args[1] = Expr(:call, f, world, ctx, meta, callargs...)

    unshift!(body.args, Expr(:meta, :inline))

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
