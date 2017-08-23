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

# these stubs get overloaded by Cassette.@context
function intercept_wrap end
function wrap end

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

@inline ctxcall(f, g, ::AbstractContext, arg::Any) = g(arg)
@inline ctxcall(f, g, ctx::AbstractContext{T}, arg::AbstractCtxArg{T}) where {T} = f(value(ctx, arg), meta(ctx, arg))

@inline hasctx(::AbstractContext, ::Any) = false
@inline hasctx(::Type{<:AbstractContext}, ::Type{<:Any}) = false
@inline hasctx(::AbstractContext{T}, ::AbstractCtxArg{T}) where {T} = true
@inline hasctx(::Type{C}, ::Type{A}) where {T,C<:AbstractContext{T},A<:AbstractCtxArg{T}} = true

################################
# Context Reflection Utilities #
################################

struct ContextInfo
    mod::Module
    argument::Symbol
    hook::Symbol
    isprimitive::Symbol
end

const DEFINED_CONTEXTS = Dict{Symbol,ContextInfo}()

function lookup_context_info(key::Symbol)
    @assert haskey(DEFINED_CONTEXTS, key) "context type not defined: $key"
    return DEFINED_CONTEXTS[key]
end

function lookup_context_info(key::Expr)
    @assert key.head == :(.) "malformed context identifier: $key"
    return lookup_context_info(extract_quoted_symbol(last(key.args)))
end

##################
# @context macro #
##################

macro context(Ctx)
    @assert isa(Ctx, Symbol) "context name must be a Symbol"
    CtxArg = gensym(string(Ctx, "ArgType"))
    CtxHookFunc = gensym(string(Ctx, "_hook_func"))
    CtxIsPrimitiveFunc = gensym(string(Ctx, "_isprimitive_func"))
    haskey(DEFINED_CONTEXTS, Ctx) && error("context type ", Ctx, " is already defined")
    namesyms = [Expr(:quote, x) for x in (Ctx, CtxArg, CtxHookFunc, CtxIsPrimitiveFunc)]
    return esc(quote
        # define the actual context type
        struct $Ctx{T,F} <: $Cassette.AbstractContext{T,F}
            tag::$Cassette.Tag{T}
            func::F
            @inline $Ctx(tag::$Cassette.Tag{T}, func::F) where {T,F} = new{T,F}(tag, func)
            @inline $Ctx(tag::$Cassette.Tag{T}, func::Type{F}) where {T,F} = new{T,Type{F}}(tag, func)
            @inline $Ctx(tag::$Cassette.Tag{T}, func::$Cassette.AbstractContext) where {T} = error("cannot nest contexts without an Intercept barrier")
        end
        @inline $Ctx(f) = $Ctx($Cassette.Tag(f, Val($(Expr(:quote, Ctx)))), f)
        @inline $Cassette.intercept_wrap(ctx::$Ctx, f::F) where {F} = $Ctx(ctx.tag, f)

        # define the context's hook function fallback
        @inline $(CtxHookFunc)(ctx::$Cassette.AbstractContext, args...) = nothing

        # define the context's isprimitive function fallback
        @inline $(CtxIsPrimitiveFunc)(ctx::$Cassette.AbstractContext, args...) = Val(false)

        # define the context's argument propagator type fallback
        struct $CtxArg{T,V,M,U} <: $Cassette.AbstractCtxArg{T,V,M,U}
            tag::$Cassette.Tag{T}
            value::U
            meta::M
            @inline function $CtxArg(ctx::$Ctx{T}, value::V, meta::M) where {T,V,M}
                new{T,V,M,V}(ctx.tag, value, meta)
            end
            @inline function $CtxArg(ctx::$Ctx{T}, value::Type{V}, meta::M) where {T,V,M}
                new{T,Type{V},M,Type{V}}(ctx.tag, value, meta)
            end
            @inline function $CtxArg(ctx::$Ctx{T}, value::$Cassette.AbstractCtxArg{<:Any,V}, meta::M) where {T,V,M}
                new{T,V,M,typeof(value)}(ctx.tag, value, meta)
            end
        end
        @inline $Cassette.wrap(ctx::$Ctx, value, meta = nothing) = $CtxArg(ctx, value, meta)

        # add context to Cassette's reflection table
        $Cassette.DEFINED_CONTEXTS[$(namesyms[1])] = $Cassette.ContextInfo(@__MODULE__, $(namesyms[2:end]...))

        # define fallback primitive behavior
        @eval $Cassette.@contextual @ctx(f, $Ctx)(args...) = $Cassette.unwrapcall(f, args...)
    end)
end

#####################
# @contextual macro #
#####################

macro contextual(def)
    contextual_method_transform!(def)
    return esc(def)
end

macro ctx(args...)
    error("cannot use @ctx macro outside of the scope of @contextual, @isprimitive, ",
          "@primitive or @hook method definitions.")
end

###############
# @hook macro #
###############

@generated function hook(ctx::C, args...) where {C<:AbstractContext}
    ctxinfo = lookup_context_info(Symbol(C.name))
    return quote
        $(Expr(:meta, :inline))
        return $(ctxinfo.mod).$(ctxinfo.hook)(ctx, args...)
    end
end

macro hook(def)
    ctxinfo = contextual_method_transform!(def)
    name = :($(ctxinfo.mod).$(ctxinfo.hook))
    switch_callable_with_method_name!(first(def.args), name)
    return esc(def)
end

######################
# @isprimitive macro #
######################

@generated function isprimitive(ctx::C, args...) where {C<:AbstractContext}
    F = unwrap(C)
    if F.name.module == Core || F <: Core.Builtin
        body = :(Val(true))
    else
        ctxinfo = lookup_context_info(Symbol(C.name))
        body = :($(ctxinfo.mod).$(ctxinfo.isprimitive)(ctx, args...))
    end
    return quote
        $(Expr(:meta, :inline))
        $(body)
    end
end

macro isprimitive(signature)
    ctxinfo = contextual_signature_transform!(signature)
    name = :($(ctxinfo.mod).$(ctxinfo.isprimitive))
    switch_callable_with_method_name!(signature, name)
    body = Expr(:block)
    push!(body.args, Expr(:meta, :inline))
    push!(body.args, :(return Val(true)))
    return esc(Expr(:function, signature, body))
end

####################
# @primitive macro #
####################

macro primitive(def)
    signature = deepcopy(first(def))
    return esc(quote
        $Cassette.@contextual $def
        $Cassette.@isprimitive $signature
    end)
end

####################################
# context-specific macro utilities #
####################################

is_ctx_dispatch(x) = isa(x, Expr) && x.head == :macrocall && x.args[1] == Symbol("@ctx")

is_non_ctx_dispatch(x) = isa(x, Expr) && x.head == :(::) && !is_ctx_dispatch(x)

function parse_ctx_dispatch(x)
    @assert is_ctx_dispatch(x) "encountered malformed @ctx syntax"
    input = x.args[3:end]
    if length(input) == 1
        v, V, C = nothing, :Any, input[]
    else
        @assert length(input) == 2 "encountered malformed @ctx syntax"
        arg, C = input
        if isa(arg, Symbol)
            v, V = arg, :Any
        elseif isa(arg, Expr)
            @assert arg.head == :(::) "encountered malformed @ctx syntax"
            v, V = (length(arg.args) == 2) ? arg.args : (nothing, arg.args[])
        end
    end
    return v, V, C
end

function transform_ctx_dispatch(v, V, C, tag)
    v === nothing && V === :Any && return :(::$C{$tag})
    v === nothing && return :(::$C{$tag,<:$V})
    V === :Any && return :($v::$C{$tag})
    return :($v::$C{$tag,<:$V})
end

function contextual_signature_transform!(signature)
    callable = extract_callable_from_signature(signature)
    input = extract_args_from_signature(signature)
    @assert is_ctx_dispatch(callable)

    # add the tag parameter to the signature's `where` clause
    tag = gensym("TagTypeVar")
    add_type_variable!(signature, tag)

    # replace context syntax with contextualized type
    v, V, C = parse_ctx_dispatch(callable)
    ctxinfo = lookup_context_info(C)
    replace_signature_callable!(signature, transform_ctx_dispatch(v, V, C, tag))

    for i in eachindex(input)
        x = input[i]
        if is_non_ctx_dispatch(x)
            # replace `::V` with `::Union{AbstractCtxArg{<:Any,V},V}`
            i = length(x.args)
            (i == 1 || i == 2) || error("failed to parse dispatch syntax in subexpression ", x)
            V = x.args[i]
            x.args[i] = :(Union{$Cassette.AbstractCtxArg{<:Any,$V},$V})
        elseif is_ctx_dispatch(x)
            # replace context syntax with contextualized type
            v, V, argC = parse_ctx_dispatch(x)
            argC !== C && error("argument context type ", argC, " must equal method context type ", C)
            input[i] = transform_ctx_dispatch(v, V, ctxinfo.argument, tag)
        end
    end
    return ctxinfo
end

function contextual_method_transform!(def)
    @assert is_method_definition(def)
    signature, body = def.args

    ctxinfo = contextual_signature_transform!(signature)

    # make sure the user didn't mistakenly use context syntax in the method body
    replace_match!(is_ctx_dispatch, body) do x
        error("@ctx can only be used in the method signature, not body! ",
              "Encountered @ctx in method body subexpression: ", x)
    end

    # force inlining
    unshift!(def.args[2].args, Expr(:meta, :inline))

    return ctxinfo
end

#############################################################################
# general macro utilities leveraged in the context-specific section (above) #
#############################################################################

function is_method_definition(x)
    if isa(x, Expr)
        if x.head == :function
            return true
        elseif x.head == :(=) && isa(x.args[1], Expr)
            lhs = x.args[1]
            if lhs.head == :where
                lhs = lhs.args[1]
            end
            return lhs.head == :call
        end
    end
    return false
end

function replace_signature_callable!(sig, new_callable)
    if sig.head == :where
        sig = sig.args[1]
    end
    sig.args[1] = new_callable
    return sig
end

function extract_callable_from_signature(sig)
    if sig.head == :where
        sig = sig.args[1]
    end
    return sig.args[1]
end

function wrap_signature_callable_type!(signature, T)
    callable = extract_callable_from_signature(signature)
    i = length(callable.args)
    callable.args[i] = :($T{<:$(callable.args[i])})
    return signature
end

function extract_args_from_signature(sig)
    if sig.head == :where
        sig = sig.args[1]
    end
    return view(sig.args, 2:length(sig.args))
end

function is_quoted_symbol(x)
    if isa(x, QuoteNode)
        return true
    elseif isa(x, Expr) && x.head == :quote && length(x.args) == 1
        return isa(x.args[1], Symbol)
    end
    return false
end

function extract_quoted_symbol(x)
    if isa(x, QuoteNode)
        return x.value
    elseif isa(x, Expr)
        return x.args[1]
    elseif isa(x, Symbol)
        return x
    end
end

function add_type_variable!(signature, T)
    if isa(signature, Expr) && signature.head == :where
        push!(signature.args, T)
    else
        signature.args = Any[deepcopy(signature), T]
        signature.head = :where
    end
    return signature
end

function switch_callable_with_method_name!(signature, name)
    arglist = (signature.head == :where) ? first(signature.args).args : signature.args
    unshift!(arglist, name)
    return signature
end
