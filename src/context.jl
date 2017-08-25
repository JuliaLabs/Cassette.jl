#######
# Tag #
#######
# Note that the code/comments here were originally used for ForwardDiff's tagging system,
# and have been modified from their original version to fit your TV - er, to fit Cassette's
# use case.

struct Tag{C,T} end

# Here, we could've just as easily used `hash`; however, this is unsafe/undefined behavior
# if `hash(::Type{F})` is overloaded in a module loaded after Cassette. Thus, we instead
# use `hash(Symbol(F))`, which is somewhat safer since it's far less likely that somebody
# would overwrite the Base definition for `Symbol(::DataType)` or `hash(::Symbol)`.
@generated function Tag(::Val{C}, ::F) where {C,F}
    @assert isa(C, Symbol)
    T = hash(Symbol(F))
    return quote
        $(Expr(:meta, :inline))
        Tag{C,$T}()
    end
end

###########################
# CtxCall/CtxVar #
###########################

abstract type CtxCall{C,T,F} end

struct CtxVar{C,T,V,M,U}
    tag::Tag{C,T}
    value::U
    meta::M
    @inline function CtxVar(tag::Tag{C,T}, value::V, meta::M) where {C,T,V,M}
        new{C,T,V,M,V}(ctx.tag, value, meta)
    end
    @inline function CtxVar(tag::Tag{C,T}, value::Type{V}, meta::M) where {C,T,V,M}
        new{C,T,Type{V},M,Type{V}}(ctx.tag, value, meta)
    end
    @inline function CtxVar(tag::Tag{C,T}, value::CtxVar{<:Any,<:Any,V}, meta::M) where {C,T,V,M}
        new{C,T,V,M,typeof(value)}(ctx.tag, value, meta)
    end
end

@inline CtxVar(ctx::CtxCall, value, meta = nothing) = CtxVar(ctx.tag, value, meta)

@inline unwrap(x) = x
@inline unwrap(ctx::CtxCall) = ctx.func
@inline unwrap(::Type{CC}) where {C,T,F,CC<:CtxCall{C,T,F}} = F

@inline unwrap(ctx, arg) = arg
@inline unwrap(::CtxCall{C,T}, arg::CtxVar{C,T}) where {C,T} = arg.value
@inline unwrap(::Type{CC}, ::Type{CV}) where {C,T,V,M,U,CC<:CtxCall{C,T},CV<:CtxVar{C,T,V,M,U}} = U

@inline meta(ctx, arg) = nothing
@inline meta(::CtxCall{C,T}, arg::CtxVar{C,T}) where {C,T} = arg.meta
@inline meta(::Type{CC}, ::Type{CV}) where {C,T,V,M,CC<:CtxCall{C,T},CV<:CtxVar{C,T,V,M}} = U

@generated function ctxcall(f, ctx::CtxCall, args...)
    args = [:(unwrap(ctx, args[$i])) for i in 1:nfields(args)]
    return quote
        $(Expr(:meta, :inline))
        f($(args...))
    end
end

@inline hasctxcall(f, g, ::CtxCall, arg::Any) = g(arg)
@inline hasctxcall(f, g, ctx::CtxCall{C,T}, arg::CtxVar{C,T}) where {C,T} = f(unwrap(ctx, arg), meta(ctx, arg))

@inline hasctx(::CtxCall, ::Any) = false
@inline hasctx(::Type{<:CtxCall}, ::Type{<:Any}) = false
@inline hasctx(::CtxCall{C,T}, ::CtxVar{C,T}) where {C,T} = true
@inline hasctx(::Type{CC}, ::Type{CV}) where {C,T,CC<:CtxCall{C,T},CV<:CtxVar{C,T}} = true

# these stubs get overloaded by Cassette's various macros
function _wrap end
function _isprimitive end
function _hook end

##################
# @context macro #
##################

macro context(Ctx)
    @assert isa(Ctx, Symbol) "context name must be a Symbol"
    seed = Val(Ctx)
    ctxsym = Expr(:quote, Ctx)
    return esc(quote
        # define the actual context type
        struct $Ctx{T,F} <: $Cassette.CtxCall{$ctxsym,T,F}
            tag::$Cassette.Tag{$ctxsym,T}
            func::F
            @inline $Ctx(tag::$Cassette.Tag{$ctxsym,T}, func::F) where {T,F} = new{T,F}(tag, func)
            @inline $Ctx(tag::$Cassette.Tag{$ctxsym,T}, func::Type{F}) where {T,F} = new{T,Type{F}}(tag, func)
            @inline $Ctx(tag::$Cassette.Tag{$ctxsym,T}, func::$Cassette.CtxCall) where {T} = error("cannot nest contexts without an Intercept barrier")
        end

        @inline $Ctx(f) = $Ctx($Cassette.Tag($seed, f), f)

        # define the context's tag propagation wrapper
        @inline $Cassette._wrap(ctx::$Ctx, f::F) where {F} = $Ctx(ctx.tag, f)

        # define the context's hook function fallback
        @inline $Cassette._hook(ctx::$Ctx, args...) = nothing

        # define the context's isprimitive function fallback
        @inline $Cassette._isprimitive(ctx::$Ctx, args...) = Val(false)

        # define fallback execution behavior
        $Cassette.@contextual $Ctx @ctx(f)(args...) = $Cassette.ctxcall(unwrap(f), f, args...)
    end)
end

#####################
# @contextual macro #
#####################

macro contextual(ctx, def)
    contextual_method_transform!(ctx, def)
    return esc(def)
end

macro ctx(args...)
    error("cannot use @ctx macro outside of the scope of @contextual, @isprimitive, ",
          "@primitive or @hook method definitions.")
end

###############
# @hook macro #
###############

# passing world age here forces recompilation
@inline hook(::Val{world}, ctx::CtxCall, args...) where {world} = _hook(ctx, args...)

macro hook(ctx, def)
    contextual_method_transform!(ctx, def)
    switch_callable_with_method_name!(first(def.args), :($Cassette._hook))
    return esc(def)
end

######################
# @isprimitive macro #
######################

# passing world age here forces recompilation
@generated function isprimitive(::Val{world}, ctx::C, args...) where {world, C<:CtxCall}
    F = unwrap(C)
    if F.name.module == Core || F <: Core.Builtin
        body = :(Val(true))
    else
        body = :($Cassette._isprimitive(ctx, args...))
    end
    return quote
        $(Expr(:meta, :inline))
        $(body)
    end
end

macro isprimitive(ctx, signature)
    contextual_signature_transform!(ctx, signature)
    switch_callable_with_method_name!(signature, :($Cassette._isprimitive))
    body = Expr(:block)
    push!(body.args, Expr(:meta, :inline))
    push!(body.args, :(return Val(true)))
    return esc(Expr(:function, signature, body))
end

####################
# @primitive macro #
####################

macro primitive(ctx, def)
    signature = deepcopy(first(def.args))
    return esc(quote
        $Cassette.@contextual $ctx $def
        $Cassette.@isprimitive $ctx $signature
    end)
end

####################################
# context-specific macro utilities #
####################################

is_ctx_dispatch(x) = isa(x, Expr) && x.head == :macrocall && x.args[1] == Symbol("@ctx")

is_non_ctx_dispatch(x) = isa(x, Expr) && x.head == :(::) && !is_ctx_dispatch(x)

function parse_ctx_dispatch(x)
    @assert is_ctx_dispatch(x) "encountered malformed @ctx syntax"
    e = x.args[3]
    if isa(e, Symbol)
        v, V = e, :Any
    elseif isa(e, Expr) && e.head == :(::)
        if length(e.args) == 1
            v, V = nothing, e.args[1]
        else
            @assert length(e.args) == 2 "encountered malformed @ctx syntax"
            v, V = e.args
        end
    end
    return v, V
end

function transform_ctx_arg_dispatch(v, V, ctx::Symbol, tag)
    qctx = Expr(:quote, ctx)
    v === nothing && V === :Any && return :(::$Cassette.CtxVar{$qctx,$tag})
    v === nothing && return :(::$Cassette.CtxVar{$qctx,$tag,<:$V})
    V === :Any && return :($v::$Cassette.CtxVar{$qctx,$tag})
    return :($v::$Cassette.CtxVar{$qctx,$tag,<:$V})
end

function transform_ctx_func_dispatch(v, V, ctx, tag)
    v === nothing && V === :Any && return :(::$ctx{$tag})
    v === nothing && return :(::$ctx{$tag,<:$V})
    V === :Any && return :($v::$ctx{$tag})
    return :($v::$ctx{$tag,<:$V})
end

function contextual_signature_transform!(ctx, signature)
    callable = extract_callable_from_signature(signature)
    ctxname = extract_unqualified_symbol(ctx)
    @assert is_ctx_dispatch(callable) "the signature's method identifier must be wrapped in @ctx"

    # add the tag parameter to the signature's `where` clause
    tag = gensym("TagTypeVar")
    add_type_variable!(signature, tag)

    # replace context syntax with contextualized type
    v, V = parse_ctx_dispatch(callable)
    replace_signature_callable!(signature, transform_ctx_func_dispatch(v, V, ctx, tag))

    argslist = extract_args_from_signature(signature)
    for i in eachindex(argslist)
        x = argslist[i]
        if is_non_ctx_dispatch(x)
            i = length(x.args)
            (i == 1 || i == 2) || error("failed to parse dispatch syntax in subexpression ", x)
            V = x.args[i]
            x.args[i] = :(Union{$Cassette.CtxVar{<:Any,<:Any,$V},$V})
        elseif is_ctx_dispatch(x)
            v, V = parse_ctx_dispatch(x)
            argslist[i] = transform_ctx_arg_dispatch(v, V, ctxname, tag)
        end
    end
    return signature
end

function contextual_method_transform!(ctx, def)
    @assert is_method_definition(def)
    signature, body = def.args

    contextual_signature_transform!(ctx, signature)

    # make sure the user didn't mistakenly use context syntax in the method body
    replace_match!(is_ctx_dispatch, body) do x
        error("@ctx can only be used in the method signature, not body! ",
              "Encountered @ctx in method body subexpression: ", x)
    end

    # force inlining
    unshift!(def.args[2].args, Expr(:meta, :inline))

    return def
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

function extract_unqualified_symbol(e::Expr)
    @assert e.head == :(.)
    return extract_unqualified_symbol(last(e.args))
end

extract_unqualified_symbol(name::Symbol) = name

function extract_args_from_signature(sig)
    if sig.head == :where
        sig = sig.args[1]
    end
    return view(sig.args, 2:length(sig.args))
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
