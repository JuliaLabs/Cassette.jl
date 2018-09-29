##################
# `AbstractPass` #
##################

abstract type AbstractPass end

struct NoPass <: AbstractPass end

(::Type{NoPass})(::Any, ::Any, code_info) = code_info

#########
# `Tag` #
#########

# this @pure annotation has official vtjnash approval :p
Base.@pure _pure_objectid(x) = objectid(x)

abstract type AbstractContextName end

struct Tag{N<:AbstractContextName,X,E#=<:Union{Nothing,Tag}=#} end

Tag(::Type{N}, ::Type{X}) where {N,X} = Tag(N, X, Nothing)

Tag(::Type{N}, ::Type{X}, ::Type{E}) where {N,X,E} = Tag{N,_pure_objectid(X),E}()

#################
# `BindingMeta` #
#################
# We define these here because we need them to define `Context`,
# but most code that works with these types is in src/tagged.jl

mutable struct BindingMeta
    data::Any
    BindingMeta() = new()
end

const BindingMetaDict = Dict{Symbol,BindingMeta}
const BindingMetaDictCache = IdDict{Module,BindingMetaDict}

#############
# `Context` #
#############

"""
```
Context{N<:Cassette.AbstractContextName,
        M<:Any,
        P<:Cassette.AbstractPass,
        T<:Union{Nothing,Cassette.Tag},
        B<:Union{Nothing,Cassette.BindingMetaDictCache}}
```

A type representing a Cassette execution context. This type is normally interacted with
through type aliases constructed via `Cassette.@context`:

```
julia> Cassette.@context MyCtx
Cassette.Context{nametype(MyCtx),M,P,T,B} where B<:Union{Nothing,IdDict{Module,Dict{Symbol,BindingMeta}}}
                                          where P<:Cassette.AbstractPass
                                          where T<:Union{Nothing,Tag}
                                          where M
```

# Constructors

Given a context type alias named e.g. `MyCtx`, an instance of the type can be constructed via:

```
MyCtx(; metadata = nothing, pass = Cassette.NoPass())
```

To construct a new context instance using an existing context instance as a template, see
the `similarcontext` function.

To enable contextual tagging for a given context instance, see the `enabletagging` function.

# Fields

- `name::N<:Cassette.AbstractContextName`: a parameter used to disambiguate different
    contexts for overloading purposes (e.g. distinguishes `MyCtx` from other `Context` type
    aliases).

- `metadata::M<:Any`: trace-local metadata as provided to the context constructor

- `pass::P<:Cassette.AbstractPass`: the Cassette pass that will be applied to all method
    bodies encountered during contextual execution (see the `@pass` macro for details).

- `tag::T<:Union{Nothing,Tag}`: the tag object that is attached to values when they are
    tagged w.r.t. the context instance

- `bindingscache::B<:Union{Nothing,BindingMetaDictCache}}`: storage for metadata associated
    with tagged module bindings
"""
struct Context{N<:AbstractContextName,
               M<:Any,
               P<:AbstractPass,
               T<:Union{Nothing,Tag},
               B<:Union{Nothing,BindingMetaDictCache}}
    name::N
    metadata::M
    pass::P
    tag::T
    bindingscache::B
    function Context(name::N, metadata::M, pass::P, ::Nothing, ::Nothing) where {N,M,P}
        return new{N,M,P,Nothing,Nothing}(name, metadata, pass, nothing, nothing)
    end
    function Context(name::N, metadata::M, pass::P, tag::Tag{N}, bindingscache::BindingMetaDictCache) where {N,M,P}
        return new{N,M,P,typeof(tag),BindingMetaDictCache}(name, metadata, pass, tag, bindingscache)
    end
end

const ContextWithTag{T} = Context{<:AbstractContextName,<:Any,<:AbstractPass,T}
const ContextWithPass{P} = Context{<:AbstractContextName,<:Any,P}

function Context(name::AbstractContextName; metadata = nothing, pass::AbstractPass = NoPass())
    return Context(name, metadata, pass, nothing, nothing)
end

"""
```
similarcontext(context::Context;
               metadata = context.metadata,
               pass = context.pass,
               tag = context.tag,
               bindingscache = context.bindingscache)
```

Return a copy of the given `context`, replacing field values in the returned instance with
those provided via the keyword arguments.
"""
function similarcontext(context::Context;
                        metadata = context.metadata,
                        pass = context.pass,
                        tag = context.tag,
                        bindingscache = context.bindingscache)
    return Context(context.name, metadata, pass, tag, bindingscache)
end

"""
```
enabletagging(context::Cassette.Context, f)
```

Return a copy of the given `context` with the tagging system enabled for the contextual
execution of `f`.

Cassette uses the type of `f` to generate the `tag` field of the returned instance.

Note that it is generally unsafe to use the returned instance to contextually execute
functions other than `f`. Specifically, in cases of nested contextual execution where
both inner and outer contexts employ the tagging system, improper application of the
tagged system could cause (for example) separate contexts to erroneously interfere with
each other's metadata propagation.

See also: [`hastagging`](@ref)
"""
function enabletagging(context::Context, f)
    return similarcontext(context;
                          tag = Tag(typeof(context.name), typeof(f)),
                          bindingscache = BindingMetaDictCache())
end

"""
```
hastagging(::Type{<:Cassette.Context})
```

Returns `true` if the given type indicates that the contextual tagging system is enabled
for context instances of the type, returns `false` otherwise.

# Example

```
julia> Cassette.@context MyCtx;

julia> ctx = MyCtx();

julia> Cassette.hastagging(typeof(ctx))
false

julia> ctx = Cassette.enabletagging(ctx, sum);

julia> Cassette.hastagging(typeof(ctx))
true
```

See also: [`enabletagging`](@ref)
"""
hastagging(::Type{<:ContextWithTag{<:Tag}}) = true
hastagging(::Type{<:ContextWithTag{Nothing}}) = false

tagtype(::C) where {C<:Context} = tagtype(C)
tagtype(::Type{<:ContextWithTag{T}}) where {T} = T

nametype(::Type{<:Context{N}}) where {N} = N

############
# @context #
############

"""
```
Cassette.@context Ctx
```

Define a new Cassette context type with the name `Ctx`. In reality, `Ctx` is simply a type
alias for `Cassette.Context{Cassette.nametype(Ctx)}`.

Note that `Cassette.execute` is automatically overloaded w.r.t. `Ctx` to define several
primitives by default. A full list of these default primitives can be obtained by running:

```
methods(Cassette.execute, (Ctx, Vararg{Any}))
```

Note also that many of the default primitives' signatures only match when contextual tagging
is enabled.

See also: [`Context`](@ref)
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

        @doc (@doc $Cassette.Context) $Ctx

        @inline $Cassette.execute(::C, ::$Typ($Cassette.Tag), ::Type{N}, ::Type{X}) where {C<:$Ctx,N,X} = $Cassette.Tag(N, X, $Cassette.tagtype(C))

        # TODO: There are certain non-`Core.Builtin` functions which the compiler often
        # relies upon constant propagation to infer, such as `isdispatchtuple`. Such
        # functions should generally be contextual primitives by default for the sake of
        # performance, and we should add more of them here as we encounter them.
        @inline $Cassette.execute(ctx::$Ctx, f::$Typ(Base.isdispatchtuple), T::Type) = $Cassette.fallback(ctx, f, T)
        @inline $Cassette.execute(ctx::$Ctx, f::$Typ(Base.eltype), T::Type) = $Cassette.fallback(ctx, f, T)
        @inline $Cassette.execute(ctx::$Ctx, f::$Typ(Base.convert), T::Type, t::Tuple) = $Cassette.fallback(ctx, f, T, t)
        @inline $Cassette.execute(ctx::$Ctx{<:Any,Nothing}, f::$Typ(Base.getproperty), x::Any, s::Symbol) = $Cassette.fallback(ctx, f, x, s)

        # the below primitives are only active when the tagging system is enabled (`typeof(ctx) <: TaggedCtx`)

        @inline $Cassette.execute(ctx::C, f::$Typ($Cassette.tag), value, ::C, metadata) where {C<:$TaggedCtx} = $Cassette.fallback(ctx, f, value, ctx, metadata)
        @inline $Cassette.execute(ctx::$TaggedCtx, ::$Typ(Array{T,N}), undef::UndefInitializer, args...) where {T,N} = $Cassette.tagged_new_array(ctx, Array{T,N}, undef, args...)
        @inline $Cassette.execute(ctx::$TaggedCtx, ::$Typ(Core.Module), args...) = $Cassette.tagged_new_module(ctx, args...)
        @inline $Cassette.execute(ctx::$TaggedCtx, ::$Typ(Core.tuple), args...) = $Cassette.tagged_new_tuple(ctx, args...)
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

        $Ctx
    end)
end

###############################
# contextual dispatch methods #
###############################

struct OverdubInstead end

"""
```
prehook(context::Context, f, args...)
```

Overload this Cassette method w.r.t. a given context in order to define a new contextual
prehook for that context.

To understand when/how this method is called, see the documentation for [`overdub`](@ref).

Invoking `prehook` is a no-op by default (it immediately returns `nothing`).

See also: [`overdub`](@ref), [`posthook`](@ref), [`execute`](@ref), [`fallback`](@ref)

# Examples

Simple trace logging:

```
julia> Cassette.@context PrintCtx;

julia> Cassette.prehook(::PrintCtx, f, args...) = println(f, args)

julia> Cassette.overdub(PrintCtx(), /, 1, 2)
float(1,)
AbstractFloat(1,)
Float64(1,)
sitofp(Float64, 1)
float(2,)
AbstractFloat(2,)
Float64(2,)
sitofp(Float64, 2)
/(1.0, 2.0)
div_float(1.0, 2.0)
0.5
```

Counting the number of method invocations with one or more arguments of a given type:

```
julia> mutable struct Count{T}
           count::Int
       end

julia> Cassette.@context CountCtx;

julia> Cassette.prehook(ctx::CountCtx{Count{T}}, f, arg::T, args::T...) where {T} = (ctx.metadata.count += 1)

# count the number of calls of the form `f(::Float64, ::Float64...)`
julia> ctx = CountCtx(metadata = Count{Float64}(0));

julia> Cassette.overdub(ctx, /, 1, 2)
0.5

julia> ctx.metadata.count
2
```
"""
@inline prehook(::Context, ::Vararg{Any}) = nothing

"""
```
posthook(context::Context, output, f, args...)
```

Overload this Cassette method w.r.t. a given context in order to define a new contextual
posthook for that context.

To understand when/how this method is called, see the documentation for [`overdub`](@ref).

Invoking `posthook` is a no-op by default (it immediately returns `nothing`).

See also: [`overdub`](@ref), [`prehook`](@ref), [`execute`](@ref), [`fallback`](@ref)

# Examples

Simple trace logging:

```
julia> Cassette.@context PrintCtx;

julia> Cassette.posthook(::PrintCtx, output, f, args...) = println(output, " = ", f, args)

julia> Cassette.overdub(PrintCtx(), /, 1, 2)
1.0 = sitofp(Float64, 1)
1.0 = Float64(1,)
1.0 = AbstractFloat(1,)
1.0 = float(1,)
2.0 = sitofp(Float64, 2)
2.0 = Float64(2,)
2.0 = AbstractFloat(2,)
2.0 = float(2,)
0.5 = div_float(1.0, 2.0)
0.5 = /(1.0, 2.0)
0.5
```

Accumulate the sum of all numeric scalar outputs encountered in the trace:

```
julia> mutable struct Accum
           x::Number
       end

julia> Cassette.@context AccumCtx;

julia> Cassette.posthook(ctx::AccumCtx{Accum}, out::Number, f, args...) = (ctx.metadata.x += out)

julia> ctx = AccumCtx(metadata = Accum(0));

julia> Cassette.overdub(ctx, /, 1, 2)
0.5

julia> ctx.metadata.x
13.0
```
"""
@inline posthook(::Context, ::Vararg{Any}) = nothing

"""
```
execute(context::Context, f, args...)
```

Overload this Cassette method w.r.t. a given context in order to define a new contextual
execution primitive for that context.

To understand when/how this method is called, see the documentation for [`overdub`](@ref).

Invoking `execute` immediately returns `Cassette.OverdubInstead()` by default.

See also: [`overdub`](@ref), [`prehook`](@ref), [`posthook`](@ref), [`fallback`](@ref)
"""
@inline execute(::Context, ::Vararg{Any}) = OverdubInstead()

"""
```
fallback(context::Context, f, args...)
```

Overload this Cassette method w.r.t. a given context in order to define a new contextual
execution fallback for that context.

To understand when/how this method is called, see the documentation for [`overdub`](@ref) and
[`canoverdub`](@ref).

By default, invoking `fallback(context, f, args...)` will simply call `f(args...)` (with all
arguments automatically untagged, if `hastagging(typeof(context))`).

See also:  [`canoverdub`](@ref), [`overdub`](@ref), [`execute`](@ref), [`prehook`](@ref), [`posthook`](@ref)
"""
@inline fallback(ctx::Context, args...) = call(ctx, args...)

@inline call(::ContextWithTag{Nothing}, f, args...) = f(args...)
@inline call(context::Context, f, args...) = untag(f, context)(ntuple(i -> untag(args[i], context), Val(nfields(args)))...)

# TODO: This is currently needed to force the compiler to specialize on the type arguments
# to `Core.apply_type`. In the future, it would be best for Julia's compiler to better handle
# varargs calls to such functions with type arguments, or at least provide a better way to
# force specialization on the type arguments.
@inline call(::ContextWithTag{Nothing}, f::typeof(Core.apply_type), ::Type{A}, ::Type{B}) where {A,B} = f(A, B)
@inline call(::Context, f::typeof(Core.apply_type), ::Type{A}, ::Type{B}) where {A,B} = f(A, B)

"""
```
canoverdub(context::Context, f, args...)
```

Return `true` if `f(args...)` has a lowered IR representation that Cassette can overdub,
return `false` otherwise.

Alternatively, but equivalently:

Return `false` if `overdub(context, f, args...)` directly translates to
`fallback(context, f, args...)`, return `true` otherwise.

Note that unlike `execute`, `fallback`, etc., this function is not intended to be overloaded.

See also:  [`overdub`](@ref), [`fallback`](@ref), [`execute`](@ref)
"""
@inline canoverdub(ctx::Context, f, ::Vararg{Any}) = !isa(untag(f, ctx), Core.Builtin)
@inline canoverdub(ctx::Context, ::typeof(Core._apply), f, args...) = canoverdub(ctx, f, apply_args(ctx, args...)...)
@inline canoverdub(ctx::Context, ::typeof(Core.invoke), f, args...) = canoverdub(ctx, f, args...)
