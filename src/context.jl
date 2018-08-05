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
