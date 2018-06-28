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
const BindingMetaCache = IdDict{Module,BindingMetaDict}

#############
# `Context` #
#############

struct Context{N<:AbstractContextName,
               M<:Any,
               P<:AbstractPass,
               T<:Union{Nothing,Tag},
               B<:Union{Nothing,BindingMetaCache}}
    name::N
    metadata::M
    pass::P
    tag::T
    bindings::B
    function Context(name::N, metadata::M, pass::P, ::Nothing, ::Nothing) where {N,M,P}
        return new{N,M,P,Nothing,Nothing}(name, metadata, pass, nothing, nothing)
    end
    function Context(name::N, metadata::M, pass::P, tag::Tag{N}, bindings::BindingMetaCache) where {N,M,P}
        return new{N,M,P,typeof(tag),BindingMetaCache}(name, metadata, pass, tag, bindings)
    end
end

function Context(name::AbstractContextName; metadata = nothing, pass::AbstractPass = NoPass())
    return Context(name, metadata, pass, nothing, nothing)
end

function similarcontext(context::Context;
                        metadata = context.metadata,
                        pass = context.pass,
                        tag = context.tag,
                        bindings = context.bindings)
    return Context(context.name, metadata, pass, tag, bindings)
end

const ContextWithTag{T} = Context{<:AbstractContextName,<:Any,<:AbstractPass,T}
const ContextWithPass{P} = Context{<:AbstractContextName,<:Any,P}

has_tagging_enabled(::Type{<:ContextWithTag{<:Tag}}) = true
has_tagging_enabled(::Type{<:ContextWithTag{Nothing}}) = false

tagtype(::C) where {C<:Context} = tagtype(C)
tagtype(::Type{<:ContextWithTag{T}}) where {T} = T

function withtagfor(context::Context, f)
    return similarcontext(context;
                          tag = Tag(typeof(context), typeof(f)),
                          bindings = BindingMetaCache())
end
