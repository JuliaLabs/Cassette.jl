##################
# `FieldStorage` #
##################

abstract type FieldStorage{D} end

#=== `Mutable` ===#

mutable struct Mutable{D} <: FieldStorage{D}
    data::D
    Mutable{D}() where D = new{D}()
    Mutable{D}(data) where D = new{D}(data)
end

@inline load(x::Mutable) = x.data

@inline store!(x::Mutable, y) = (x.data = y)

#=== `Immutable` ===#

struct Immutable{D} <: FieldStorage{D}
    data::D
    Immutable{D}(data) where D = new{D}(data)
end

@inline load(x::Immutable) = x.data

@inline store!(x::Immutable, y) = error("cannot mutate immutable field")

##########
# `Meta` #
##########

struct NoMetaData end
struct NoMetaFields end

struct Meta{D,F#=<:Union{NamedTuple,Array,MetaModule}=#}
    data::Union{D,NoMetaData}
    fields::Union{F,NoMetaFields}
end

# These defined to allow conversion of `Meta(NoMetaData,NoMetaFields)`
# into whatever metatype is expected by a container.
Base.convert(::Type{M}, meta::M) where {M<:Meta} = meta
Base.convert(::Type{Meta{D,F}}, meta::Meta) where {D,F} = Meta{D,F}(meta.data, meta.fields)

# TODO metatype specification (should be relatively easy to port from the old implementation)

############
# `Tagged` #
############

struct Tagged{T<:AbstractTag,U,V,D,F}
    tag::T
    value::V
    meta::Meta{D,F}
    function Tagged(tag::T, value::V, meta::Meta{D,F}) where {T<:AbstractTag,V,D,F}
        return new{T,_underlying_type(V),V,D,F}(tag, value, meta)
    end
end

#=== `Tagged` internals ===#

_underlying_type(::Type{V}) where {V} = V
_underlying_type(::Type{<:Tagged{<:AbstractTag,U}}) where {U} = U

#=== `Tagged` API ===#

function tag(context::AbstractContext, value, metadata = NoMetaData())
    return Tagged(context.tag, value, initmeta(context, value, metadata))
end

untag(x, context::AbstractContext) = untag(x, context.tag)
untag(x::Tagged{T}, tag::T) where {T<:AbstractTag} = x.value
untag(x, tag::AbstractTag) = x

untagtype(::Type{X}, ::Type{<:AbstractContext{P,T}}) where {X,P,T} = untagtype(X, T)
untagtype(::Type{<:Tagged{T,U,V}}, ::Type{T}) where {T<:AbstractTag,U,V} = V
untagtype(::Type{X}, ::Type{<:AbstractTag}) = X

metadata(x, context::AbstractContext) = metadata(x, context.tag)
metadata(x::Tagged{T}, tag::T) where {T<:AbstractTag} = x.meta.data
metadata(x, tag::AbstractTag) = NoMetaData()

istagged(x, context::AbstractContext) = istagged(x, context.tag)
istagged(x::Tagged{T}, tag::T) where {T<:AbstractTag} = true
istagged(x, tag::AbstractTag) = false

hasmetadata(x, context::AbstractContext) = hasmetadata(x, context.tag)
hasmetadata(x, tag::AbstractTag) = isa(metadata(x, tag), NoMetaData)

################
# `tagged_new` #
################

@generated function tagged_new(context::C, ::Type{T}, args...) where {C<:AbstractContext,T}

end

@generated function tagged_new(context::C, ::Type{T}, args...) where {C<:AbstractContext,T<:Array}

end

# TODO: tagged_new for Module?

@generated function _new(::Type{T}, args...) where {T}
    return quote
        $(Expr(:meta, :inline))
        $(Expr(:new, T, [:(args[$i]) for i in 1:nfields(args)]...))
    end
end

###################################
# `Module`/`GlobalRef` Primitives #
###################################

const MetaModule = Dict{Symbol,Any}
const MetaModuleCache = IdDict{Module,Meta{NoMetaData,MetaModule}}

# TODO We actually need to be able to call this kind of function at IR-generation
# time, as opposed to at runtime. This is because, for fast methods (~ns), this fetch
# can cost drastically more than the primal method invocation. We easily have the
# module at IR-generation time, but we don't have access to the actual context
# object, implying we need to store/access this metamodule cache somewhere
# else...
function fetch_tagged_module(context::AbstractContext, m::Module)
    if haskey(context.metamodules, m)
        _m = context.metamodules[m]
    else
        _m = Meta(NoMetaData(), MetaModule())
        context.metamodules[m] = _m
    end
    return Tagged(context.tag, m, _m::Meta{NoMetaData,MetaModule})
end

function _tagged_global_ref(m::Tagged, binding::Symbol, primal)
    return tagged_global_ref(m.tag, m.value, m.meta.fields, binding, primal)
end

function tagged_global_ref(tag::AbstractTag, m::Module, _m::MetaModule, binding::Symbol, primal)
    if isconst(m, binding) && isbits(primal)
        # It's very important that this fast path exists and is taken with no runtime
        # overhead; this is the path that will be taken by, for example, access of simple
        # named function bindings.
        return primal
    else
        # TODO: create entry in _m if not already available
        # TODO: typeassert `_m[binding]`
        return Tagged(tag, primal, _m[binding])
    end
end

function _tagged_global_ref_set_meta!(m::Tagged, binding::Symbol, primal)
    # TODO
end

#################
# `tagged_load` #
#################

_tagged_load(x::Tagged, y) = tagged_load(x.tag, x.value, x.meta.fields, untag(y, x.tag))

function tagged_load(tag::AbstractTag, x::Array, _x::Array, index)
    return Tagged(tag, x[index], _x[index])
end

function tagged_load(tag::AbstractTag, x::Module, _x::MetaModule, binding)
    return tagged_global_ref(tag, x, _x, binding, getfield(x, binding))
end

function tagged_load(tag::AbstractTag, x, _x::NamedTuple, field)
    return Tagged(tag, getfield(x, field), load(getfield(_x, field)))
end

###################
# `tagged_store!` #
###################

function _tagged_store!(x::Tagged, y, item)
    item_meta = istagged(item, x.tag) ? item.meta : Meta(NoMetaData(), NoMetaFields())
    return tagged_store!(x.tag, x.value, x.meta.fields,
                         untag(y, x.tag), untag(item, x.tag),
                         item_meta)
end

function tagged_store!(tag::AbstractTag, x::Array, _x::Array, index, item_value, item_meta)
    x[index] = item_value
    _x[index] = item_meta
    return nothing
end

function tagged_store!(tag::AbstractTag, x::Module, _x::MetaModule, binding, item_value, item_meta)
    # Julia doesn't have a valid `setfield!`` for `Modules`, so we
    # should never run into this case in well-formed code anyway
    error("cannot assign variables in other modules")
end

function tagged_store!(tag::AbstractTag, x, _x::NamedTuple, field, item_value, item_meta)
    setfield!(x, field, item_value)
    store!(getfield(_x, field), item_meta)
    return nothing
end

############################
# Other `Array` Primitives #
############################
# TODO
# _growbeg!
# _growat!
# _growend!
# _deletebeg!
# _deleteat!
# _deleteend!
