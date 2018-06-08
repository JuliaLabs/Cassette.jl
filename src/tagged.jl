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

load(x::Mutable) = x.data

store!(x::Mutable, y) = (x.data = y)

#=== `Immutable` ===#

struct Immutable{D} <: FieldStorage{D}
    data::D
    Immutable{D}(data) where D = new{D}(data)
end

load(x::Immutable) = x.data

store!(x::Immutable, y) = error("cannot mutate immutable field")

##########
# `Meta` #
##########

struct NoMetaData end
struct NoMetaMeta end

struct Meta{D,M#=<:Union{NamedTuple,Array,MetaModule}=#}
    data::Union{D,NoMetaData}
    meta::Union{M,NoMetaMeta}
end

const NOMETA = Meta(NoMetaData(), NoMetaMeta())

# These defined to allow conversion of `Meta{NoMetaData,NoMetaMeta}`
# into whatever metatype is expected by a container.
Base.convert(::Type{M}, meta::M) where {M<:Meta} = meta
Base.convert(::Type{Meta{D,F}}, meta::Meta) where {D,F} = Meta{D,F}(meta.data, meta.meta)

#=== `metatype` specification ===#

metadatatype(::Type{<:AbstractContext}, ::DataType) = NoMetaData

#=
The optimal metatype specification for a given non-leaftype is the union of the metatypes of
its subtypes, and thus can be implemented as:

```
function metatype(::Type{C}, ::Type{T}) where {C,T}
    if isconcretetype(T)
        return Meta{metadatatype(C, T),metametatype(C, T)}
    else
        return Union{map(X -> metatype(C, X), subtypes(T))...}
    end
end
```

However, this is an expensive implementation of this operation. Thus, our actual `metatype`
implementation returns a still-correct, but an extremely pessimistic metatype with the
benefit that metatype computation is very fast. If, in the future, `metaype` is
parameterized on world age, then we can call `subtypes` at compile time, and compute a more
optimally bounded dualtype.
=#
function metatype(::Type{C}, ::Type{T}) where {C<:AbstractContext,T}
    if isconcretetype(T)
        return Meta{metadatatype(C, T),metametatype(C, T)}
    end
    return Meta
end

@generated function metametatype(::Type{C}, ::Type{T}) where {C<:AbstractContext,T}
    if !(isconcretetype(T))
        return :(error("cannot call metametatype on non-concrete type ", $T))
    end
    if T <: Array
        body = :(Array{metatype(C, $(eltype(T))),$(ndims(T))})
    elseif fieldcount(T) == 0
        body = :(NoMetaMeta)
    else # TODO: handle Modules
        S = isimmutable(T) ? :Immutable : :Mutable
        fnames = Expr(:tuple, map(Base.Meta.quot, fieldnames(T))...)
        ftypes = map(F -> :($S{metatype(C, $F)}), T.types)
        body = :(NamedTuple{$fnames,Tuple{$(ftypes...)}})
    end
    return quote
        $(Expr(:meta, :inline))
        $body
    end
end

@generated function initmeta(::C, value::V, metadata::D, ::Val{isconst}) where {C<:AbstractContext,V,D,isconst}
    if isconst
        metametatype_expr = :NoMetaMeta
        metameta_expr = :(NoMetaMeta())
    else
        metametatype_expr = :(metametatype(C, V))
        # TODO do we need to call `initmeta` recursively for the mutable cases?
        if V <: Array
            metameta_expr = :(similar(value, eltype(M)))
        elseif fieldcount(V) == 0
            metameta_expr = :(NoMetaMeta())
        else # TODO: handle Modules
            S = isimmutable(V) ? :Immutable : :Mutable
            fnames = fieldnames(V)
            fdatas = map(F -> :($S{metatype(C, $F)}(NOMETA), V.types)
            metameta_expr = Expr(:tuple)
            for i in 1:fieldcount(V)
                push!(metameta_expr.args, :($(fnames[i]) = $(fdatas[i])))
            end
        end
    end
    return quote
        $(Expr(:meta, :inline))
        _D = metadatatype(C, V)
        M = $(metametatype_expr)
        Meta{_D,M}(metadata, $metameta_expr)
    end
end

############
# `Tagged` #
############

struct Tagged{T<:AbstractTag,U,V,D,M}
    tag::T
    value::V
    meta::Meta{D,M}
    function Tagged(tag::T, value::V, meta::Meta{D,M}) where {T<:AbstractTag,V,D,M}
        return new{T,_underlying_type(V),V,D,M}(tag, value, meta)
    end
end

#=== `Tagged` internals ===#

_underlying_type(::Type{V}) where {V} = V
_underlying_type(::Type{<:Tagged{<:AbstractTag,U}}) where {U} = U

#=== `Tagged` API ===#

function tag(context::AbstractContext, value, metadata = NoMetaData(), isconst = Val(false))
    return Tagged(context.tag, value, initmeta(context, value, metadata, isconst))
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

#######################
# `Module` Primitives #
#######################

#=== `MetaModule` ===#

const MetaModule = Dict{Symbol,Any}
const MetaModuleCache = IdDict{Module,Meta{NoMetaData,MetaModule}}

# TODO: We actually need to be able to call this kind of function at IR-generation time, as
# opposed to at runtime. This is because, for fast methods (~ns), this fetch can cost
# drastically more than the primal method invocation. We easily have the module at
# IR-generation time, but we don't have access to the actual context object.
#
# This `@pure` is vtjnash-approved. It should allow the compiler to do the above as an
# optimization once we have support for it, e.g. loop invariant code motion
@pure @noinline function fetch_tagged_module(context::AbstractContext, m::Module)
    return get!(context.metamodules, m) do
        Tagged(context.tag, m, Meta(NoMetaData(), MetaModule()))
    end
end

#=== `Binding` ===#

mutable struct Binding
    meta
    Binding() = new()
end

@pure @noinline function fetch_binding!(context::AbstractContext, m::Module, _m::MetaModule, name::Symbol)
    return get!(_m, name) do
        if isdefined(m, name)
            return Binding(initmeta(context, getfield(m, name)))
        else
            # If this case occurs, it means there must be a context-observable assigment to
            # the primal binding before we can access it again. This is okay because a) it's
            # obvious that the primal program can't access bindings without defining them,
            # and b) we already don't allow non-context-observable assignments that could
            # invalidate the context's metadata.
            return Binding()
        end
    end
end

function fetch_binding_meta!(context::AbstractContext, _m::MetaModule, name::Symbol, primal)
    M = metatype(typeof(context), typeof(primal))
    return convert(M, fetch_binding!(context, m, _m, name).meta)::M
end

#=== `GlobalRef` & assignment ===#

function tagged_global_ref(context::AbstractContext{T}, m::Tagged{T}, name::Symbol, primal) where {T}
    return tagged_module_load(context, m.value, m.meta.fields, name, primal)
end

function tagged_module_load(context::AbstractContext, m::Module, _m::MetaModule, name::Symbol, primal)
    if isconst(m, name) && isbits(primal)
        # It's very important that this fast path exists and is taken with no runtime
        # overhead; this is the path that will be taken by, for example, access of simple
        # named function bindings.
        return primal
    else
        return Tagged(context.tag, primal, fetch_binding_meta!(context, _m, name, primal))
    end
end

# TODO: try to inline these operations into the IR so that the primal binding and meta
# binding mutations occur directly next to one another
function tagged_global_set!(context::AbstractContext{T}, m::Tagged{T}, name::Symbol, primal) where {T}
    binding = fetch_binding!(context, m.value, m.meta.fields, name)
    meta = istagged(primal, context) ? primal.meta : NOMETA
    # this line is where the primal binding assignment should happen order-wise
    binding.meta = meta
    return nothing
end


#################
# `tagged_load` #
#################

function _tagged_load(context::AbstractContext{T}, x::Tagged{T}, y) where {T}
    return tagged_load(context, x.value, x.meta.fields, untag(y, context))
end

function tagged_load(context::AbstractContext, x::Array, _x::Array, index)
    return Tagged(context.tag, x[index], _x[index])
end

function tagged_load(context::AbstractContext, x::Module, _x::MetaModule, binding)
    return tagged_module_load(context, x, _x, binding, getfield(x, binding))
end

function tagged_load(context::AbstractContext, x, _x::NamedTuple, field)
    return Tagged(context.tag, getfield(x, field), load(getfield(_x, field)))
end

###################
# `tagged_store!` #
###################

function _tagged_store!(context::AbstractContext{T}, x::Tagged{T}, y, item) where {T}
    item_meta = istagged(item, context) ? item.meta : NOMETA
    return tagged_store!(context, x.value, x.meta.fields,
                         untag(y, context), untag(item, context),
                         item_meta)
end

function tagged_store!(context::AbstractContext, x::Array, _x::Array, index, item_value, item_meta)
    x[index] = item_value
    _x[index] = item_meta
    return nothing
end

function tagged_store!(context::AbstractContext, x::Module, _x::MetaModule, binding, item_value, item_meta)
    # Julia doesn't have a valid `setfield!`` for `Modules`, so we
    # should never run into this case in well-formed code anyway
    error("cannot assign variables in other modules")
end

function tagged_store!(context::AbstractContext, x, _x::NamedTuple, field, item_value, item_meta)
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
