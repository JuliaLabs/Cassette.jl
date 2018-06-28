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

struct Meta{D,M#=<:Union{Tuple,NamedTuple,Array,ModuleMeta}=#}
    data::Union{D,NoMetaData}
    meta::Union{M,NoMetaMeta}
end

Meta(data::D, meta::M) where {D,M} = Meta{D,M}(data, meta)

const NOMETA = Meta(NoMetaData(), NoMetaMeta())

# These defined to allow conversion of `Meta{NoMetaData,NoMetaMeta}`
# into whatever metatype is expected by a container.
Base.convert(::Type{M}, meta::M) where {M<:Meta} = meta
Base.convert(::Type{Meta{D,M}}, meta::Meta) where {D,M} = Meta{D,M}(meta.data, meta.meta)

################
# `ModuleMeta` #
################
# note that `BindingMeta` was defined earlier in src/context.jl

struct ModuleMeta{D,M}
    name::Meta{D,M}
    bindings::BindingMetaDict
end

# TODO: For fast methods (~ns), this fetch can cost drastically more than the primal method
# invocation. We easily have the module at compile time, but we don't have access to the
# actual context object. This `@pure` is vtjnash-approved. It should allow the compiler to
# optimize away the fetch once we have support for it, e.g. loop invariant code motion.
Base.@pure @noinline function fetch_tagged_module(context::Context, m::Module)
    bindings = get!(() -> BindingMetaDict(), context.bindings, m)
    return Tagged(context.tag, m, Meta(NoMetaData(), ModuleMeta(NOMETA, bindings)))
end

Base.@pure @noinline function _fetch_binding_meta!(context::Context,
                                                   m::Module,
                                                   bindings::BindingMetaDict,
                                                   name::Symbol)
    return get!(bindings, name) do
        if isdefined(m, name)
            return BindingMeta(initmeta(context, getfield(m, name)))
        else
            # If this case occurs, it means there must be a context-observable assigment to
            # the primal binding before we can access it again. This is okay because a) it's
            # obvious that the primal program can't access bindings without defining them,
            # and b) we already don't allow non-context-observable assignments that could
            # invalidate the context's metadata.
            return BindingMeta()
        end
    end
end

function fetch_binding_meta!(context::Context,
                             m::Module,
                             bindings::BindingMetaDict,
                             name::Symbol,
                             primal)
    M = metatype(typeof(context), typeof(primal))
    return convert(M, _fetch_binding_meta!(context, m, bindings, name).data)::M
end

############################
# `metatype` specification #
############################

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
implementation returns a still-correct, but extremely pessimistic metatype with the
benefit that metatype computation is very fast. If, in the future, `metatype` is
parameterized on world age, then we can call `subtypes` at compile time, and compute a more
optimally bounded metatype.
=#
function metatype(::Type{C}, ::Type{T}) where {C<:Context,T}
    if isconcretetype(T)
        return Meta{metadatatype(C, T),metametatype(C, T)}
    end
    return Meta
end

#=== metadatatype ===#

metadatatype(::Type{<:Context}, ::DataType) = NoMetaData

#=== metametatype ===#

@generated function metametatype(::Type{C}, ::Type{T}) where {C<:Context,T}
    if !(isconcretetype(T))
        return :(error("cannot call metametatype on non-concrete type ", $T))
    end
    if fieldcount(T) == 0
        body = :(NoMetaMeta)
    else
        F = isimmutable(T) ? :Immutable : :Mutable
        ftypes = [:($F{metatype(C, $(fieldtype(T, i)))}) for i in 1:fieldcount(T)]
        tuplemetatype = :(Tuple{$(ftypes...)})
        if T <: Tuple
            body = tuplemetatype
        else
            fnames = Expr(:tuple, map(Base.Meta.quot, fieldnames(T))...)
            body = :(NamedTuple{$fnames,$tuplemetatype})
        end
    end
    return quote
        $(Expr(:meta, :inline))
        $body
    end
end

@generated function metametatype(::Type{C}, ::Type{T}) where {C<:Context,T<:Array}
    return quote
        $(Expr(:meta, :inline))
        Array{metatype(C, $(eltype(T))),$(ndims(T))}
    end
end

@inline function metametatype(::Type{C}, ::Type{Module}) where {C<:Context}
    return ModuleMeta{metadatatype(C, Symbol), metametatype(C, Symbol)}
end

#=== initmetameta ===#

@inline initmetameta(context::Context, value::Module) = fetch_tagged_module(context, value).meta

@inline initmetameta(context::C, value::Array{T}) where {C<:Context,T} = similar(value, metatype(C, T))

function initmetameta(context::C, value::V) where {C<:Context,V}
    if fieldcount(V) == 0
        metameta_expr = :(NoMetaMeta())
    else
        F = isimmutable(V) ? :Immutable : :Mutable
        ftypes = [:($F{metatype(C, $(fieldtype(T, i)))}) for i in 1:fieldcount(T)]
        fdatas = [:($S(NOMETA)) for S in ftypes]
        metameta_expr = Expr(:tuple, fdatas...)
        if !(T <: Tuple)
            fnames = fieldnames(V)
            for i in 1:fieldcount(V)
                metameta_expr[i] = :($(fnames[i]) = $(metameta_expr[i]))
            end
        end
    end
    return quote
        $(Expr(:meta, :inline))
        $(metameta_expr)
    end
end

#=== initmeta ===#

@inline function initmeta(context::C, value::V, metadata::D) where {C<:Context,V,D}
    return Meta{metadatatype(C, V),metametatype(C, V)}(metadata, initmetameta(context, value))
end

############
# `Tagged` #
############

#=
Here, `U` is the innermost, "underlying" type of the value being wrapped. This parameter is
precomputed so that Cassette can directly dispatch on it in signatures generated for
contextual primitives.
=#
struct Tagged{T<:Tag,U,V,D,M}
    tag::T
    value::V
    meta::Meta{D,M}
    function Tagged(tag::T, value::V, meta::Meta{D,M}) where {T<:Tag,V,D,M}
        return new{T,_underlying_type(V),V,D,M}(tag, value, meta)
    end
end

#=== `Tagged` internals ===#

_underlying_type(V::Type) = V
_underlying_type(::Type{<:Tagged{<:Tag,U}}) where {U} = U

#=== `Tagged` API ===#

function tag(value, context::Context, metadata = NoMetaData())
    return Tagged(context.tag, value, initmeta(context, value, metadata))
end

untag(x, context::Context) = untag(x, context.tag)
untag(x::Tagged{T}, tag::T) where {T<:Tag} = x.value
untag(x, ::Union{Tag,Nothing}) = x

untagtype(X::Type, ::Type{C}) where {C<:Context} = untagtype(X, tagtype(C))
untagtype(::Type{<:Tagged{T,U,V}}, ::Type{T}) where {T<:Tag,U,V} = V
untagtype(X::Type, ::Type{<:Union{Tag,Nothing}}) = X

metadata(x, context::Context) = metadata(x, context.tag)
metadata(x::Tagged{T}, tag::T) where {T<:Tag} = x.meta.data
metadata(::Any, ::Union{Tag,Nothing}) = NoMetaData()

metameta(x, context::Context) = metameta(x, context.tag)
metameta(x::Tagged{T}, tag::T) where {T<:Tag} = x.meta.meta
metameta(::Any, ::Union{Tag,Nothing}) = NoMetaMeta()

istagged(x, context::Context) = istagged(x, context.tag)
istagged(x::Tagged{T}, tag::T) where {T<:Tag} = true
istagged(::Any, ::Union{Tag,Nothing}) = false

istaggedtype(X::Type, ::Type{C}) where {C<:Context} = istaggedtype(X, tagtype(C))
istaggedtype(::Type{<:Tagged{T}}, ::Type{T}) where {T<:Tag} = true
istaggedtype(::DataType, ::Type{<:Union{Tag,Nothing}}) = false

hasmetadata(x, context::Context) = hasmetadata(x, context.tag)
hasmetadata(x, tag::Union{Tag,Nothing}) = !isa(metadata(x, tag), NoMetaData)

hasmetameta(x, context::Context) = hasmetameta(x, context.tag)
hasmetameta(x, tag::Union{Tag,Nothing}) = !isa(metameta(x, tag), NoMetaMeta)

################
# `tagged_new` #
################

@generated function tagged_new(context::C, ::Type{T}, args...) where {C<:Context,T}
    tagged_count = 0
    fields = Expr(:tuple)
    ftypes = [fieldtype(T, i) for i in 1:fieldcount(T)]
    F = isimmutable(T) ? :Immutable : :Mutable

    # build `fields` expr from `args` (destructure any metadata present in arguments)
    for i in 1:nfields(args)
        if istaggedtype(args[i], C)
            tagged_count += 1
            argmeta = :(args[$i].meta)
        else
            argmeta = :NOMETA
        end
        ftype = :(metatype(C, $(ftypes[i])))
        push!(fields.args, :($F{$ftype}($argmeta)))
    end

    if tagged_count == 0 && isbitstype(T) # no tagged args present
        # return expr constructing the new object as-is
        return quote
            $(Expr(:meta, :inline))
            $(Expr(:new, T, Expr(:..., :args)))
        end
    else # there were tagged args, so we continue
        # build the rest of the `fields` expr, if there are fields left over that were not
        # populated by the given `args` (since `new` supports 0 to N arguments for objects
        # of fieldcount N).
        for i in (nfields(args) + 1):fieldcount(T)
            ftype = :(metatype(C, $(ftypes[i])))
            push!(fields.args, :($F{$ftype}(NOMETA)))
        end
        if !(T <: Tuple) # if fields are identified by name as well as position, use the names
            fnames = fieldnames(T)
            for i in 1:fieldcount(T)
                fields.args[i] = :($(fnames[i]) = $(fields.args[i]))
            end
        end
        # return expr that constructs the new Tagged object
        untagged_args = [:(untagged(args[$i], context)) for i in 1:nfields(args)]
        return quote
            $(Expr(:meta, :inline))
            M = metatype(C, T)
            return Tagged(context,
                          $(Expr(:new, T, untagged_args...)),
                          convert(M, Meta(NoMetaData(), $fields)))
        end
    end
end

@generated function tagged_new(context::C, ::Type{T}, args...) where {C<:Context,T<:Array}
    untagged_args = [:(untagged(args[$i], context)) for i in 1:nfields(args)]
    return quote
        $(Expr(:meta, :inline))
        return tag($(T)($(untagged_args...)), context)
    end
end

@generated function tagged_new(context::C, ::Type{Module}, args...) where {C<:Context}
    if istaggedtype(args[1], C)
        return_expr = quote
            Tagged(tagged_module.tag, tagged_module.value,
                   Meta(NoMetaData(),
                        ModuleMeta(args[1].meta, tagged_module.meta.meta.bindings)))
        end
    else
        return_expr = :(tagged_module)
    end
    return quote
        $(Expr(:meta, :inline))
        new_module = Module(args...)
        tagged_module = fetch_tagged_module(context, new_module)
        return $return_expr
    end
end

#########################
# `tagged_*` primitives #
#########################

#=== tagged_nameof ===#

tagged_nameof(context::Context, x) = nameof(untag(x, context))

function tagged_nameof(context::ContextWithTag{T}, x::Tagged{T,Module}) where {T}
    name_value = nameof(x.value)
    name_meta = hasmetameta(x, context) ? x.meta.meta.name : NOMETA
    return Tagged(context.tag, name_value, name_meta)
end

#=== tagged_globalref ===#

function tagged_globalref(context::ContextWithTag{T},
                          m::Tagged{T},
                          name,
                          primal) where {T}
    if hasmetameta(m, context)
        return _tagged_globalref(context, m, name, primal)
    else
        return primal
    end
end

function _tagged_globalref(context::ContextWithTag{T},
                           m::Tagged{T},
                           name,
                           primal) where {T}
    untagged_name = untag(name, context)
    if isconst(m.value, untagged_name) && isbits(primal)
        # It's very important that this fast path exists and is taken with no runtime
        # overhead; this is the path that will be taken by, for example, access of simple
        # named function bindings.
        return primal
    else
        meta = fetch_binding_meta!(context, m.value, m.meta.meta.bindings, untagged_name, primal)
        return Tagged(context.tag, primal, meta)
    end
end

#=== tagged_global_set! ===#

# TODO: try to inline these operations into the IR so that the primal binding and meta
# binding mutations occur directly next to one another
function tagged_global_set!(context::ContextWithTag{T}, m::Tagged{T}, name::Symbol, primal) where {T}
    binding = fetch_binding!(context, m.value, m.meta.meta.bindings, name)
    meta = istagged(primal, context) ? primal.meta : NOMETA
    # this line is where the primal binding assignment should happen order-wise
    binding.meta = meta
    return nothing
end

#=== tagged_getfield ===#

tagged_getfield(context::Context, x, name) = getfield(x, untag(name, context))

function tagged_getfield(context::ContextWithTag{T}, x::Tagged{T,Module}, name) where {T}
    untagged_name = untag(name, context)
    return tagged_global_ref(context, x, untagged_name, getfield(x.value, untagged_name))
end

function tagged_getfield(context::ContextWithTag{T}, x::Tagged{T}, name) where {T}
    untagged_name = untag(name, context)
    y_value = getfield(untag(x, context), untagged_name)
    y_meta = hasmetameta(x, context) ? load(getfield(x.meta.meta, untagged_name)) : NOMETA
    return Tagged(context.tag, y_value, y_meta)
end

#=== tagged_setfield! ===#

tagged_setfield!(context::Context, x, name, y) = setfield!(x, untag(name, context), y)

function tagged_setfield!(context::ContextWithTag{T}, x::Tagged{T}, name, y) where {T}
    untagged_name = untag(name, context)
    y_value = untag(y, context)
    y_meta = istagged(y, context) ? y.meta : NOMETA
    setfield!(x.value, untagged_name, y_value)
    store!(getfield(x.meta.meta, untagged_name), y_meta)
    return y
end

#=== tagged_arrayref ===#

function tagged_arrayref(context::Context, boundscheck, x, i)
    return Core.arrayref(untag(boundscheck, context), x, untag(i, context))
end

function tagged_arrayref(context::ContextWithTag{T}, boundscheck, x::Tagged{T}, i) where {T}
    untagged_boundscheck = untag(boundscheck, context)
    untagged_i = untag(i, context)
    y_value = Core.arrayref(untagged_boundscheck, untag(x, context), untagged_i)
    y_meta = hasmetameta(x, context) ? Core.arrayref(untagged_boundscheck, x.meta.meta, untagged_i) : NOMETA
    return Tagged(context.tag, y_value, y_meta)
end

#=== tagged_arrayset ===#

function tagged_arrayset(context::Context, boundscheck, x, y, i)
    return Core.arrayset(untag(boundscheck, context), x, y, untag(i, context))
end

function tagged_arrayset(context::ContextWithTag{T}, boundscheck, x::Tagged{T}, y, i) where {T}
    untagged_boundscheck = untag(boundscheck, context)
    untagged_i = untag(i, context)
    y_value = untag(y, context)
    y_meta = istagged(y, context) ? y.meta : NOMETA
    Core.arrayset(untagged_boundscheck, untag(x, context), y_value, untagged_i)
    Core.arrayset(untagged_boundscheck, x.meta.meta, y_meta, untagged_i)
    return x
end

#=== tagged_growbeg! ===#

tagged_growbeg!(context::Context, x, delta) = Base._growbeg!(x, untag(delta, context))

function tagged_growbeg!(context::ContextWithTag{T}, x::Tagged{T}, delta) where {T}
    untagged_delta = untag(delta, context)
    Base._growbeg!(x.value, delta_untagged)
    hasmetameta(x, context) && Base._growbeg!(x.meta.meta, delta_untagged)
    return nothing
end

#=== tagged_growend! ===#

tagged_growend!(context::Context, x, delta) = Base._growend!(x, untag(delta, context))

function tagged_growend!(context::ContextWithTag{T}, x::Tagged{T}, delta) where {T}
    untagged_delta = untag(delta, context)
    Base._growend!(x.value, delta_untagged)
    hasmetameta(x, context) && Base._growend!(x.meta.meta, delta_untagged)
    return nothing
end

#=== tagged_growat! ===#

function tagged_growat!(context::Context, x, i, delta)
    return Base._growat!(x, untag(i, context), untag(delta, context))
end

function tagged_growat!(context::ContextWithTag{T}, x::Tagged{T}, i, delta) where {T}
    i_untagged = untag(i, context)
    delta_untagged = untag(delta, context)
    Base._growat!(x.value, i_untagged, delta_untagged)
    hasmetameta(x, context) && Base._growat!(x.meta.meta, i_untagged, delta_untagged)
    return nothing
end

#=== tagged_deletebeg! ===#

tagged_deletebeg!(context::Context, x, delta) = Base._deletebeg!(x, untag(delta, context))

function tagged_deletebeg!(context::ContextWithTag{T}, x::Tagged{T}, delta) where {T}
    untagged_delta = untag(delta, context)
    Base._deletebeg!(x.value, delta_untagged)
    hasmetameta(x, context) && Base._deletebeg!(x.meta.meta, delta_untagged)
    return nothing
end

#=== tagged_deleteend! ===#

tagged_deleteend!(context::Context, x, delta) = Base._deleteend!(x, untag(delta, context))

function tagged_deleteend!(context::ContextWithTag{T}, x::Tagged{T}, delta) where {T}
    untagged_delta = untag(delta, context)
    Base._deleteend!(x.value, delta_untagged)
    hasmetameta(x, context) && Base._deleteend!(x.meta.meta, delta_untagged)
    return nothing
end

#=== tagged_deleteat! ===#

function tagged_deleteat!(context::Context, x, i, delta)
    return Base._deleteat!(x, untag(i, context), untag(delta, context))
end

function tagged_deleteat!(context::ContextWithTag{T}, x::Tagged{T}, i, delta) where {T}
    i_untagged = untag(i, context)
    delta_untagged = untag(delta, context)
    Base._deleteat!(x.value, i_untagged, delta_untagged)
    hasmetameta(x, context) && Base._deleteat!(x.meta.meta, i_untagged, delta_untagged)
    return nothing
end
