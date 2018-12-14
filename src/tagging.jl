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

_metadataconvert(T, x::NoMetaData) = x
_metadataconvert(T, x) = convert(T, x)

_metametaconvert(T, x::NoMetaMeta) = x
_metametaconvert(T, x) = convert(T, x)

struct Meta{D,M#=<:Union{Tuple,NamedTuple,Array,ModuleMeta}=#}
    data::Union{D,NoMetaData}
    meta::Union{M,NoMetaMeta}
    Meta(data::D, meta::M) where {D,M} = Meta{D,M}(data, meta)
    Meta{D,M}(data, meta) where {D,M} = new{D,M}(_metadataconvert(D, data), _metametaconvert(M, meta))
end

const NOMETA = Meta(NoMetaData(), NoMetaMeta())

# These defined to allow conversion of `Meta{NoMetaData,NoMetaMeta}`
# into whatever metatype is expected by a container.
Base.convert(::Type{M}, meta::M) where {M<:Meta} = meta

function Base.convert(::Type{Meta{D,M}}, meta::Meta) where {D,M}
    metadata = _metadataconvert(D, meta.data)
    metameta = _metametaconvert(M, meta.meta)
    return Meta{D,M}(metadata, metameta)
end

################
# `ModuleMeta` #
################
# note that `BindingMeta` was defined earlier in src/context.jl

struct ModuleMeta{D,M}
    name::Meta{D,M}
    bindings::BindingMetaDict
end

Base.convert(::Type{M}, meta::M) where {M<:ModuleMeta} = meta

function Base.convert(::Type{ModuleMeta{D,M}}, meta::ModuleMeta) where {D,M}
    return ModuleMeta(convert(Meta{D,M}, meta.name), meta.bindings)
end

# TODO: For fast methods (~ns), this fetch can cost drastically more than the primal method
# invocation. We easily have the module at compile time, but we don't have access to the
# actual context object (just the type). This `@pure` is vtjnash-approved. It should allow
# the compiler to optimize away the fetch once we have support for it, e.g. loop invariant
# code motion.
Base.@pure @noinline function fetch_tagged_module(context::Context, m::Module)
    return Tagged(context, m, Meta(NoMetaData(), fetch_modulemeta(context, m)))
end

Base.@pure @noinline function fetch_modulemeta(context::Context, m::Module)
    if haskey(context.bindingscache, m)
        bindings = context.bindingscache[m]
    else
        bindings = Cassette.BindingMetaDict()
        context.bindingscache[m] = bindings
    end
    return ModuleMeta(NOMETA, bindings::BindingMetaDict)
end

Base.@pure @noinline function _fetch_bindingmeta!(context::Context,
                                                  m::Module,
                                                  bindings::BindingMetaDict,
                                                  name::Symbol)
    return get!(bindings, name) do
        bindingmeta = BindingMeta()
        # If `!(isdefined(m, name))`, there must be a context-observable assigment to
        # the primal binding before we can access it again. This is okay because a) it's
        # obvious that the primal program can't access bindings without defining them,
        # and b) we already don't allow non-context-observable assignments that could
        # invalidate the context's metadata.
        if isdefined(m, name)
            bindingmeta.data = initmeta(context, getfield(m, name), NoMetaData())
        end
        return bindingmeta
    end
end

function fetch_bindingmeta!(context::Context,
                             m::Module,
                             bindings::BindingMetaDict,
                             name::Symbol,
                             primal)
    M = metatype(typeof(context), typeof(primal))
    return convert(M, _fetch_bindingmeta!(context, m, bindings, name).data)::M
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
    if isconcretetype(T) || T <: Type
        return Meta{metadatatype(C, T),metametatype(C, T)}
    end
    return Meta
end

function _fieldtypes_for_metatype(T::Type)
    ftypes = Any[]
    for i in 1:fieldcount(T)
        # TODO improve recursion detection; this only spots 1-degree cycles right now
        ftype = fieldtype(T, i)
        ftype = ftype === T ? Any : ftype
        push!(ftypes, ftype)
    end
    return ftypes
end

doesnotneedmetatype(::Type{T}) where {T} = isbitstype(T)
doesnotneedmetatype(::Type{Symbol}) = true
doesnotneedmetatype(::Type{<:Type}) = true

doesnotneedmeta(x) = isbits(x)
doesnotneedmeta(::Symbol) = true
doesnotneedmeta(::Type) = true

#=== metadatatype ===#

"""
```
metadatatype(::Type{<:Context}, ::Type{T})
```

Overload this Cassette method w.r.t. a given context to define the type of metadata that
can be tagged to values of type `T` within that context.

By default, this method is set such that associating metadata with any tagged value is
disallowed.

Cassette uses `metadatatype` to statically compute a context-specific metadata type hiearchy
for all tagged values within overdubbed programs. To gain a mental model for this mechanism,
consider a simple struct definition as follows:

```
struct Foo
    x::Int
    y::Complex{Int}
end
```

Now, Cassette can use `metadatatype` to determine type constraints for metadata structures
associated with tagged values of type `Foo`. In psuedo-Julia-code, these metadata structures
might look something like the following for `Foo`:

```
struct IntMeta
    data::metadatatype(Ctx, Int)
    meta::Cassette.NoMetaMeta
end

struct ComplexIntMeta
    data::metadatatype(Ctx, Complex{Int})
    meta::NamedTuple{(:re,:im),Tuple{IntMeta,IntMeta}}
end

struct FooMeta
    data::metadatatype(Ctx, Foo)
    meta::NamedTuple{(:x,:y),Tuple{IntMeta,ComplexIntMeta}
end
```

# Examples

```
julia> Cassette.@context Ctx;

# any value of type `Number` can now be tagged with metadata of type `Number`
julia> Cassette.metadatatype(::Type{<:Ctx}, ::Type{<:Number}) = Number

# any value of type `T<:Number` can now be tagged with metadata of type `T`
julia> Cassette.metadatatype(::Type{<:Ctx}, ::Type{T}) where {T<:Number} = T

# any value of type `T<:Number` can now be tagged with metadata of type `promote_type(T, M)`
# where `M` is the type of the trace-local metadata associated with the context
julia> Cassette.metadatatype(::Type{<:Ctx{M}}, ::Type{T}) where {M<:Number,T<:Number} = promote_type(T, M)
```
"""
metadatatype(::Type{<:Context}, ::DataType) = NoMetaData

#=== metametatype ===#

@generated function metametatype(::Type{C}, ::Type{T}) where {C<:Context,T}
    if T <: Type || fieldcount(T) == 0
        body = :(NoMetaMeta)
    elseif !(isconcretetype(T))
        body = :(error("cannot call metametatype on non-concrete type ", $T))
    else
        F = T.mutable ? :Mutable : :Immutable
        ftypes = [:($F{metatype(C, $S)}) for S in _fieldtypes_for_metatype(T)]
        tuplemetatype = :(Tuple{$(ftypes...)})
        if T <: Tuple
            body = tuplemetatype
        else
            fnames = Expr(:tuple, map(Base.Meta.quot, fieldnames(T))...)
            body = :(NamedTuple{$fnames,$tuplemetatype})
        end
    end
    return quote
        $body
    end
end

@generated function metametatype(::Type{C}, ::Type{T}) where {C<:Context,T<:Array}
    return :(Array{metatype(C, $(eltype(T))),$(ndims(T))})
end

function metametatype(::Type{C}, ::Type{Module}) where {C<:Context}
    return ModuleMeta{metadatatype(C, Symbol), metametatype(C, Symbol)}
end

#=== initmetameta ===#

function _metametaexpr(::Type{C}, ::Type{V}, metaexprs::Vector) where {C,V}
    if V <: Type || fieldcount(V) == 0 || (all(x == :NOMETA for x in metaexprs) && doesnotneedmetatype(V))
        return :(NoMetaMeta())
    else
        F = V.mutable ? :Mutable : :Immutable
        metatypes = [:(metatype(C, $S)) for S in _fieldtypes_for_metatype(V)]
        metaconverts = [:(convert($(metatypes[i]), $(metaexprs[i]))) for i in 1:fieldcount(V)]
        metametafields = [:($F{$(metatypes[i])}($(metaconverts[i]))) for i in 1:fieldcount(V)]
        if !(V <: Tuple)
            fnames = fieldnames(V)
            for i in 1:fieldcount(V)
                metametafields[i] = :($(fnames[i]) = $(metametafields[i]))
            end
        end
        return Expr(:tuple, metametafields...)
    end
end

initmetameta(context::Context, value::Module) = fetch_modulemeta(context, value)

function initmetameta(context::C, value::Array{V}) where {C<:Context,V}
    M = metatype(C, V)
    if M <: typeof(NOMETA)
        return NoMetaMeta()
    else
        return fill!(similar(value, M), NOMETA)
    end
end

@generated function initmetameta(context::C, value::V) where {C<:Context,V}
    return quote
        $(_metametaexpr(C, V, [:NOMETA for i in 1:fieldcount(V)]))
    end
end

#=== initmeta ===#

function initmeta(context::C, value::V, metadata::D) where {C<:Context,V,D}
    return Meta{metadatatype(C, V),metametatype(C, V)}(metadata, initmetameta(context, value))
end

############
# `Tagged` #
############

struct Tagged{T<:Tag,V,D,M,C<:ContextTagged}
    context::C
    value::V
    meta::Meta{D,M}
    function Tagged(context::C, value::V, meta::Meta) where {T<:Tag,V,C<:ContextTagged{T}}
        D = metadatatype(C, V)
        M = metametatype(C, V)
        return new{T,V,D,M,C}(context, value, convert(Meta{D,M}, meta))
    end
end

#=== `Tagged` API ===#

"""
```
tag(value, context::Context, metadata = Cassette.NoMetaData())
```

Return `value` tagged w.r.t. `context`, optionally associating `metadata` with the returned
`Tagged` instance.

Any provided `metadata` must obey the type constraints determined by Cassette's
[`metadatatype`](@ref) method.

Note that `hastagging(typeof(context))` must be `true` for a value to be tagged w.r.t. to
`context`.

See also: [`untag`](@ref), [`enabletagging`](@ref), [`hastagging`](@ref)
"""
function tag(value, context::Context, metadata = NoMetaData())
    return Tagged(context, value, initmeta(context, value, metadata))
end

function tag(value, context::ContextUntagged, metadata = NoMetaData())
    error("cannot `tag` a value w.r.t. a `context` if `!hastagging(typeof(context))`")
end

"""
```
untag(x, context::Context)
```

Return `x` untagged w.r.t. `context` if `istagged(x, context)`, otherwise return
`x` directly.

In other words, `untag(tag(x, context), context) === x` is always `true`.

If `!istagged(x, context)`, then `untag(x, context) === x` is `true`.

See also: [`tag`](@ref), [`istagged`](@ref)
"""
untag(x, context::Context) = untag(x, context.tag)
untag(x::Tagged{T}, tag::T) where {T<:Tag} = x.value
untag(x, ::Union{Tag,Nothing}) = x

"""
```
untagtype(::Type{T}, ::Type{C<:Context})
```

Return `typeof(untag(::T, ::C))`.

In other words, `untagtype(typeof(tag(x, context)), typeof(context)) === typeof(x)` is always
`true`.

If `!istaggedtype(T, C)`, then `untagtype(T, C) === T` is `true`.
"""
untagtype(X::Type, ::Type{C}) where {C<:Context} = untagtype(X, tagtype(C))
untagtype(::Type{<:Tagged{T,V}}, ::Type{T}) where {T<:Tag,V} = V
untagtype(X::Type, ::Type{<:Union{Tag,Nothing}}) = X

"""
```
metadata(x, context::Context)
```

Return the `metadata` attached to `x` if `hasmetadata(x, context)`, otherwise return
`Cassette.NoMetaData()`.

In other words, `metadata(tag(x, context, m)), context) === m` is always `true`.

If `!hasmetadata(x, context)`, then `metadata(x, context) === Cassette.NoMetaData()` is
`true`.
"""
metadata(x, context::Context) = metadata(x, context.tag)
metadata(x::Tagged{T}, tag::T) where {T<:Tag} = x.meta.data
metadata(::Any, ::Union{Tag,Nothing}) = NoMetaData()

metameta(x, context::Context) = metameta(x, context.tag)
metameta(x::Tagged{T}, tag::T) where {T<:Tag} = x.meta.meta
metameta(::Any, ::Union{Tag,Nothing}) = NoMetaMeta()

"""
```
istagged(x, context::Context)
```

Return `true` if `x` is tagged w.r.t. `context`, return `false` otherwise.

In other words, `istagged(tag(x, context), context)` is always `true`.

See also: [`tag`](@ref), [`istaggedtype`](@ref)
"""
istagged(x, context::Context) = istagged(x, context.tag)
istagged(x::Tagged{T}, tag::T) where {T<:Tag} = true
istagged(::Any, ::Union{Tag,Nothing}) = false

"""
```
istaggedtype(::Type{T}, ::Type{C<:Context})
```

Return `typeof(istagged(::T, ::C))`.

In other words, `istaggedtype(typeof(tag(x, context)), typeof(context))` is always `true`.

See also: [`tag`](@ref), [`istagged`](@ref)
"""
istaggedtype(X::Type, ::Type{C}) where {C<:Context} = istaggedtype(X, tagtype(C))
istaggedtype(::Type{<:Tagged{T}}, ::Type{T}) where {T<:Tag} = true
istaggedtype(::DataType, ::Type{<:Union{Tag,Nothing}}) = false

"""
```
hasmetadata(x, context::Context)
```

Return `true` if `!isa(metadata(x, context), Cassette.NoMetaData)`, return `false` otherwise.

In other words, `hasmetadata(tag(x, context, m), context)` is always `true` and
`hasmetadata(tag(x, context), context)` is always `false`.

See also: [`metadata`](@ref)
"""
hasmetadata(x, context::Context) = hasmetadata(x, context.tag)
hasmetadata(x, tag::Union{Tag,Nothing}) = !isa(metadata(x, tag), NoMetaData)

hasmetameta(x, context::Context) = hasmetameta(x, context.tag)
hasmetameta(x, tag::Union{Tag,Nothing}) = !isa(metameta(x, tag), NoMetaMeta)

#########################
# Core._apply iteration #
#########################

destructstate(ctx::ContextTagged{T}, state::Tagged{T,<:Tuple}) where {T} = (tagged_getfield(ctx, state, 1), tagged_getfield(ctx, state, 2))
destructstate(ctx, state) = untag(state, ctx)

Base.iterate(t::Tagged) = destructstate(t.context, overdub(t.context, iterate, t))
Base.iterate(t::Tagged, state) = destructstate(t.context, overdub(t.context, iterate, t, state))

################
# `tagged_new` #
################

@generated function tagged_new(context::C, ::Type{T}, args...) where {C<:Context,T}
    argmetaexprs = Any[]
    for i in 1:fieldcount(T)
        if i <= nfields(args) && istaggedtype(args[i], C)
            push!(argmetaexprs, :(args[$i].meta))
        else
            push!(argmetaexprs, :NOMETA)
        end
    end
    untagged_args = [:(untag(args[$i], context)) for i in 1:nfields(args)]
    newexpr = (T <: Tuple) ? Expr(:tuple, untagged_args...) : Expr(:new, T, untagged_args...)
    onlytypeargs = true
    for arg in args
        if !(arg <: Type)
            onlytypeargs = false
            break
        end
    end
    if (all(x == :NOMETA for x in argmetaexprs) && doesnotneedmetatype(T)) || onlytypeargs
        return newexpr
    else
        metametaexpr = _metametaexpr(C, T, argmetaexprs)
        return quote
            M = metatype(C, T)
            return Tagged(context, $newexpr, Meta(NoMetaData(), $metametaexpr))
        end
    end
end

@generated function tagged_new_array(context::C, ::Type{T}, args...) where {C<:Context,T<:Array}
    untagged_args = [:(untag(args[$i], context)) for i in 1:nfields(args)]
    return quote
        return tag($(T)($(untagged_args...)), context)
    end
end

@generated function tagged_new_module(context::C, args...) where {C<:Context}
    if istaggedtype(args[1], C)
        return_expr = quote
            Tagged(context, tagged_module.value,
                   Meta(NoMetaData(),
                        ModuleMeta(args[1].meta, tagged_module.meta.meta.bindings)))
        end
    else
        return_expr = :(tagged_module)
    end
    return quote
        new_module = Module(args...)
        tagged_module = fetch_tagged_module(context, new_module)
        return $return_expr
    end
end

@generated function tagged_new_tuple(context::C, args...) where {C<:Context}
    T = Tuple{[untagtype(args[i], C) for i in 1:nfields(args)]...}
    return quote
        tagged_new(context, $T, args...)
    end
end

# like `tagged_new_tuple`, but will not necessarily tag non-`doesnotneedmeta` elements of `args`
@generated function _tagged_new_tuple_unsafe(context::C, args...) where {C<:Context}
    if all(!istaggedtype(arg, C) for arg in args)
        return quote
            Core.tuple(args...)
        end
    else
        return quote
            tagged_new_tuple(context, args...)
        end
    end
end

##################################
# `tagged_*` intrisic primitives #
##################################

_untag_all(context::Context, a) = untag(a, context)
_untag_all(context::Context, a, b) = (untag(a, context), untag(b, context))
_untag_all(context::Context, a, b, c) = (untag(a, context), untag(b, context), untag(c, context))
_untag_all(context::Context, a, b, c, rest...) = (untag(a, context), untag(b, context), untag(c, context), _untag_all(context, rest...)...)

#=== tagged_nameof ===#

tagged_nameof(context::Context, x) = nameof(untag(x, context))

function tagged_nameof(context::ContextTagged{T}, x::Tagged{T,Module}) where {T}
    name_value = nameof(x.value)
    name_meta = hasmetameta(x, context) ? x.meta.meta.name : NOMETA
    return Tagged(context, name_value, name_meta)
end

#=== tagged_globalref ===#

@inline function tagged_globalref(context::ContextTagged{T},
                                  m::Tagged{T},
                                  name,
                                  primal) where {T}
    if hasmetameta(m, context) && !istagged(primal, context)
        return _tagged_globalref(context, m, name, primal)
    else
        return primal
    end
end

# assume that `context` === `primal` TODO is this assumption valid?
@inline function tagged_globalref(context::ContextTagged{T},
                                  m::Tagged{T},
                                  name,
                                  primal::ContextTagged{T}) where {T}
    return primal
end

@inline function _tagged_globalref(context::ContextTagged{T},
                                   m::Tagged{T},
                                   name,
                                   primal) where {T}
    untagged_name = untag(name, context)
    if isconst(m.value, untagged_name) && doesnotneedmeta(primal)
        # It's very important that this fast path exists and is taken with no runtime
        # overhead; this is the path that will be taken by, for example, access of simple
        # named function bindings.
        return primal
    else
        meta = fetch_bindingmeta!(context, m.value, m.meta.meta.bindings, untagged_name, primal)
        return Tagged(context, primal, meta)
    end
end

#=== tagged_globalref_set_meta! ===#

@inline function tagged_globalref_set_meta!(context::ContextTagged{T}, m::Tagged{T}, name::Symbol, primal) where {T}
    bindingmeta = _fetch_bindingmeta!(context, m.value, m.meta.meta.bindings, name)
    bindingmeta.data = istagged(primal, context) ? primal.meta : NOMETA
    return nothing
end

#=== tagged_getfield ===#

tagged_getfield(context::Context, x, name) = tagged_getfield(context, x, name, false)

tagged_getfield(context::ContextTagged{T}, x, name, boundscheck) where {T} = getfield(x, untag(name, context), untag(boundscheck, context))

function tagged_getfield(context::ContextTagged{T}, x::Tagged{T}, name, boundscheck) where {T}
    untagged_boundscheck = untag(boundscheck, context)
    untagged_name = untag(name, context)
    x_value = untag(x, context)
    y_value = getfield(x_value, untagged_name, untagged_boundscheck)
    if isa(x_value, Module)
        return tagged_globalref(context, x, untagged_name, getfield(x_value, untagged_name))
    elseif hasmetameta(x, context)
        y_meta = load(getfield(x.meta.meta, untagged_name, untagged_boundscheck))
        doesnotneedmeta(y_value) && y_meta === NOMETA && return y_value
        return Tagged(context, y_value, y_meta)
    elseif doesnotneedmeta(y_value)
        return y_value
    else
        return Tagged(context, y_value, NOMETA)
    end
end

#=== tagged_setfield! ===#

tagged_setfield!(context::ContextTagged{T}, x, name, y) where {T} = setfield!(x, untag(name, context), y)

function tagged_setfield!(context::ContextTagged{T}, x::Tagged{T}, name, y) where {T}
    untagged_name = untag(name, context)
    y_value = untag(y, context)
    y_meta = istagged(y, context) ? y.meta : NOMETA
    setfield!(x.value, untagged_name, y_value)
    if hasmetameta(x, context)
        store!(getfield(x.meta.meta, untagged_name), y_meta)
    end
    return y
end

#=== tagged_arrayref ===#

function tagged_arrayref(context::ContextTagged{T}, boundscheck, x, inds...) where {T}
    return Core.arrayref(untag(boundscheck, context), x, _untag_all(context, inds...)...)
end

function tagged_arrayref(context::ContextTagged{T}, boundscheck, x::Tagged{T}, inds...) where {T}
    untagged_boundscheck = untag(boundscheck, context)
    untagged_inds = _untag_all(context, inds...)
    y_value = Core.arrayref(untagged_boundscheck, untag(x, context), untagged_inds...)
    if hasmetameta(x, context)
        y_meta = Core.arrayref(untagged_boundscheck, x.meta.meta, untagged_inds...)
    else
        y_meta = NOMETA
    end
    return Tagged(context, y_value, y_meta)
end

#=== tagged_arrayset ===#

function tagged_arrayset(context::ContextTagged{T}, boundscheck, x, y, inds...) where {T}
    return Core.arrayset(untag(boundscheck, context), x, y, _untag_all(context, inds...)...)
end

function tagged_arrayset(context::ContextTagged{T}, boundscheck, x::Tagged{T}, y, inds...) where {T}
    untagged_boundscheck = untag(boundscheck, context)
    untagged_inds = _untag_all(context, inds...)
    y_value = untag(y, context)
    y_meta = istagged(y, context) ? y.meta : NOMETA
    Core.arrayset(untagged_boundscheck, untag(x, context), y_value, untagged_inds...)
    if hasmetameta(x, context)
        Core.arrayset(untagged_boundscheck, x.meta.meta, convert(eltype(x.meta.meta), y_meta), untagged_inds...)
    end
    return x
end

#=== tagged_growbeg! ===#

tagged_growbeg!(context::ContextTagged{T}, x, delta) where {T} = Base._growbeg!(x, untag(delta, context))

function tagged_growbeg!(context::ContextTagged{T}, x::Tagged{T}, delta) where {T}
    delta_untagged = untag(delta, context)
    Base._growbeg!(x.value, delta_untagged)
    if hasmetameta(x, context)
        Base._growbeg!(x.meta.meta, delta_untagged)
        x.meta.meta[1:delta_untagged] .= Ref(NOMETA)
    end
    return nothing
end

#=== tagged_growend! ===#

tagged_growend!(context::ContextTagged{T}, x, delta) where {T} = Base._growend!(x, untag(delta, context))

function tagged_growend!(context::ContextTagged{T}, x::Tagged{T}, delta) where {T}
    delta_untagged = untag(delta, context)
    Base._growend!(x.value, delta_untagged)
    if hasmetameta(x, context)
        old_length = length(x.meta.meta)
        Base._growend!(x.meta.meta, delta_untagged)
        x.meta.meta[(old_length + 1):(old_length + delta_untagged)] .= Ref(NOMETA)
    end
    return nothing
end

#=== tagged_growat! ===#

function tagged_growat!(context::ContextTagged{T}, x, i, delta) where {T}
    return Base._growat!(x, untag(i, context), untag(delta, context))
end

function tagged_growat!(context::ContextTagged{T}, x::Tagged{T}, i, delta) where {T}
    i_untagged = untag(i, context)
    delta_untagged = untag(delta, context)
    Base._growat!(x.value, i_untagged, delta_untagged)
    if hasmetameta(x, context)
        Base._growat!(x.meta.meta, i_untagged, delta_untagged)
        x.meta.meta[i_untagged:(i_untagged + delta_untagged - 1)] .= Ref(NOMETA)
    end
    return nothing
end

#=== tagged_deletebeg! ===#

tagged_deletebeg!(context::ContextTagged{T}, x, delta) where {T} = Base._deletebeg!(x, untag(delta, context))

function tagged_deletebeg!(context::ContextTagged{T}, x::Tagged{T}, delta) where {T}
    delta_untagged = untag(delta, context)
    Base._deletebeg!(x.value, delta_untagged)
    hasmetameta(x, context) && Base._deletebeg!(x.meta.meta, delta_untagged)
    return nothing
end

#=== tagged_deleteend! ===#

tagged_deleteend!(context::ContextTagged{T}, x, delta) where {T} = Base._deleteend!(x, untag(delta, context))

function tagged_deleteend!(context::ContextTagged{T}, x::Tagged{T}, delta) where {T}
    delta_untagged = untag(delta, context)
    Base._deleteend!(x.value, delta_untagged)
    hasmetameta(x, context) && Base._deleteend!(x.meta.meta, delta_untagged)
    return nothing
end

#=== tagged_deleteat! ===#

function tagged_deleteat!(context::ContextTagged{T}, x, i, delta) where {T}
    return Base._deleteat!(x, untag(i, context), untag(delta, context))
end

function tagged_deleteat!(context::ContextTagged{T}, x::Tagged{T}, i, delta) where {T}
    i_untagged = untag(i, context)
    delta_untagged = untag(delta, context)
    Base._deleteat!(x.value, i_untagged, delta_untagged)
    hasmetameta(x, context) && Base._deleteat!(x.meta.meta, i_untagged, delta_untagged)
    return nothing
end

#=== tagged_typeassert ===#

function tagged_typeassert(context::ContextTagged{T}, x, typ) where {T}
    return Core.typeassert(x, untag(typ, context))
end

function tagged_typeassert(context::ContextTagged{T}, x::Tagged{T}, typ) where {T}
    untagged_result = Core.typeassert(untag(x, context), untag(typ, context))
    return Tagged(context, untagged_result, x.meta)
end

#=== tagged_sitofp ===#

function tagged_sitofp(context::ContextTagged{T}, F, x) where {T}
    return Base.sitofp(untag(F, context), x)
end

function tagged_sitofp(context::ContextTagged{T}, F, x::Tagged{T}) where {T}
    return Tagged(context, Base.sitofp(untag(F, context), x.value), x.meta)
end

#=== tagged_sle_int ===#

tagged_sle_int(context::Context, x, y) = Base.sle_int(untag(x, context), untag(y, context))

###################
# Pretty Printing #
###################

Base.show(io::IO, meta::Union{NoMetaMeta,NoMetaData}) = print(io, "_")

function Base.show(io::IO, meta::Meta)
    if isa(meta.data, NoMetaData) && isa(meta.meta, NoMetaMeta)
        print(io, "_")
    else
        if isa(meta.meta, NamedTuple)
            tmp = IOBuffer()
            write(tmp, "(")
            i = 1
            for (k, v) in pairs(meta.meta)
                print(tmp, k, " = ", load(v))
                if i == length(meta.meta)
                    print(tmp, ")")
                else
                    print(tmp, ", ")
                    i += 1
                end
            end
            metametastr = String(take!(tmp))
        elseif isa(meta.meta, Tuple)
            tmp = IOBuffer()
            write(tmp, "(")
            i = 1
            for v in meta.meta
                print(tmp, load(v))
                if i == length(meta.meta)
                    print(tmp, ")")
                else
                    print(tmp, ", ")
                    i += 1
                end
            end
            metametastr = String(take!(tmp))
        else
            metametastr = sprint(show, meta.meta)
        end
        print(io, "Meta(", meta.data, ", ", metametastr, ")")
    end
end

Base.show(io::IO, x::Tagged) = print(io, "Tagged(", x.context.tag, ", ", x.value, ", ", x.meta, ")")

Base.show(io::IO, ::Tag{N,X,E}) where {N,X,E} = print(io, "Tag{", N, ",", X, ",", E, "}()")
