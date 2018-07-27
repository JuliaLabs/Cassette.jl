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

function Base.convert(::Type{Meta{D,M}}, meta::Meta) where {D,M}
    metadata = _metadataconvert(D, meta.data)
    metameta = _metametaconvert(M, meta.meta)
    return Meta{D,M}(metadata, metameta)
end

_metadataconvert(T, x::NoMetaData) = x
_metadataconvert(T, x) = convert(T, x)

_metametaconvert(T, x::NoMetaMeta) = x
_metametaconvert(T, x) = convert(T, x)

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
# actual context object (just the type). This `@pure` is vtjnash-approved. It should allow
# the compiler to optimize away the fetch once we have support for it, e.g. loop invariant
# code motion.
Base.@pure @noinline function fetch_tagged_module(context::Context, m::Module)
    bindings = get!(() -> BindingMetaDict(), context.bindings, m)
    return Tagged(context, m, Meta(NoMetaData(), ModuleMeta(NOMETA, bindings)))
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
    if isconcretetype(T) || T <: Type
        return Meta{metadatatype(C, T),metametatype(C, T)}
    end
    return Meta
end

#=== metadatatype ===#

metadatatype(::Type{<:Context}, ::DataType) = NoMetaData

#=== metametatype ===#

@generated function metametatype(::Type{C}, ::Type{T}) where {C<:Context,T}
    if T <: Type || fieldcount(T) == 0
        body = :(NoMetaMeta)
    elseif !(isconcretetype(T))
        body = :(error("cannot call metametatype on non-concrete type ", $T))
    else
        F = T.mutable ? :Mutable : :Immutable
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
    if V <: Type || fieldcount(V) == 0 || (all(x == :NOMETA for x in metaexprs) && isbitstype(V))
        return :(NoMetaMeta())
    else
        F = V.mutable ? :Mutable : :Immutable
        metatypes = [:(metatype(C, $(fieldtype(V, i)))) for i in 1:fieldcount(V)]
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

initmetameta(context::Context, value::Module) = fetch_tagged_module(context, value).meta

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

#=
Here, `U` is the innermost, "underlying" type of the value being wrapped. This parameter is
precomputed so that Cassette can directly dispatch on it in signatures generated for
contextual primitives.
=#
struct Tagged{T<:Tag,U,V,D,M}
    tag::T
    value::V
    meta::Meta{D,M}
    function Tagged(context::C, value::V, meta::Meta) where {T<:Tag,V,C<:ContextWithTag{T}}
        U = _underlying_type(V)
        D = metadatatype(C, V)
        M = metametatype(C, V)
        return new{T,U,V,D,M}(context.tag, value, convert(Meta{D,M}, meta))
    end
end

#=== `Tagged` internals ===#

_underlying_type(V::Type) = V
_underlying_type(::Type{<:Tagged{<:Tag,U}}) where {U} = U

#=== `Tagged` API ===#

function tag(value, context::Context, metadata = NoMetaData())
    return Tagged(context, value, initmeta(context, value, metadata))
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
    if (all(x == :NOMETA for x in argmetaexprs) && isbitstype(T)) || onlytypeargs
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

# has an extra optimization that can be used when you can assume that
# you don't e.g. need to allocate meta objects for non-isbits elements
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

#=== tagged_nameof ===#

tagged_nameof(context::Context, x) = nameof(untag(x, context))

function tagged_nameof(context::ContextWithTag{T}, x::Tagged{T,Module}) where {T}
    name_value = nameof(x.value)
    name_meta = hasmetameta(x, context) ? x.meta.meta.name : NOMETA
    return Tagged(context, name_value, name_meta)
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
        return Tagged(context, primal, meta)
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

tagged_getfield(context::ContextWithTag{T}, x, name, boundscheck...) where {T} = getfield(x, untag(name, context), boundscheck...)

function tagged_getfield(context::ContextWithTag{T}, x::Tagged{T,Module}, name) where {T}
    untagged_name = untag(name, context)
    return tagged_global_ref(context, x, untagged_name, getfield(x.value, untagged_name))
end

function tagged_getfield(context::ContextWithTag{T}, x::Tagged{T}, name, boundscheck...) where {T}
    untagged_name = untag(name, context)
    y_value = getfield(untag(x, context), untagged_name, boundscheck...)
    if hasmetameta(x, context)
        y_meta = load(getfield(x.meta.meta, untagged_name, boundscheck...))
    else
        y_meta = NOMETA
    end
    return Tagged(context, y_value, y_meta)
end

#=== tagged_setfield! ===#

tagged_setfield!(context::ContextWithTag{T}, x, name, y, boundscheck...) where {T} = setfield!(x, untag(name, context), y, boundscheck...)

function tagged_setfield!(context::ContextWithTag{T}, x::Tagged{T}, name, y, boundscheck...) where {T}
    untagged_name = untag(name, context)
    y_value = untag(y, context)
    y_meta = istagged(y, context) ? y.meta : NOMETA
    setfield!(x.value, untagged_name, y_value, boundscheck...)
    if hasmetameta(x, context)
        store!(getfield(x.meta.meta, untagged_name, boundscheck...), y_meta)
    end
    return y
end

#=== tagged_arrayref ===#

function tagged_arrayref(context::ContextWithTag{T}, boundscheck, x, i) where {T}
    return Core.arrayref(untag(boundscheck, context), x, untag(i, context))
end

function tagged_arrayref(context::ContextWithTag{T}, boundscheck, x::Tagged{T}, i) where {T}
    untagged_boundscheck = untag(boundscheck, context)
    untagged_i = untag(i, context)
    y_value = Core.arrayref(untagged_boundscheck, untag(x, context), untagged_i)
    if hasmetameta(x, context)
        y_meta = Core.arrayref(untagged_boundscheck, x.meta.meta, untagged_i)
    else
        y_meta = NOMETA
    end
    return Tagged(context, y_value, y_meta)
end

#=== tagged_arrayset ===#

function tagged_arrayset(context::ContextWithTag{T}, boundscheck, x, y, i) where {T}
    return Core.arrayset(untag(boundscheck, context), x, y, untag(i, context))
end

function tagged_arrayset(context::ContextWithTag{T}, boundscheck, x::Tagged{T}, y, i) where {T}
    untagged_boundscheck = untag(boundscheck, context)
    untagged_i = untag(i, context)
    y_value = untag(y, context)
    y_meta = istagged(y, context) ? y.meta : NOMETA
    Core.arrayset(untagged_boundscheck, untag(x, context), y_value, untagged_i)
    if hasmetameta(x, context)
        Core.arrayset(untagged_boundscheck, x.meta.meta, convert(eltype(x.meta.meta), y_meta), untagged_i)
    end
    return x
end

#=== tagged_growbeg! ===#

tagged_growbeg!(context::ContextWithTag{T}, x, delta) where {T} = Base._growbeg!(x, untag(delta, context))

function tagged_growbeg!(context::ContextWithTag{T}, x::Tagged{T}, delta) where {T}
    delta_untagged = untag(delta, context)
    Base._growbeg!(x.value, delta_untagged)
    if hasmetameta(x, context)
        Base._growbeg!(x.meta.meta, delta_untagged)
        x.meta.meta[1:delta_untagged] .= Ref(NOMETA)
    end
    return nothing
end

#=== tagged_growend! ===#

tagged_growend!(context::ContextWithTag{T}, x, delta) where {T} = Base._growend!(x, untag(delta, context))

function tagged_growend!(context::ContextWithTag{T}, x::Tagged{T}, delta) where {T}
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

function tagged_growat!(context::ContextWithTag{T}, x, i, delta) where {T}
    return Base._growat!(x, untag(i, context), untag(delta, context))
end

function tagged_growat!(context::ContextWithTag{T}, x::Tagged{T}, i, delta) where {T}
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

tagged_deletebeg!(context::ContextWithTag{T}, x, delta) where {T} = Base._deletebeg!(x, untag(delta, context))

function tagged_deletebeg!(context::ContextWithTag{T}, x::Tagged{T}, delta) where {T}
    delta_untagged = untag(delta, context)
    Base._deletebeg!(x.value, delta_untagged)
    hasmetameta(x, context) && Base._deletebeg!(x.meta.meta, delta_untagged)
    return nothing
end

#=== tagged_deleteend! ===#

tagged_deleteend!(context::ContextWithTag{T}, x, delta) where {T} = Base._deleteend!(x, untag(delta, context))

function tagged_deleteend!(context::ContextWithTag{T}, x::Tagged{T}, delta) where {T}
    delta_untagged = untag(delta, context)
    Base._deleteend!(x.value, delta_untagged)
    hasmetameta(x, context) && Base._deleteend!(x.meta.meta, delta_untagged)
    return nothing
end

#=== tagged_deleteat! ===#

function tagged_deleteat!(context::ContextWithTag{T}, x, i, delta) where {T}
    return Base._deleteat!(x, untag(i, context), untag(delta, context))
end

function tagged_deleteat!(context::ContextWithTag{T}, x::Tagged{T}, i, delta) where {T}
    i_untagged = untag(i, context)
    delta_untagged = untag(delta, context)
    Base._deleteat!(x.value, i_untagged, delta_untagged)
    hasmetameta(x, context) && Base._deleteat!(x.meta.meta, i_untagged, delta_untagged)
    return nothing
end

#=== tagged_typeassert ===#

function tagged_typeassert(context::ContextWithTag{T}, x, typ) where {T}
    return Core.typeassert(x, untag(typ, context))
end

function tagged_typeassert(context::ContextWithTag{T}, x::Tagged{T}, typ) where {T}
    untagged_result = Core.typeassert(untag(x, context), untag(typ, context))
    return Tagged(context, untagged_result, x.meta)
end

#=== tagged_apply_args ===#

@generated function tagged_apply_args(context::ContextWithTag{T}, args...) where {T}
    flattened = Expr(:tuple)
    for i in 1:nfields(args)
        x = args[i]
        if x <: Tuple
            push!(flattened.args, Expr(:..., Expr(:ref, :args, i)))
        elseif istaggedtype(x, context) && untagtype(x, context) <: Tuple
            for j in 1:fieldcount(untagtype(x, context))
                push!(flattened.args, :(tagged_getfield(context, args[$i], $j)))
            end
        else
            push!(flattened.args, :(args[$i]))
        end
    end
    return flattened
end


#=== tagged_sitofp ===#

function tagged_sitofp(context::ContextWithTag{T}, F, x) where {T}
    return Base.sitofp(untag(F, context), x)
end

function tagged_sitofp(context::ContextWithTag{T}, F, x::Tagged{T}) where {T}
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

Base.show(io::IO, x::Tagged) = print(io, "Tagged(", x.tag, ", ", x.value, ", ", x.meta, ")")

Base.show(io::IO, ::Tag{N,X,E}) where {N,X,E} = print(io, "Tag{", N, ",", X, ",", E, "}()")
