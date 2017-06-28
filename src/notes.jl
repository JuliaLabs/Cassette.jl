########
# Note #
########

struct Root end

mutable struct Note{G<:AbstractGenre,V,C,P}
    value::V
    cache::C
    parent::P
    @inline Note{G}(value::V, cache::C, parent::Root) where {G,V,C} = new{G,V,C,Root}(value, cache, parent)
    @inline Note{G}(value::V, cache::C, parent::Note) where {G,V,C} = new{G,V,C,Note}(value, cache, parent)
    @inline Note{G}(value::V, cache::C, parent::P) where {G,V,C,P<:Tuple} = new{G,V,C,P}(value, cache, parent)
    @inline Note{G}(value, cache) where {G} = Note{G}(value, cache, Root())
end

#############
# Interface #
#############

@inline unwrap(n::Note) = value(n)
@inline unwrap(::Type{N}) where {N<:Note} = value(N)

@inline genre(::Type{N}) where {G,N<:Note{G}} = G()

@inline value(note::Note) = note.value
@inline value(::Type{N}) where {G,V,N<:Note{G,V}} = V
@inline value!(note::Note, v) = (note.value = v)

@inline cache(note::Note) = note.cache
@inline cache(::Type{N}) where {G,V,C,N<:Note{G,V,C}} = C
@inline cache!(note::Note, c) = (note.cache = c)

@inline parent(note::Note) = note.parent
@inline parent(::Type{N}) where {G,V,C,P,N<:Note{G,V,C,P}} = P
@inline parent!(note::Note, p) = (note.parent = p)

@inline isroot(x) = isroot(typeof(x))
@inline isroot(::DataType) = true
@inline isroot(::Type{<:Note}) = false
@inline isroot(::Type{N}) where {G<:AbstractGenre,V,C,N<:Note{G,V,C,Root}} = true

###########
# rewind! #
###########
# TODO: How to handle elementwise Note ancestors?

@inline function rewind!(f!, n::Note, recursive::Bool = false)
    if recursive
        recur_rewind!(f!, n)
    else
        iter_rewind!(f!, n)
    end
    return nothing
end

# iter_rewind! #
#--------------#

iter_rewind!(f!, n) = nothing

function iter_rewind!(f!, n::Note)
    queue = Vector{Note}()
    push!(queue, n)
    while !(isempty(queue))
        current = pop!(queue)
        f!(current)
        isroot(current) || iter_rewind_ancestors!(queue, parent(current))
    end
    return nothing
end

@generated function iter_rewind_ancestors!(queue, n::Note{G,V,C,P}) where {G,V,C,P<:Tuple}
    push_calls = Expr(:block, Any[])
    for i in 1:nfields(P)
        T = P.parameters[i]
        if T <: Note
            push!(push_calls.args, :(push!(queue, ancestors[$i]::$T)))
        end
    end
    return quote
        $(Expr(:meta, :noinline))
        ancestors = parent(n)
        $(push_calls)
        return nothing
    end
end

# recur_rewind! #
#---------------#

@inline recur_rewind!(f!, n) = nothing

@inline function recur_rewind!(f!, n::Note)
    f!(n)
    isroot(n) || recur_rewind_ancestors!(f!, parent(n))
    return nothing
end

@generated function recur_rewind_ancestors!(f!, n::Note{G,V,C,P}) where {G,V,C,P<:Tuple}
    rewind_calls = Expr(:block, Any[])
    for i in 1:nfields(P)
        push!(rewind_calls.args, :(recur_rewind!(f!, ancestors[$i]::$(P.parameters[i]))))
    end
    return quote
        $(Expr(:meta, :noinline))
        ancestors = parent(n)
        $(rewind_calls)
        return nothing
    end
end

###################
# Expr Generation #
###################

idstring(x) = base(62, object_id(x))

interpolated_variable(x::Note) = Symbol("x_" * idstring(x))
interpolated_variable(x) = x

function toexpr(output::Note)
    body = Expr(:block)
    args = Symbol[]
    rewind!(output) do x
        y = interpolated_variable(x)
        if !(isroot(x))
            p = parent(x)
            push!(body.args, :($y = $(value(p))($(interpolated_variable.(parent(p))))))
        elseif isa(x, Note)
            in(y, args) || push!(args, y)
        end
    end
    reverse!(body.args)
    reverse!(args)
    return args, body
end

###################
# Pretty Printing #
###################

function Base.show(io::IO, n::Note{G,V,C,P}) where {G,V,C,P}
    if P <: Note
        parent_str = "Note(...)"
    elseif P <: Tuple
        parent_str = "(...)"
    elseif P <: Root
        parent_str = "Root"
    end
    return print(io, "Note{$G}<$(idstring(n))>($(value(n)), $(cache(n)), $parent_str)")
end
