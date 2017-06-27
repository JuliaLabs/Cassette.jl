########
# Note #
########
# TODO: Define only one `Note` struct

abstract type AbstractNote{G<:AbstractGenre} end

mutable struct FunctionNote{G<:AbstractGenre,V,P<:Tuple,C} <: AbstractNote{G}
    value::V
    parent::P
    cache::C
    FunctionNote{G}(value::V, parent::P, cache::C = nothing) where {G,V,P,C} = new{G,V,P,C}(value, parent, cache)
end

mutable struct ValueNote{G<:AbstractGenre,V,P,C} <: AbstractNote{G}
    value::V
    parent::FunctionNote{G,P}
    cache::C
    ValueNote{G}(value::V, parent::FunctionNote{G,P}, cache::C = nothing)  where {G,V,P,C} = new{G,V,P,C}(value, parent, cache)
    ValueNote{G}(value::V, cache::C = nothing) where {G,V,C} = ValueNote{G}(value, FunctionNote{G}(nothing, tuple(), nothing), cache)
end

ValueNote(value, parent::FunctionNote{G}, cache...) where {G} = ValueNote{G}(value, parent, cache...)

##############
# Properties #
##############

@inline unwrap(n::AbstractNote) = value(n)
@inline unwrap(::Type{T}) where {T<:AbstractNote} = valuetype(T)

# getters #
#---------#

@inline value(x) = x
@inline value(note::ValueNote) = note.value
@inline value(note::FunctionNote) = note.value
@inline value(::Type{T}) where {T} = valuetype(T)

@inline parent(note::ValueNote) = note.parent
@inline parent(note::FunctionNote) = note.parent

@inline cache(note::ValueNote) = note.cache
@inline cache(note::FunctionNote) = note.cache

@inline genre(::Type{AbstractNote{G}}) where {G} = G()
@inline genre(::Type{ValueNote{G,V,P,C}}) where {G,V,P,C} = G()
@inline genre(::Type{FunctionNote{G,V,P,C}}) where {G,V,P,C} = G()

@inline valuetype(x) = valuetype(typeof(x))
@inline valuetype(::Type{V}) where {V} = V
@inline valuetype(::Type{ValueNote{G,V,P,C}}) where {G,V,P,C} = V
@inline valuetype(::Type{FunctionNote{G,V,P,C}}) where {G,V,P,C} = V

@inline cachetype(x) = cachetype(typeof(x))
@inline cachetype(::Type{V}) where {V} = Void
@inline cachetype(::Type{ValueNote{G,V,P,C}}) where {G,V,P,C} = C
@inline cachetype(::Type{FunctionNote{G,V,P,C}}) where {G,V,P,C} = C

# setters #
#---------#

@inline value!(note::ValueNote, v) = (note.value = v)

@inline cache!(note::ValueNote, c) = (note.cache = c)
@inline cache!(note::FunctionNote, c) = (note.cache = c)

##########
# isroot #
##########

@inline isroot(x) = isroot(typeof(x))
@inline isroot(::DataType) = true
@inline isroot(::Type{<:AbstractNote}) = false
@inline isroot(::Type{<:FunctionNote{<:AbstractGenre,Void}}) = true
@inline isroot(::Type{<:ValueNote{<:AbstractGenre,<:Any,Void}}) = true

###########
# rewind! #
###########
# TODO: How to handle elementwise ValueNote ancestors?

@inline function rewind!(f!, n::ValueNote, recursive::Bool = false)
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

function iter_rewind!(f!, n::ValueNote)
    queue = Vector{Any}()
    push!(queue, n)
    while !(isempty(queue))
        current = pop!(queue)
        f!(current)
        isroot(current) || iter_rewind_ancestors!(queue, parent(current))
    end
    return nothing
end

@generated function iter_rewind_ancestors!(queue, n::FunctionNote{G,V,P}) where {G<:AbstractGenre,V,P<:Tuple}
    push_calls = Expr(:block, Any[])
    for i in 1:nfields(P)
        T = P.parameters[i]
        if T <: ValueNote
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

@inline function recur_rewind!(f!, n::ValueNote)
    f!(n)
    isroot(n) || recur_rewind_ancestors!(f!, parent(n))
    return nothing
end

@generated function recur_rewind_ancestors!(f!, n::FunctionNote{G,V,P}) where {G<:AbstractGenre,V,P<:Tuple}
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

interpolated_variable(x::ValueNote) = Symbol("x_" * idstring(x))
interpolated_variable(x) = x

function toexpr(output::ValueNote)
    body = Expr(:block)
    args = Symbol[]
    rewind!(output) do x
        y = interpolated_variable(x)
        if !(isroot(x))
            p = parent(x)
            push!(body.args, :($y = $(value(p))($(interpolated_variable.(parent(p))))))
        elseif isa(x, ValueNote)
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

function Base.show(io::IO, n::FunctionNote{G,V}) where {G,V}
    return print(io, "FunctionNote{$G,$V}($(parent(n)), $(cache(n)))")
end

function Base.show(io::IO, n::ValueNote{G}) where {G}
    rootmsg = isroot(n) ? ";ROOT" : ""
    return print(io, "ValueNote{$G}<$(idstring(n))$(rootmsg)>($(value(n)), $(cache(n)))")
end
