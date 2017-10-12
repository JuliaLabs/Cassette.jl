###############
# StorageUnit #
###############

abstract type StorageUnit{D} end

@inline datatype(::Union{StorageUnit{D},Type{<:StorageUnit{D}}}) where {D} = D

# Mutable #
#---------#

mutable struct Mutable{D} <: StorageUnit{D}
    data::D
    Mutable{D}() where D = new{D}()
    Mutable{D}(data) where D = new{D}(data)
end

@inline Mutable(data::D) where {D} = Mutable{D}(data)

@inline Base.getindex(x::Mutable) = x.data

@inline Base.setindex!(x::Mutable, y) = (x.data = y)

# Immutable #
#-----------#

struct Immutable{D} <: StorageUnit{D}
    data::D
end

@inline Base.getindex(x::Immutable) = x.data

@inline Base.setindex!(x::Immutable, y) = error("cannot mutate immutable field")

#########
# Field #
#########

struct Name{n} end

struct Field{n,S<:StorageUnit}
    name::Name{n}
    storage::S
end

@inline nametype(::Union{Field{n},Type{<:Field{n}}}) where {n} = n

@inline datatype(::Union{Field{n,S},Type{Field{n,S}}}) where {n,S} = datatype(S)

@inline ismutable(::Union{Field{n,S},Type{Field{n,S}}}) where {n,S} = S <: Mutable

@inline Base.getindex(field::Field) = field.storage[]

@inline Base.setindex!(field::Field, y) = (field.storage[] = y)

#############
# Anonymous #
#############

struct Anonymous{F<:Tuple{Vararg{Field}}}
    fields::F
end

@inline Anonymous(fields::Field...) = Anonymous(fields)

######################
# getfield/setfield! #
######################

@inline anon_getfield(x, ::Name{n}) where {n} = getfield(x, n)

@generated function anon_getfield(x::Anonymous{F}, ::Name{n}) where {F,n}
    for (i, fieldtype) in enumerate(F.parameters)
        if nametype(fieldtype) == n
            return quote
                $(Expr(:meta, :inline))
                return (x.fields[$i]::$fieldtype)[]
            end
        end
    end
    return quote
        $(Expr(:meta, :inline))
        error("type ", typeof(x), "has no field ", n)
    end
end

@inline anon_setfield!(x, ::Name{n}, y) where {n} = setfield!(x, n, y)

@generated function anon_setfield!(x::Anonymous{F}, ::Name{n}, y) where {F,n}
    for (i, fieldtype) in enumerate(F.parameters)
        if nametype(fieldtype) == n
            return quote
                $(Expr(:meta, :inline))
                return (x.fields[$i]::$fieldtype)[] = y
            end
        end
    end
    return quote
        $(Expr(:meta, :inline))
        error("type ", typeof(x), " has no field ", n)
    end
end

##########
# macros #
##########

#=
Valid forms of `expr`:
- `x = d`
- `x::T = d`
- `mut(x = d)`
- `mut(x::T = d)`
=#
function translatefield(expr::Expr)
    if expr.head == :call
        if expr.args[1] == :mut
            S = :Mutable
            lhs, rhs = expr.args[2].args
        else
            error("invalid field syntax: $expr")
        end
    else
        S = :Immutable
        lhs, rhs = expr.args
    end
    name, T = (isa(lhs, Expr) && lhs.head == :(::)) ? lhs.args : (lhs, nothing)
    data = T === nothing ? :($Cassette.$S($rhs)) : :($Cassette.$S{$T}($rhs))
    return :($Cassette.Field($Cassette.Name{$(Expr(:quote, name))}(), $data))
end

macro anon(args...)
    fields = Expr(:tuple)
    for arg in args
        push!(fields.args, translatefield(arg))
    end
    return :($Cassette.Anonymous($fields))
end

macro getfield(x, n)
    esc(:($Cassette.anon_getfield($x, $Cassette.Name{$(Expr(:quote, n))}())))
end

macro setfield!(x, n, y)
    esc(:($Cassette.anon_setfield!($x, $Cassette.Name{$(Expr(:quote, n))}(), $y)))
end

###################
# pretty printing #
###################

function Base.show(io::IO, x::Anonymous)
    print(io, "@anon(")
    for (i, field) in enumerate(x.fields)
        fieldstr = string(nametype(field), "::", datatype(field), " = ", field[])
        if ismutable(field)
            fieldstr = string("mut(", fieldstr, ")")
        end
        print(io, fieldstr)
        i != length(x.fields) && print(io, ", ")
    end
    print(io, ")")
    return nothing
end
