#######
# Tag #
#######

struct Tag{T} end

@generated function Tag(::T) where {T}
    return quote
        $(Expr(:meta, :inline))
        Tag{$(object_id(T))}()
    end
end

###########
# Context #
###########

abstract type Context{S,T} end

Base.show(io::IO, ::Context{S,T}) where {S,T} = print(io, "$S{$T}()")
