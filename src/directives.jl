##############
# Directives #
##############

abstract type Directive{G<:AbstractGenre,F} end

macro defdirective(D)
    return esc(quote
        struct $D{G<:$AbstractGenre,F} <: $Directive{G,F}
            func::F
            @inline $D{G}(func::F) where {G<:$AbstractGenre,F}  = new{G,F}(func)
        end
    end)
end

@inline func(d::Directive) = d.func

@inline genre(d::Directive{G}) where {G} = G()
@inline genre(::Type{Directive{G,F}}) where {G,F} = G()

@defdirective Play
@defdirective Record
@defdirective Replay
@defdirective Rewind

##############
# ValueGenre #
##############

# TODO

#############
# VoidGenre #
#############

@inline (p::Play{VoidGenre})(input...) = func(p)(input...)
@inline (r::Record{VoidGenre})(output, args...) = output
