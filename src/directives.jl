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

####################
# Play{ValueGenre} #
####################

@inline (p::Play{ValueGenre})(input...) = func(p)(input...)

######################
# Record{ValueGenre} #
######################

@inline (r::Record{ValueGenre})(output, input) = output #track(output, FunctionNote{ValueGenre}(func(r), input))

######################
# Replay{ValueGenre} #
######################

# @inline (r::Replay{ValueGenre})(output::RealNote, input::Tuple, parent) = value!(output, disarm(func(r))(input...))
# @inline (r::Replay{ValueGenre})(output::ArrayNote, input::Tuple, parent) = copy!(value(output), disarm(func(r))(input...))

######################
# Rewind{ValueGenre} #
######################

# ValueGenre doesn't support `Rewind`.
