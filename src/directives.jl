###########
# TypeArg #
###########

struct TypeArg{T} end

@inline value(::TypeArg{T}) where {T} = value(T)
@inline value(::Type{TypeArg{T}}) where {T} = value(T)

##########
# Disarm #
##########

struct Disarm{F} <: Function
    func::F
end

@inline Disarm(f::Disarm) = f

@inline func(f::Disarm) = f.func

@inline (f::Disarm{<:Any})(a) = func(f)(value(a))
@inline (f::Disarm{<:Any})(a, b) = func(f)(value(a), value(b))
@inline (f::Disarm{<:Any})(a, b, c) = func(f)(value(a), value(b), value(c))
@inline (f::Disarm{<:Any})(a, b, c, d) = func(f)(value(a), value(b), value(c), value(d))
@inline (f::Disarm{<:Any})(a, b, c, d, e) = func(f)(value(a), value(b), value(c), value(d), value(e))
@inline (f::Disarm{<:Any})(a, b, c, d, e, args...) = func(f)(value(a), value(b), value(c), value(d), value(e), value.(args)...)

Base.show(io::IO, f::Disarm) = print(io, typeof(f), "()")

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

@inline genre(::Type{Directive{G,F}}) where {G,F} = G()

@defdirective Play
@defdirective Record
@defdirective Replay
@defdirective Rewind

##############
# ValueGenre #
##############

@inline (p::Play{ValueGenre})(input...) = Disarm(func(p))(input...)

@inline (r::Replay{ValueGenre})(output, input, parent) = value!(output, Disarm(func(r))(input...))

@inline (r::Record{ValueGenre})(output::Union{Real,AbstractArray}, input) = ValueNote(output, FunctionNote{ValueGenre}(func(r), input))
@inline (r::Record{ValueGenre})(output::Bool, input) = output
@inline (r::Record{ValueGenre})(output, input) = output

#############
# VoidGenre #
#############

@inline (p::Play{VoidGenre})(input...) = Disarm(func(p))(input...)

@inline (r::Record{VoidGenre})(output, input) = output
