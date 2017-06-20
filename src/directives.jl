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

#######################
# SpecializedFunction #
#######################

struct TypeArgument{T} end

@inline unwrap(x) = x
@inline unwrap(::TypeArgument{T}) where {T} = T

struct SpecializedFunction{F} <: Function
    func::F
end

@inline SpecializedFunction(f::SpecializedFunction) = f

@inline func(f::SpecializedFunction) = f.func

@inline (f::SpecializedFunction{<:Any})(a) = func(f)(unwrap(a))
@inline (f::SpecializedFunction{<:Any})(a, b) = func(f)(unwrap(a), unwrap(b))
@inline (f::SpecializedFunction{<:Any})(a, b, c) = func(f)(unwrap(a), unwrap(b), unwrap(c))
@inline (f::SpecializedFunction{<:Any})(a, b, c, d) = func(f)(unwrap(a), unwrap(b), unwrap(c), unwrap(d))
@inline (f::SpecializedFunction{<:Any})(a, b, c, d, e) = func(f)(unwrap(a), unwrap(b), unwrap(c), unwrap(d), unwrap(e))
@inline (f::SpecializedFunction{<:Any})(a, b, c, d, e, args...) = func(f)(unwrap(a), unwrap(b), unwrap(c), unwrap(d), unwrap(e), unwrap.(args)...)

Base.show(io::IO, f::SpecializedFunction) = print(io, typeof(f), "()")

##############
# Directives #
##############

abstract type Directive{G<:AbstractGenre,F} end

macro defdirective(D)
    return esc(quote
        struct $D{G<:$AbstractGenre,F} <: $Directive{G,F}
            func::$SpecializedFunction{F}
            @inline $D{G}(func::$SpecializedFunction{F}) where {G<:$AbstractGenre,F}  = new{G,F}(func)
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

@inline (r::Record{ValueGenre})(output::Union{Real,AbstractArray}, input) = ValueNote(output, FunctionNote{ValueGenre}(func(r), input))
@inline (r::Record{ValueGenre})(output::Bool, input) = output
@inline (r::Record{ValueGenre})(output, input) = output

#############
# VoidGenre #
#############

@inline (p::Play{VoidGenre})(input...) = Disarm(func(p))(input...)

@inline (r::Record{VoidGenre})(output, input) = output
