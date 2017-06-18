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

##############
# Directives #
##############

abstract type Directive{G<:AbstractGenre,F} end

macro defdirective(D)
    return esc(quote
        struct $D{G<:$AbstractGenre,F} <: $Directive{G,F}
            func::SpecializedFunction{F}
            @inline $D{G}(func::SpecializedFunction{F}) where {G<:$AbstractGenre,F}  = new{G,F}(func)
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

@inline (r::Record{VoidGenre})(output, input, cache...) = output
