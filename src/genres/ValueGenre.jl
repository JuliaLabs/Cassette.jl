@defgenre ValueGenre

@inline Base.promote_rule(::Type{ValueGenre}, ::Type{VoidGenre}) = ValueGenre

@inline (::TraceBehavior{ValueGenre})(input...) = Intercept()

@inline (p::Play{ValueGenre})(input...) = Record{ValueGenre}(p)(unwrapcall(p, input...), input)

@inline (r::Record{ValueGenre})(output::Tuple, input) = map(o -> r(o, input), output)
@inline (r::Record{ValueGenre})(output::Bool, input) = output
@inline (r::Record{ValueGenre})(output::Any, input) = output

@inline function (r::Record{ValueGenre})(output::Union{Real,AbstractArray}, input)
    parent = Note{ValueGenre}(unwrap(r), nothing, input)
    return Note{ValueGenre}(output, nothing, parent)
end

@inline (r::Replay{ValueGenre})(output::Tuple, input, f) = foreach(o -> r(o, input, f), output)
@inline (r::Replay{ValueGenre})(output::Note, input, f) = value!(output, unwrapcall(r, input...))
