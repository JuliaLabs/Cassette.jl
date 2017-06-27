@defgenre VoidGenre

@inline (::TraceBehavior{VoidGenre})(input...) = Recurse()

@inline (p::Play{VoidGenre})(input...) = unwrapcall(p, input...)
