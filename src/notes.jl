########
# Note #
########

mutable struct Note{G<:AbstractGenre,V,P,C}
    value::V
    parent::Note{G,P}
    cache::C
end

###################
# Getters/Setters #
###################

value(note::Note) = note.value

parent(note::Note) = note.parent

cache(note::Note) = note.cache

genre(::Type{Note{G,V,P,C}}) = G()

value!(note::Note, v) = (note.value = v)

cache!(note::Note, c) = (note.cache = c)
