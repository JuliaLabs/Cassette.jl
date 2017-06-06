#########
# Chord #
#########

struct Chord{G<:AbstractGenre,F,I<:Tuple,O,C}
    genre::G
    func::F
    input::I
    output::O
    cache::C
end

@inline Chord(note::FunctionNote, output, cache = nothing) = Chord(note.genre, note.func, note.input, output, cache)

#############
# PlayChord #
#############

abstract type PlayMode end

struct ForwardMode <: PlayMode end
struct ReverseMode <: PlayMode end

struct PlayChord{C<:Chord,M<:PlayMode} <: Function
    chord::C
    mode::M
end

@noinline (p::PlayChord)() = (play!(p.chord, p.mode); nothing)

########
# Tape #
########

const ExecutionWrapper = FunctionWrappers.FunctionWrapper{Void,Tuple{}}

struct Tape
    chords::Vector{Chord}
    forward::Vector{ExecutionWrapper}
    reverse::Vector{ExecutionWrapper}
    function Tape(chords::Vector{Chord})
        forward = [ExecutionWrapper(Play(chords[i], ForwardMode())) for i in 1:length(chords)]
        reverse = [ExecutionWrapper(Play(chords[i], ReverseMode())) for i in length(chords):-1:1]
        return new(chords, forward, reverse)
    end
end

function Tape(output::ValueNote)
    chords = Vector{Chord}()
    walkback(output) do note, hasparent
        hasparent && push!(chords, Chord(note.parent, note))
    end
    return Tape(reverse!(chords))
end

#########
# play! #
#########

@inline function play!(chord::Chord{ValueGenre,<:Any,<:Tuple,<:RealNote}, ::ForwardMode)
    chord.output.value = Untrack(chord.func)(chord.input...)
    return nothing
end

@inline function play!(chord::Chord{ValueGenre,<:Any,<:Tuple,<:ArrayNote}, ::ForwardMode)
    copy!(chord.output.value, Untrack(chord.func)(chord.input...))
    return nothing
end

play!(t::Tape, ::ForwardMode) = (for c! in t.forward; c!(); end; nothing)
play!(t::Tape, ::ReverseMode) = (for c! in t.reverse; c!(); end; nothing)
