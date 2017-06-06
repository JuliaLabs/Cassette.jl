#########
# Chord #
#########

struct Instruction{G<:AbstractGenre,F,I<:Tuple,O<:ValueNote,C}
    genre::G
    func::F
    input::I
    output::O
    cache::C
end

@noinline Instruction(parent::FunctionNote, output::ValueNote) = Dub(parent.genre, parent.func)(output, parent.input)

struct Dub{G<:AbstractGenre,F} <: Function
    genre::G
    func::F
end

(dub::Dub{ValueGenre}(output, input) = Instruction(dub.genre, dub.func, dub.input, dub.output, nothing)

###########
# Replay! #
###########

abstract type ReplayMode end

struct ForwardMode <: ReplayMode end
struct ReverseMode <: ReplayMode end

struct Replay!{M<:ReplayMode,G<:AbstractGenre,F} <: Function
    mode::M
    genre::G
    func::F
end

@inline function (r::Replay!{ForwardMode,ValueGenre})(output::RealNote, input, cache)
    output.value = Untrack(r.func)(input...)
    return nothing
end

@inline function (r::Replay!{ForwardMode,ValueGenre})(output::ArrayNote, input, cache)
    copy!(output.value, Untrack(r.func)(chord.input...))
    return nothing
end

################
# ReplayCaller #
################

struct ReplayCaller{M<:ReplayMode,C<:Chord} <: Function
    mode::M
    chord::C
end

@noinline (p::ReplayCaller)() = (Replay!(p.mode, p.chord.genre, p.func)(p.output, p.input, p.cache); nothing)

########
# Tape #
########

const ExecutionWrapper = FunctionWrappers.FunctionWrapper{Void,Tuple{}}

struct Tape
    chords::Vector{Chord}
    forward::Vector{ExecutionWrapper}
    reverse::Vector{ExecutionWrapper}
    function Tape(chords::Vector{Chord})
        forward = [ExecutionWrapper(ReplayCaller(ForwardMode(), chords[i])) for i in 1:length(chords)]
        reverse = [ExecutionWrapper(ReplayCaller(ReverseMode(), chords[i])) for i in length(chords):-1:1]
        return new(chords, forward, reverse)
    end
end

function Tape(output::ValueNote)
    chords = Vector{Chord}()
    walkback(output) do note, hasparent
        if hasparent
            push!(chords, Dub(note.))
    end
    return Tape(reverse!(chords))
end



###########
# replay! #
###########

replay!(t::Tape, ::ForwardMode) = (for f! in t.forward; f!(); end; nothing)
replay!(t::Tape, ::ReverseMode) = (for f! in t.reverse; f!(); end; nothing)
