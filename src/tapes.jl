####################
# FunctionWrappers #
####################

const DispatchWrapper = FunctionWrappers.FunctionWrapper{Void,Tuple{}}

struct HookWrapper{M<:HookMode,P<:FunctionNote,O<:ValueNote} <: Function
    parent::P
    output::O
    HookWrapper{M}(note::ValueNote) where {M} = HookWrapper{M}(parent(note), note)
    HookWrapper{M}(parent::P, output::O) where {M,P,O} = new{M,P,O}(parent, output)
    #=
    I'd rather have this method as the sole inner constructor in order enforce
    `parent(hw.output) === hw.parent`. However, implementing it that way causes
    performance problems (e.g. 2x slowdown) for `rewind!`. I've no idea why -
    this constructor doesn't get called during `rewind!`. My only guess is that
    this constructor somehow screws up the dispatch precomputation performed by
    FunctionWrappers.
    =#
    # function HookWrapper{M}(output::O) where {M<:HookMode,O<:ValueNote}
    #     p = parent(output)
    #     return new{M,typeof(p),O}(p, output)
    # end
end

@inline HookWrapper(mode::M, note::ValueNote) where {M<:HookMode} = HookWrapper{M}(note)

@noinline function (w::HookWrapper{M})() where {M}
    h! = Hook(genre(w.parent), M(), value(w.parent))
    h!(w.output, parent(w.parent), w.parent)
    return nothing
end

########
# Tape #
########

struct Tape
    notes::Vector{ValueNote}
    forward::Vector{DispatchWrapper}
    reverse::Vector{DispatchWrapper}
    function Tape(notes::Vector{ValueNote})
        forward = [DispatchWrapper(HookWrapper{Replay}(notes[i])) for i in 1:length(notes)]
        reverse = [DispatchWrapper(HookWrapper{Rewind}(notes[i])) for i in length(notes):-1:1]
        return new(notes, forward, reverse)
    end
end

function Tape(output::ValueNote)
    notes = Vector{ValueNote}()
    rewind!(note -> isroot(note) || push!(notes, note), output, false)
    return Tape(reverse!(notes))
end

replay!(t::Tape) = (for f! in t.forward; f!(); end; nothing)
rewind!(t::Tape) = (for f! in t.reverse; f!(); end; nothing)
