####################
# FunctionWrappers #
####################

const DispatchWrapper = FunctionWrappers.FunctionWrapper{Void,Tuple{}}

struct HookWrapper{M<:HookMode,F<:FunctionNote,O<:ValueNote} <: Function
    mode::M
    parent::F
    output::O
end

HookWrapper(mode::HookMode, note::ValueNote) = HookWrapper(mode, parent(note), note)

@noinline function (w::HookWrapper)()
    h! = Hook(w.mode, genre(w.parent), value(w.parent))
    h!(w.output, parent(w.parent), cache(w.parent))
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
        forward = [DispatchWrapper(HookWrapper(Replay(), notes[i])) for i in 1:length(notes)]
        reverse = [DispatchWrapper(HookWrapper(Rewind(), notes[i])) for i in length(notes):-1:1]
        return new(notes, forward, reverse)
    end
end

function Tape(output::ValueNote)
    notes = Vector{ValueNote}()
    rewind!(note -> isroot(note) || push!(notes, note), output)
    return Tape(reverse!(notes))
end

replay!(t::Tape) = (for f! in t.forward; f!(); end; nothing)
rewind!(t::Tape) = (for f! in t.reverse; f!(); end; nothing)
