####################
# FunctionWrappers #
####################

const DispatchWrapper = FunctionWrappers.FunctionWrapper{Void,Tuple{}}

struct Instruction{D<:Directive,O<:Note,I<:Tuple,P<:Note} <: Function
    directive!::D
    output::O
    input::I
    parent::P
end

function Instruction(::Type{D}, output::Note{G}) where {D<:Directive,G}
    output_parent = parent(output)
    directive! = D{G}(value(output_parent))
    input = parent(output_parent)
    return Instruction(directive!, output, input, output_parent)
end

@noinline (i::Instruction)() = (i.directive!(i.output, i.input, i.parent); nothing)

########
# Tape #
########

struct Tape
    notes::Vector{Note}
    forward::Vector{DispatchWrapper}
    reverse::Vector{DispatchWrapper}
    function Tape(notes::Vector{Note})
        forward = [DispatchWrapper(Instruction(Replay, notes[i])) for i in 1:length(notes)]
        reverse = [DispatchWrapper(Instruction(Rewind, notes[i])) for i in length(notes):-1:1]
        return new(notes, forward, reverse)
    end
end

function Tape(output::Note)
    notes = Vector{Note}()
    rewind!(note -> isroot(note) || push!(notes, note), output, false)
    return Tape(reverse!(notes))
end

replay!(t::Tape) = (for f! in t.forward; f!(); end; nothing)
rewind!(t::Tape) = (for f! in t.reverse; f!(); end; nothing)
