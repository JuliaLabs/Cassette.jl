###############
# Instruction #
###############

struct Instruction{G<:AbstractGenre,F,I<:Tuple,O<:ValueNote,C}
    genre::G
    func::F
    input::I
    output::O
    cache::C
end

####################
# FunctionWrappers #
####################

const DispatchWrapper = FunctionWrappers.FunctionWrapper{Void,Tuple{}}

struct HookWrapper{M<:HookMode,I} <: Function
    mode::M
    instruction::I
end

@noinline function (w::HookWrapper)()
    h! = Hook(w.mode, w.instruction.genre, w.instruction.func)
    h!(w.instruction.output, w.instruction.input, w.instruction.cache)
    return nothing
end

########
# Tape #
########

struct Tape
    instructions::Vector{Instruction}
    forward::Vector{DispatchWrapper}
    reverse::Vector{DispatchWrapper}
    function Tape(instructions::Vector{Instruction})
        forward = [DispatchWrapper(HookWrapper(Replay(), instructions[i])) for i in 1:length(instructions)]
        reverse = [DispatchWrapper(HookWrapper(Rewind(), instructions[i])) for i in length(instructions):-1:1]
        return new(instructions, forward, reverse)
    end
end

function Tape(output::ValueNote)
    instructions = Vector{Instruction}()
    walkback(output) do note, hasparent
        hasparent && push!(instructions, dub(note))
    end
    return Tape(reverse!(instructions))
end

@noinline dub(output::ValueNote) = dub(output.parent, output)
@noinline dub(parent::FunctionNote, output::ValueNote) = Hook(Dub(), parent.genre, parent.func)(output, parent.input)

replay!(t::Tape) = (for f! in t.forward; f!(); end; nothing)
rewind!(t::Tape) = (for f! in t.reverse; f!(); end; nothing)
