###############
# Instruction #
###############

struct Instruction{G<:AbstractGenre,F,I<:Tuple,O,C}
    genre::G
    func::F
    input::I
    output::O
    cache::C
end

########################
# Instruction Wrappers #
########################

struct ForwardWrapper{I<:Instruction} <: Function
    instruction::I
end

function (w::ForwardWrapper)()
    i = w.instruction
    Dub(Forward(), i.genre, i.func)(i.cache, i.output, i.input...)
    return nothing
end

struct ReverseWrapper{I<:Instruction} <: Function
    instruction::I
end

function (w::ReverseWrapper)()
    i = w.instruction
    Dub(Reverse(), i.genre, i.func)(i.cache, i.output, i.input...)
    return nothing
end

########
# Tape #
########

const ExecutionWrapper = FunctionWrapper{Void,Tuple{}}

struct Tape
    instructions::Vector{Instruction}
    forward::Vector{ExecutionWrapper}
    reverse::Vector{ExecutionWrapper}
    function Tape(instructions::Vector{Instruction})
        forward = [ExecutionWrapper(ForwardWrapper(instructions[i])) for i in 1:length(instructions)]
        reverse = [ExecutionWrapper(ReverseWrapper(instructions[i])) for i in length(instructions):-1:1]
        return new(instructions, forward, reverse)
    end
end

forward!(t::Tape) = (for f! in t.forward; f!(); end; nothing)
reverse!(t::Tape) = (for f! in t.reverse; f!(); end; nothing)
