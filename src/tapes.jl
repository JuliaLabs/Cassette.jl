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
# Execution Directives #
########################

const ExecutionWrapper = FunctionWrapper{Void,Tuple{}}

# instruction wrappers #
#----------------------#

struct ForwardWrapper{I<:Instruction} <: Directive
    instruction::I
end

function (w::ForwardWrapper)()
    i = w.instruction
    ForwardExecute!(i.genre, i.func)(i.cache, i.output, i.input...)
    return nothing
end

struct ReverseWrapper{I<:Instruction} <: Directive
    instruction::I
end

function (w::ReverseWrapper)()
    i = w.instruction
    ReverseExecute!(i.genre, i.func)(i.cache, i.output, i.input...)
    return nothing
end

# execution directives #
#----------------------#

struct ForwardExecute!{G,F} <: Directive
    genre::G
    func::F
end

struct ReverseExecute!{G,F} <: Directive
    genre::G
    func::F
end

function (e::ForwardExecute!{ValueGenre})(cache, output::ValueNode, input...)
    output.value = Untrack(e.f)(input...)
    return nothing
end

########
# Tape #
########

struct Tape
    instructions::Vector{Instruction}
    forward::Vector{ExecutionWrapper}
    reverse::Vector{ExecutionWrapper}
    function Tape(instructions::Vector{Instruction})
        forward = [ExecutionWrapper(ForwardWrapper(i)) for i in instructions]
        reverse = [ExecutionWrapper(ReverseWrapper(instructions[k])) for k in length(instructions):-1:1]
        return new(instructions, forward, reverse)
    end
end

forward!(t::Tape) = (for f! in t.forward; f!(); end; nothing)
reverse!(t::Tape) = (for f! in t.reverse; f!(); end; nothing)
