##################
# abstract types #
##################

abstract type AbstractInstruction end
abstract type AbstractGenre end
abstract type AbstractTape{G<:AbstractGenre} end

##########
# genres #
##########

struct SyntaxGenre <: AbstractGenre end
struct TypeGenre   <: AbstractGenre end
struct ValueGenre  <: AbstractGenre end

#########
# tapes #
#########

immutable Tape{G<:AbstractGenre} <: AbstractTape{G}
    genre::G
    instructions::Vector{AbstractInstruction}
end

Tape(genre::AbstractGenre) = Tape(genre, Vector{AbstractInstruction}())

function record!(tape::Tape, instruction::AbstractInstruction)
    push!(tape.instructions, specialize(instruction, tape.genre))
    return tape
end

########################
# OperationInstruction #
########################

struct OperationInstruction{F,I,O,C} <: AbstractInstruction
    func::F
    input::I
    output::O
    cache::C
end

OperationInstruction(func, input, output) = OperationInstruction(func, input, output, nothing)

# SyntaxGenre specialization #
#----------------------------#

struct SyntaxVariable
    id::UInt
    SyntaxVariable(x) = new(object_id(x))
end

function Base.show(io::IO, var::SyntaxVariable)
    return print(io "var_", string(base(62, object_id(var.id)))[1:3])
end

function specialize(i::OperationInstruction, ::SyntaxGenre)
    return OperationInstruction(i.func,
                                SyntaxVariable.(i.input),
                                SyntaxVariable.(i.output))
end

# TypeGenre specialization #
#--------------------------#

function specialize(i::OperationInstruction, ::TypeGenre)
    # TODO: Should this "un-track" any Tracked* types in the input/output?
    return OperationInstruction(typeof(i.func),
                                typeof.(i.input),
                                typeof.(i.output))
end

# value recording #
#-----------------#

# This can be overloaded to ensure that external state is "captured",
# such that external reference-breaking (e.g. destructive assignment)
# doesn't break internal instruction state. By default, `capture` is
# a no-op.
@inline capture(state) = state

# This can be overloaded for specific functions which need to
# allocate memory used in forward/reverse execution passes.
@inline value_cache(f, input, output) = nothing

function specialize(i::OperationInstruction, ::ValueGenre)
    return OperationInstruction(i.func,
                                capture.(i.input),
                                capture.(i.output),
                                value_cache(i.func, i.input, i.output))
end

####################
# MergeInstruction #
####################

struct MergeInstruction{G<:AbstractGenre,N} <: AbstractInstruction
    tapes::NTuple{N,Tape{G}}
end

@inline specialize(i::MergeInstruction{G}, ::G) where {G} = i
