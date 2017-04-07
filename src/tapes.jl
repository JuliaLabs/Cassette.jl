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
    push!(tape.instructions, instruction)
    return tape
end

#############
# Operation #
#############

struct Operation{F,I,O,C} <: AbstractInstruction
    func::F
    input::I
    output::O
    cache::C
end

Operation(func, input, output) = Operation(func, input, output, nothing)

# SyntaxGenre #
#-------------#

struct SyntaxVariable
    id::UInt
    SyntaxVariable(x) = new(object_id(x))
end

function Base.show(io::IO, var::SyntaxVariable)
    return print(io, "var_", string(base(62, object_id(var.id)))[1:3])
end

function record!(tape::Tape{SyntaxGenre}, f::F, input...) where F
    output = track(Skip(f)(input...), tape)
    record!(tape, Operation(f, SyntaxVar.(input), SyntaxVar.(output)))
    return output
end

# TypeGenre #
#-----------#

function record!(tape::Tape{TypeGenre}, f::F, input...) where F
    output = track(Skip(f)(input...), tape)
    record!(tape, Operation(typeof(f), typeof.(input), typeof.(output)))
    return output
end

# ValueGenre #
#------------#

function record!(tape::Tape{ValueGenre}, f::F, input...) where F
    output = track(Skip(f)(input...), tape)
    record!(tape, Operation(f, capture.(input), capture.(output)))
    return output
end

####################
# MergeInstruction #
####################

struct TapeMerge{G<:AbstractGenre,N} <: AbstractInstruction
    tapes::NTuple{N,Tape{G}}
end

function Base.merge(a::Tape{G}, b::Tape{G}) where G
    if a === b
        return a::Tape{G}
    else
        tape = Tape(G())
        record!(tape, TapeMerge(a, b))
        return tape::Tape{G}
    end
end

function Base.merge(a::Tape{G}, b::Tape{G}, c::Tape{G}) where G
    if a === b && b === c
        return a::Tape{G}
    else
        tape = Tape(G())
        if a === b
            record!(tape, TapeMerge(a, c))
        elseif b === c
            record!(tape, TapeMerge(a, b))
        else
            record!(tape, TapeMerge(a, b, c))
        end
        return tape::Tape{G}
    end
end

function Base.merge(tapes::Tape{G}...) where G
    return merge(merge(tapes[1], tapes[2], tapes[3]), tapes[2:end]...)::Tape{G}
end
