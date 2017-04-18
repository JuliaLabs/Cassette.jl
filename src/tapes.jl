##################
# abstract types #
##################

abstract type AbstractInstruction end
abstract type AbstractGenre end
abstract type AbstractTape{G<:AbstractGenre} end

#########
# tapes #
#########

struct Tape{G<:AbstractGenre} <: AbstractTape{G}
    genre::G
    instructions::Vector{AbstractInstruction}
end

Tape(genre::AbstractGenre) = Tape(genre, Vector{AbstractInstruction}())

function Base.push!(tape::Tape, instruction::AbstractInstruction)
    push!(tape.instructions, instruction)
    return tape
end

Base.length(tape::Tape) = length(tape.instructions)

function record!(tape::Tape, f::F, input...) where F
    output = track(Skip(f)(input...), tape)
    push!(tape, edit(tape.genre, Operation(f, input, output)))
    return output
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

struct SyntaxGenre <: AbstractGenre end

struct SyntaxVariable
    id::UInt
    SyntaxVariable(x) = new(object_id(value(x)))
end

Base.show(io::IO, var::SyntaxVariable) = print(io, "var_", idstr(var.id))

edit(op::Operation, ::SyntaxGenre) = Operation(op.func, SyntaxVariable.(op.input), SyntaxVariable.(op.output))

# TypeGenre #
#-----------#

struct TypeGenre <: AbstractGenre end

struct TypedVariable{T}
    var::SyntaxVariable
end

TypedVariable(x) = TypedVariable{typeof(value(x))}(SyntaxVariable(x))

Base.show(io::IO, tvar::TypedVariable{T}) where {T} = print(io, tvar.var, "::", T)

edit(op::Operation, ::TypeGenre) = Operation(op.func, TypedVariable.(op.input), TypedVariable.(op.output))

# ValueGenre #
#------------#

struct ValueGenre <: AbstractGenre end

edit(op::Operation, ::ValueGenre) = Operation(op.func, capture.(op.input), capture.(op.output))

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

###################
# Pretty Printing #
###################

# extra spaces here accomodates padding in show(::IO, ::Operation)
compactrepr(x::Tuple, pad="") = "("*join(map(compactrepr, x), ",\n "*pad)*")"
compactrepr(x::AbstractArray, pad="") = length(x) < 5 ? match(r"\[.*?\]", repr(x)).match : summary(x)
compactrepr(x, pad="") = repr(x)

function Base.show(io::IO, instruction::Operation, pad = "")
    println(io, pad, "Operation:")
    argpad =         "          "
    println(io, pad, "  func:   ", instruction.func)
    println(io, pad, "  input:  ", compactrepr(instruction.input, argpad))
    println(io, pad, "  output: ", compactrepr(instruction.output, argpad))
    print(io,   pad, "  cache:  ", compactrepr(instruction.cache, argpad))
end

function Base.show(io::IO, tape::Tape{G}) where G
    println("$(length(tape))-element Tape{$G}:")
    i = 1
    for instruction in tape.instructions
        print(io, "$i => ")
        show(io, instruction)
        println(io)
        i += 1
    end
end
