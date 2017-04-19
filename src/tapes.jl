########
# Tape #
########

struct Tape{G<:AbstractGenre}
    genre::G
    instructions::Vector{AbstractInstruction}
end

Tape(genre::AbstractGenre) = Tape(genre, Vector{AbstractInstruction}())

function Base.push!(tape::Tape, instruction::AbstractInstruction)
    push!(tape.instructions, instruction)
    return tape
end

Base.length(tape::Tape) = length(tape.instructions)

#################
# Tape Matching #
#################

macro match(tapes...)
    expr = Expr(:block, Any[])
    a = tapes[1]
    for i in 2:length(tapes)
        b = tapes[i]
        push!(expr.args, :($a === $b || throw(TapeMismatchError($a, $b)))
    end
    push!(expr.args, a)
    return esc(expr)
end

struct TapeMismatchError{A<:Tape,B<:Tape} <: Exception
    a::A
    b::B
end

Base.showerror(io::IO, e::TapeMismatchError) = print(io, "Cassette does now allow mixed-tape operations (Tape<$(idstr(e.a))> !== Tape<$(idstr(e.b))>)")

###################
# Pretty Printing #
###################

function Base.show(io::IO, tape::Tape{G}) where G
    println("$(length(tape))-element Tape{$G}<$(idstr(tape))>:")
    i = 1
    for instruction in tape.instructions
        print(io, "$i => ")
        show(io, instruction)
        println(io)
        i += 1
    end
end
