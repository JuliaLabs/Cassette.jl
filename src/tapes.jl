###############
# Instruction #
###############

abstract type AbstractExecution end

struct BasicExecution <: AbstractExecution end

struct Instruction{E<:AbstractExecution,F,I,O,C}
    exec::E
    func::F
    input::I
    output::O
    cache::C
    function (::Type{Instruction{E,F,I,O,C}}){E,F,I,O,C}(exec::E,
                                                         func::F,
                                                         input::I,
                                                         output::O,
                                                         cache::C)
        return new{E,F,I,O,C}(exec, func, input, output, cache)
    end
end

function Instruction(exec::E,
                     func::F,
                     input,
                     output,
                     cache::C = nothing) where {E,F,C}
    captured_input = capture(input)
    captured_output = capture(output)
    I = typeof(captured_input)
    O = typeof(captured_output)
    return Instruction{E,F,I,O,C}(exec, func,
                                  captured_input,
                                  captured_output,
                                  cache)
end

# Ensure that the external state is "captured" so that external
# reference-breaking (e.g. destructive assignment) doesn't break
# internal instruction state. By default, `capture` is a no-op.
# Later, we'll add `capture` methods to `Tracked*` types.
@inline capture(state) = state
@inline capture(state::Tuple) = map(capture, state)

function Base.:(==)(a::Instruction, b::Instruction)
    return (a.exec == b.exec &&
            a.func == b.func &&
            a.input == b.input &&
            a.output == b.output &&
            a.cache == b.cache)
end

########
# Tape #
########

const Tape = Vector{Instruction}

const DISABLED = Tape()

function record!(tape::Tape, args...)
    tape !== DISABLED && push!(tape, Instruction(args...))
    return nothing
end

#############
# execution #
#############

function forward_execute!(tape::Tape)
    for instruction in tape
        forward_execute!(instruction)
    end
    return nothing
end

@noinline function forward_execute!(i::Instruction)
    return forward_execute!(i.exec, i.func, i.input, i.output, i.cache)
end

function reverse_execute!(tape::Tape)
    for i in length(tape):-1:1
        reverse_execute!(tape[i])
    end
    return nothing
end

@noinline function reverse_execute!(i::Instruction)
    return reverse_execute!(i.exec, i.func, i.input, i.output, i.cache)
end
