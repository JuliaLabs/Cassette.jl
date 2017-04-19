abstract type AbstractInstruction end

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

###################
# Pretty Printing #
###################

compactrepr(x::Tuple, pad="") = "("*join(map(compactrepr, x), ",\n "*pad)*")"
compactrepr(x::AbstractArray, pad="") = length(x) < 5 ? match(r"\[.*?\]", repr(x)).match : summary(x)
compactrepr(x, pad="") = repr(x)

function Base.show(io::IO, op::Operation, pad = "")
    println(io, pad, "Operation:")
    argpad =         "          "
    println(io, pad, "  func:   ", op.func)
    println(io, pad, "  input:  ", compactrepr(op.input, argpad))
    println(io, pad, "  output: ", compactrepr(op.output, argpad))
    print(io,   pad, "  cache:  ", compactrepr(op.cache, argpad))
end
