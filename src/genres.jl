abstract type AbstractGenre end

###############
# SyntaxGenre #
###############

struct SyntaxVariable
    id::UInt
    SyntaxVariable(x) = new(object_id(value(x)))
end

Base.show(io::IO, var::SyntaxVariable) = print(io, "var_", idstr(var.id))

struct SyntaxGenre <: AbstractGenre end

@inline remix(::SyntaxGenre, op::Operation) = Operation(op.func, SyntaxVariable.(op.input), SyntaxVariable.(op.output))

#############
# TypeGenre #
#############

struct TypedVariable
    var::SyntaxVariable
    typ::DataType
end

TypedVariable(x) = TypedVariable(SyntaxVariable(x), typeof(value(x)))

Base.show(io::IO, tvar::TypedVariable) = print(io, tvar.var, "::", tvar.typ)

struct TypeGenre <: AbstractGenre end

@inline remix(::TypeGenre, op::Operation) = Operation(op.func, TypedVariable.(op.input), TypedVariable.(op.output))

##############
# ValueGenre #
##############

struct ValueGenre <: AbstractGenre end

@inline remix(::ValueGenre, op::Operation) = Operation(op.func, capture.(op.input), capture.(op.output))

@inline capture(x) = x
