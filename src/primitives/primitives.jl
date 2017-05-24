#############
# Primitive #
#############

struct Primitive{G,F} <: Function
    genre::G
    func::F
end

###############################
# Default Primitive Execution #
###############################

@inline untrack_call(f::F, a) where {F} = f(untrack(a))
@inline untrack_call(f::F, a, b) where {F} = f(untrack(a), untrack(b))
@inline untrack_call(f::F, a, b, c) where {F} = f(untrack(a), untrack(b), untrack(c))
@inline untrack_call(f::F, a, b, c, d) where {F} = f(untrack(a), untrack(b), untrack(c), untrack(d))
@inline untrack_call(f::F, a, b, c, d, e) where {F} = f(untrack(a), untrack(b), untrack(c), untrack(d), untrack(e))
@inline untrack_call(f::F, a, b, c, d, e, others...) where {F} = f(untrack(a), untrack(b), untrack(c), untrack(d), untrack(e), untrack.(others)...)

# This doesn't specialize on DataType arguments naively, so we have to force specialization
# by unrolling access + type assertions via a generated function. This is pretty annoying
# since the naive method is so simple otherwise...
@generated function (p::Primitive)(input...)
    typed_input = [:(input[$i]::$(input[i])) for i in 1:nfields(input)]
    return quote
        $(Expr(:meta, :inline))
        output = untrack_call(p.func, $(typed_input...))
        return track_if_possible(p, output, input)
    end
end

@inline track_if_possible(p::Primitive, output, input) = conditional_track(istrackable(output), p, output, input)

@inline conditional_track(::True,  p::Primitive, output, input) = track(output, p.genre, FunctionNode(p.func, input))
@inline conditional_track(::False, p::Primitive, output, input) = output

#############
# Intercept #
#############

struct Intercept{F} <: Function
    func::F
end

@inline Intercept(i::Intercept) = Intercept(i.func)

@inline (i::Intercept)(input...) = Primitive(promote_genre(input...), i.func)(input...)

#=
works for the following formats:
- `@intercept(f)(args...)`
- `@intercept f(args...) = ...`
- `@intercept function f(args...) ... end`
- `@intercept f = (args...) -> ...`
=#
macro intercept(expr)
    if isa(expr, Expr) && (expr.head == :(=) || expr.head == :function)
        lhs = expr.args[1]
        if isa(lhs, Expr) && lhs.head == :call # named function definition site
            name_and_types = lhs.args[1]
            if isa(name_and_types, Expr) && name_and_types.head == :curly
                old_name = name_and_types.args[1]
                hidden_name = Symbol("#cassette_hidden_$(old_name)")
                name_and_types.args[1] = hidden_name
            elseif isa(name_and_types, Symbol)
                old_name = name_and_types
                hidden_name = Symbol("#cassette_hidden_$(old_name)")
                lhs.args[1] = hidden_name
            else
                error("failed to apply Cassette.@intercept to expression $(expr); potentially malformed function signature?")
            end
            result = quote
                $expr
                if !(isdefined($(Expr(:quote, old_name))))
                    const $(old_name) = $(Intercept)($(hidden_name))
                end
            end
        elseif isa(lhs, Symbol) # variable assignment site
            expr.args[2] = :($(Intercept)($(expr.args[2])))
            result = expr
        else
            error("failed to apply Cassette.@intercept to expression $expr")
        end
    else # call site
        result = :($(Intercept)($expr))
    end
    return esc(result)
end
