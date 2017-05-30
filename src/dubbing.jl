###########
# Untrack #
###########

struct Untrack{F} <: Function
    func::F
end

@inline (u::Untrack{F})(a) where {F} = u.func(untrack(a))
@inline (u::Untrack{F})(a, b) where {F} = u.func(untrack(a), untrack(b))
@inline (u::Untrack{F})(a, b, c) where {F} = u.func(untrack(a), untrack(b), untrack(c))
@inline (u::Untrack{F})(a, b, c, d) where {F} = u.func(untrack(a), untrack(b), untrack(c), untrack(d))
@inline (u::Untrack{F})(a, b, c, d, e) where {F} = u.func(untrack(a), untrack(b), untrack(c), untrack(d), untrack(e))
@inline (u::Untrack{F})(a, b, c, d, e, others...) where {F} = u.func(untrack(a), untrack(b), untrack(c), untrack(d), untrack(e), untrack.(others)...)

#############
# Intercept #
#############

struct Intercept{F} <: Function
    func::F
end

@inline Intercept(i::Intercept) = i

@inline (i::Intercept)(input...) = Dub(Record(), promote_genre(input...), i.func)(input...)

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

#######
# Dub #
#######

abstract type DubMode end

struct Dub{M<:DubMode,G<:AbstractGenre,F} <: Function
    mode::M
    genre::G
    func::F
end

# Record #
#--------#

struct Record <: DubMode end

#=
This doesn't specialize on DataType arguments naively, so we have to force specialization
by unrolling access + type assertions via a generated function. This is pretty annoying
since the naive method is so simple and clean otherwise:

@inline (d::Dub{Record})(input...) = track_if_possible(d, Untrack(d.func)(input...), input)

=#
@generated function (d::Dub{Record})(input...)
    typed_input = [:(input[$i]::$(input[i])) for i in 1:nfields(input)]
    return quote
        $(Expr(:meta, :inline))
        return track_if_possible(d, Untrack(d.func)($(typed_input...)), input)
    end
end

@inline track_if_possible(d::Dub, output::Tuple, input) = map(o -> track_if_possible(d, o, input), output)
@inline track_if_possible(d::Dub, output, input) = _track_if_possible(d, output, input, trackability(output))

@inline _track_if_possible(d::Dub, output, input, ::NotTrackable) = output
@inline _track_if_possible(d::Dub, output, input, ::Any) = track(output, d.genre, FunctionNode(d.func, input))

# Forward #
#---------#

struct Forward <: DubMode end

function (d::Dub{Forward,ValueGenre})(cache, output::ValueNode, input...)
    output.value = Untrack(d.func)(input...)
    return nothing
end

# Reverse #
#---------#

struct Reverse <: DubMode end
