
abstract type Directive <: Function end

#############
# Intercept #
#############

struct Intercept{F} <: Directive
    func::F
end

@inline Intercept(d::Directive) = Intercept(unwrap(d))

@inline unwrap(i::Intercept) = i.func

@inline (i::Intercept)(input...) = Record(promote_genre(input...), i.func)(input...)

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

##########
# Record #
##########

struct Untrack{F} <: Directive
    func::F
end

@inline Untrack(d::Directive) = Untrack(unwrap(d))

@inline unwrap(u::Untrack) = u.func

@inline (u::Untrack)(a) where {G,F} = u.f(untrack(a))
@inline (u::Untrack)(a, b) where {G,F} = u.f(untrack(a), untrack(b))
@inline (u::Untrack)(a, b, c) where {G,F} = u.f(untrack(a), untrack(b), untrack(c))
@inline (u::Untrack)(a, b, c, d) where {G,F} = u.f(untrack(a), untrack(b), untrack(c), untrack(d))
@inline (u::Untrack)(a, b, c, d, e) where {G,F} = u.f(untrack(a), untrack(b), untrack(c), untrack(d), untrack(e))
@inline (u::Untrack)(a, b, c, d, e, others...) where {G,F} = u.f(untrack(a), untrack(b), untrack(c), untrack(d), untrack(e), untrack.(others)...)

##########
# Record #
##########

struct Record{G<:AbstractGenre,F} <: Directive
    genre::G
    func::F
end

@inline Record(genre::AbstractGenre, d::Directive) = Record(genre, unwrap(d))

@inline unwrap(r::Record) = r.func

# This doesn't specialize on DataType arguments naively, so we have to force specialization
# by unrolling access + type assertions via a generated function. This is pretty annoying
# since the naive method is so simple and clean otherwise...
@generated function (r::Record)(input...)
    typed_input = [:(input[$i]::$(input[i])) for i in 1:nfields(input)]
    return quote
        $(Expr(:meta, :inline))
        return track_if_possible(r, Untrack(r)($(typed_input...)), input)
    end
end

@inline track_if_possible(r::Record, output::Tuple, input) = map(o -> track_if_possible(r, o, input), output)
@inline track_if_possible(r::Record, output, input) = _track_if_possible(trackability(output), r, output, input)

@inline _track_if_possible(::NotTrackable, r::Record, output, input) = output
@inline _track_if_possible(::Any, r::Record, output, input) = track(output, r.genre, FunctionNode(r.func, input))
