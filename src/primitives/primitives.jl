#############
# Primitive #
#############

struct Primitive{F} <: Function
    func::F
end

@inline Primitive(p::Primitive) = Primitive(p.func)

@inline (p::Primitive)(input...) = ForwardExecute(promote_genre(input...), p.func)(input...)

#=
works for the following formats:
- `@primitive(f)(args...)`
- `@primitive f(args...) = ...`
- `@primitive function f(args...) ... end`
- `@primitive f = (args...) -> ...`
=#
macro primitive(expr)
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
                error("failed to apply Cassette.Primitive to expression $(expr); potentially malformed function signature?")
            end
            result = quote
                $expr
                if !(isdefined($(Expr(:quote, old_name))))
                    const $(old_name) = $(Primitive)($(hidden_name))
                end
            end
        elseif isa(lhs, Symbol) # variable assignment site
            expr.args[2] = :($(Primitive)($(expr.args[2])))
            result = expr
        else
            error("failed to apply Cassette.Primitive to expression $expr")
        end
    else # call site
        result = :($(Primitive)($expr))
    end
    return esc(result)
end

##################
# ForwardExecute #
##################

struct ForwardExecute{G,F}
    genre::G
    func::F
end

@inline (e::ForwardExecute)(input...) = error("ForwardExecute is not yet defined for genre $(e.genre) and function $(e.func).")

#################################
# ForwardExecute for ValueGenre #
#################################

@inline untrack_call(f, a) = f(untrack(a))
@inline untrack_call(f, a, b) = f(untrack(a), untrack(b))
@inline untrack_call(f, a, b, c) = f(untrack(a), untrack(b), untrack(c))
@inline untrack_call(f, a, b, c, d) = f(untrack(a), untrack(b), untrack(c), untrack(d))
@inline untrack_call(f, a, b, c, d, e) = f(untrack(a), untrack(b), untrack(c), untrack(d), untrack(e))
@inline untrack_call(f, args...) = f(untrack.(args)...)

# general functions #
#-------------------#

@inline function (e::ForwardExecute{ValueGenre})(input::Union{Real,AbstractArray}...)
    output = untrack_call(e.func, input...)
    return maybe_track_output(e, output, input, TrackableTrait(output))
end

@inline maybe_track_output(e::ForwardExecute{ValueGenre}, output, input, ::Trackable) = track(output, e.genre, FunctionNode(e.func, input))
@inline maybe_track_output(e::ForwardExecute{ValueGenre}, output, input, ::NotTrackable) = output

# specific functions #
#--------------------#
