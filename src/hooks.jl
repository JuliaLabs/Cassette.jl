#############
# Intercept #
#############

struct Intercept{F} <: Function
    func::F
end

@inline Intercept(i::Intercept) = i

@inline func(i::Intercept) = i.func

@inline function (i::Intercept)(input...)
    genre = promote_genre(input...)
    output, cache = Hook(Play(), genre, func(i))(input...)
    return handle_record(Hook(Record(), genre, func(i)), output, input, cache)
end

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

########
# Hook #
########

abstract type HookMode end

struct Play   <: HookMode end
struct Record <: HookMode end
struct Replay <: HookMode end
struct Rewind <: HookMode end

struct Hook{M<:HookMode,G<:AbstractGenre,F} <: Function
    mode::M
    genre::G
    func::F
end

@inline func(f::Hook) = f.func

#############################################
# handle_record (used by (::Intercept)(...) #
#############################################

@inline handle_record(h::Hook{Record}, output::NTuple{N,Any}, input::Tuple, cache) where {N} = NTuple{N}(h(o, input, cache) for o in output)
@inline handle_record(h::Hook{Record}, output, input::Tuple, cache) = handle_record(trackability(output), h, output, input, cache)
@inline handle_record(::TrackabilityTrait, h::Hook{Record}, output, input::Tuple, cache) = h(output, input, cache)
@inline handle_record(::NotTrackable, h::Hook{Record}, output, input::Tuple, cache) = output

#########################
# Hook{Play,ValueGenre} #
#########################

@inline (h::Hook{Play,ValueGenre})(input...) = (disarm(func(h))(input...), nothing)

###########################
# Hook{Record,ValueGenre} #
###########################

@inline (h::Hook{Record,ValueGenre})(output, input, ::Void) = track(output, FunctionNote{ValueGenre}(func(h), nothing, input))

###########################
# Hook{Replay,ValueGenre} #
###########################

@inline (h::Hook{Replay,ValueGenre})(output::RealNote, input::Tuple, ::Void) = value!(output, disarm(func(h))(input...))
@inline (h::Hook{Replay,ValueGenre})(output::ArrayNote, input::Tuple, ::Void) = copy!(value(output), disarm(func(h))(input...))

###########################
# Hook{Rewind,ValueGenre} #
###########################

# ValueGenre doesn't support `Rewind`.
