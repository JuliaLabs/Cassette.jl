########
# Hook #
########

abstract type HookMode end

struct Play   <: HookMode end
struct Record <: HookMode end
struct Replay <: HookMode end
struct Rewind <: HookMode end

struct Hook{G<:AbstractGenre,M<:HookMode,F} <: Function
    func::F
    Hook{G,M}(func::F) where {G,M,F} = new{G,M,F}(mode, func)
end

@inline Hook(genre::G, mode::M, func) where {G<:AbstractGenre,M<:HookMode} = Hook{G,M}(func)

@inline func(f::Hook) = f.func

#########################
# Hook{ValueGenre,Play} #
#########################

@inline (h::Hook{ValueGenre,Play})(input...) = disarm(func(h))(input...)

###########################
# Hook{ValueGenre,Record} #
###########################

@inline (h::Hook{ValueGenre,Record})(output, input) = track(output, FunctionNote{ValueGenre}(func(h), input))

###########################
# Hook{ValueGenre,Replay} #
###########################

@inline (h::Hook{ValueGenre,Replay})(output::RealNote, input::Tuple, parent) = value!(output, disarm(func(h))(input...))
@inline (h::Hook{ValueGenre,Replay})(output::ArrayNote, input::Tuple, parent) = copy!(value(output), disarm(func(h))(input...))

###########################
# Hook{ValueGenre,Rewind} #
###########################

# ValueGenre doesn't support `Rewind`.

#########
# Cache #
#########

struct Cache{T}
    data::T
end

@inline Base.getindex(cache::Cache) = cache.data

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
    results = Hook(genre, Play(), func(i))(input...)
    return init_and_call_record_hook(genre, func(i), input, results)
end

@inline function init_and_call_record_hook(genre::AbstractGenre, f::F, input::Tuple, results::Tuple{O,C}) where {F,O,C<:Cache}
    output::O, cache::C = results
    return call_record_hook(Hook(genre, Record(), f), input, output, cache)
end

@inline function init_and_call_record_hook(genre::AbstractGenre, f::F, input::Tuple, output) where {F}
    return call_record_hook(Hook(genre, Record(), f), input, output)
end

@inline function call_record_hook(h::Hook, input::Tuple, output::NTuple{N,Any}, cache::Cache...) where {N}
    return NTuple{N}(call_record_hook(h, input, o, cache...) for o in output)
end

@inline function call_record_hook(h::Hook, input::Tuple, output, cache::Cache...)
    return _call_record_hook(trackability(output), h, input, output, cache...)
end

@inline _call_record_hook(::TrackabilityTrait, h::Hook, input::Tuple, output, cache::Cache) = h(output, input, cache[])

@inline _call_record_hook(::TrackabilityTrait, h::Hook, input::Tuple, output) = h(output, input)

@inline _call_record_hook(::NotTrackable, h::Hook, input::Tuple, output, cache::Cache...) = output

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
