abstract type Directive <: Function end

###########
# Untrack #
###########

struct Untrack{F} <: Directive
    func::F
end

@inline Untrack(u::Untrack) = u

@inline (u::Untrack{<:Any})(a) = u.func(untrack(a))
@inline (u::Untrack{<:Any})(a, b) = u.func(untrack(a), untrack(b))
@inline (u::Untrack{<:Any})(a, b, c) = u.func(untrack(a), untrack(b), untrack(c))
@inline (u::Untrack{<:Any})(a, b, c, d) = u.func(untrack(a), untrack(b), untrack(c), untrack(d))
@inline (u::Untrack{<:Any})(a, b, c, d, e) = u.func(untrack(a), untrack(b), untrack(c), untrack(d), untrack(e))
@inline (u::Untrack{<:Any})(a, b, c, d, e, others...) = u.func(untrack(a), untrack(b), untrack(c), untrack(d), untrack(e), untrack.(others)...)

#############
# Intercept #
#############

struct Intercept{F} <: Directive
    func::F
end

@inline Intercept(i::Intercept) = i

@inline (i::Intercept)(input...) = Hook(Play(), promote_genre(input...), i.func)(input...)
@inline (i::Intercept)(::Type{T}) where {T} = Hook(Play(), genre(T), i.func)(T)

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
struct Dub    <: HookMode end
struct Replay <: HookMode end
struct Rewind <: HookMode end

struct Hook{M<:HookMode,G<:AbstractGenre,F} <: Directive
    mode::M
    genre::G
    func::F
end

##############
# Hook{Play} #
##############

@inline function (h::Hook{Play})(input...)
    record = Hook(Record(), h.genre, h.func)
    output = Untrack(h.func)(input...)
    return call_record(record, output, input)
end

# include this method just to force specialization for `DataType` arguments
@inline function (h::Hook{Play})(::Type{T}) where {T}
    record = Hook(Record(), h.genre, h.func)
    output = Untrack(h.func)(T)
    return call_record(record, output, tuple(T))
end

@inline call_record(h::Hook{Record}, output::NTuple{N}, input::Tuple, args...) where {N} = NTuple{N}(h(o, input, args...) for o in output)
@inline call_record(h::Hook{Record}, output, input::Tuple, args...) = call_record(trackability(output), h, output, input, args...)
@inline call_record(::TrackabilityTrait, h::Hook{Record}, output, input::Tuple, args...) = h(output, input, args...)
@inline call_record(::NotTrackable, h::Hook{Record}, output, input::Tuple, args...) = output

###########################
# Hook{Record,ValueGenre} #
###########################

@inline function (h::Hook{Record,ValueGenre})(output, input)
    output_note = track(output, h.genre)
    output_note.parent = FunctionNote(h.genre, h.func, input)
    return output_note
end

@inline (h::Hook{Record,ValueGenre})(output, ::Tuple{<:DataType})   = track(output, h.genre)
@inline (h::Hook{Record,ValueGenre,typeof(ones)})(output, input)    = track(output, h.genre)
@inline (h::Hook{Record,ValueGenre,typeof(zeros)})(output, input)   = track(output, h.genre)
@inline (h::Hook{Record,ValueGenre,typeof(copy)})(output, input)    = track(output, h.genre)
@inline (h::Hook{Record,ValueGenre,typeof(reshape)})(output, input) = track(output, h.genre)

########################
# Hook{Dub,ValueGenre} #
########################

(h::Hook{Dub,ValueGenre}(output, input) = Instruction(h.genre, h.func, input, output, nothing)

###########################
# Hook{Replay,ValueGenre} #
###########################

@inline function (h::Hook{Replay,ValueGenre})(output::RealNote, input::Tuple, cache)
    output.value = Untrack(r.func)(input...)
    return nothing
end

@inline function (h::Hook{Replay,ValueGenre})(output::ArrayNote, input::Tuple, cache)
    copy!(output.value, Untrack(r.func)(input...))
    return nothing
end

###########################
# Hook{Rewind,ValueGenre} #
###########################

# ValueGenre doesn't support `Rewind`.
