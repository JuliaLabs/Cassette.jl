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

@inline (i::Intercept)(input...) = Transport(Play(), promote_genre(input...), i.func)(input...)
@inline (i::Intercept)(::Type{T}) where {T} = Transport(Play(), genre(T), i.func)(T)

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

#############
# Transport #
#############

abstract type TransportMode end

struct Play      <: TransportMode end
struct Record    <: TransportMode end
struct Dub       <: TransportMode end
struct Replay{D} <: TransportMode end

struct Transport{M<:TransportMode,G<:AbstractGenre,F} <: Directive
    mode::M
    genre::G
    func::F
end

# Play Mode #
#-----------#

@inline function (t::Transport{Play})(input...)
    t = Transport(Record(), t.genre, t.func)
    output = Untrack(t.func)(input...)
    return call_record(t, output, input)
end

# include this method just to force specialization for `DataType` arguments
@inline function (t::Transport{Play})(::Type{T}) where {T}
    t = Transport(Record(), t.genre, t.func)
    output = Untrack(t.func)(T)
    return call_record(t, output, tuple(T))
end

@inline call_record(t::Transport{Record}, output::NTuple{N}, input::Tuple, args...) where {N} = NTuple{N}(t(o, input, args...) for o in output)
@inline call_record(t::Transport{Record}, output, input::Tuple, args...) = call_record(trackability(output), t, output, input, args...)
@inline call_record(::TrackabilityTrait, t::Transport{Record}, output, input::Tuple, args...) = t(output, input, args...)
@inline call_record(::NotTrackable, t::Transport{Record}, output, input::Tuple, args...) = output

# Record Mode #
#-------------#

@inline function (t::Transport{Record,ValueGenre})(output, input)
    output_note = track(output, t.genre)
    output_note.parent = FunctionNote(t.genre, t.func, input)
    return output_note
end

@inline (t::Transport{Record,ValueGenre})(output, ::Tuple{<:DataType})   = track(output, t.genre)
@inline (t::Transport{Record,ValueGenre,typeof(ones)})(output, input)    = track(output, t.genre)
@inline (t::Transport{Record,ValueGenre,typeof(zeros)})(output, input)   = track(output, t.genre)
@inline (t::Transport{Record,ValueGenre,typeof(copy)})(output, input)    = track(output, t.genre)
@inline (t::Transport{Record,ValueGenre,typeof(reshape)})(output, input) = track(output, t.genre)

# Dub Mode #
#----------#

(t::Transport{Dub,ValueGenre}(output, input) = Instruction(t.genre, t.func, t.input, t.output, nothing)


@inline function (t::Transport{Record,ValueGenre})(output, input)
    output_note = track(output, t.genre)
    output_note.parent = FunctionNote(t.genre, t.func, input)
    return output_note
end

@inline (t::Transport{Record,ValueGenre})(output, ::Tuple{<:DataType})   = track(output, t.genre)
@inline (t::Transport{Record,ValueGenre,typeof(ones)})(output, input)    = track(output, t.genre)
@inline (t::Transport{Record,ValueGenre,typeof(zeros)})(output, input)   = track(output, t.genre)
@inline (t::Transport{Record,ValueGenre,typeof(copy)})(output, input)    = track(output, t.genre)
@inline (t::Transport{Record,ValueGenre,typeof(reshape)})(output, input) = track(output, t.genre)
