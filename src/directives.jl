##############
# Directives #
##############

abstract type Directive{G<:AbstractGenre,F} end

macro defdirective(D)
    return esc(quote
        struct $D{G<:$AbstractGenre,F} <: $Directive{G,F}
            func::F
            @inline $D{G}(func::F) where {G<:$AbstractGenre,F}  = new{G,F}(func)
        end
    end)
end

@inline func(d::Directive) = d.func

@inline genre(d::Directive{G}) where {G} = G()
@inline genre(::Type{Directive{G,F}}) where {G,F} = G()

@defdirective Play
@defdirective Record
@defdirective Replay
@defdirective Rewind

####################
# Play{ValueGenre} #
####################

@inline (p::Play{ValueGenre})(input...) = disarm(func(p))(input...)

######################
# Record{ValueGenre} #
######################

@inline (r::Record{ValueGenre})(output, input) = track(output, FunctionNote{ValueGenre}(func(r), input))

######################
# Replay{ValueGenre} #
######################

@inline (r::Replay{ValueGenre})(output::RealNote, input::Tuple, parent) = value!(output, disarm(func(r))(input...))
@inline (r::Replay{ValueGenre})(output::ArrayNote, input::Tuple, parent) = copy!(value(output), disarm(func(r))(input...))

######################
# Rewind{ValueGenre} #
######################

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
    G = typeof(promote_genre(input...))
    results = Play{G}(func(i))(input...)
    return process_record(Record{G}(func(i)), input, results)
end

@inline process_record(r::Record, input::Tuple, results::Tuple{O,C}) where {O,C<:Cache} = _process_record(r, input, results[1]::O, results[2]::C)
@inline process_record(r::Record, input::Tuple, results) = _process_record(r, input, results)

@inline _process_record(r::Record, input::Tuple, output::NTuple{N,Any}, cache::Cache...) where {N} = NTuple{N}(_process_record(r, input, o, cache...) for o in output)
@inline _process_record(r::Record, input::Tuple, output, cache::Cache...) = _process_record(trackability(output), r, input, output, cache...)
@inline _process_record(::TrackabilityTrait, r::Record, input::Tuple, output, cache::Cache) = r(output, input, cache[])
@inline _process_record(::TrackabilityTrait, r::Record, input::Tuple, output) = r(output, input)
@inline _process_record(::NotTrackable, r::Record, input::Tuple, output, cache::Cache...) = output

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
