#############
# Directive #
#############

abstract type Directive <: Function end

#=
works for the following formats:
- `@annotation(f)(args...)`
- `@annotation f(args...) = ...`
- `@annotation f = (args...) -> ...`
=#
function annotate(::Type{T}, expr) where T<:Directive
    if isa(expr, Expr) && (expr.head == :(=) || expr.head == :function)
        lhs = expr.args[1]
        if isa(lhs, Expr) && lhs.head == :call # named function definition site
            name_and_types = lhs.args[1]
            if isa(name_and_types, Expr) && name_and_types.head == :curly
                old_name = name_and_types.args[1]
                hidden_name = Symbol("#hidden_$(old_name)")
                name_and_types.args[1] = hidden_name
            elseif isa(name_and_types, Symbol)
                old_name = name_and_types
                hidden_name = Symbol("#hidden_$(old_name)")
                lhs.args[1] = hidden_name
            else
                error("potentially malformed function signature for $T")
            end
            return quote
                $expr
                if !(isdefined($(Expr(:quote, old_name))))
                    const $(old_name) = $(T)($(hidden_name))
                end
            end
        elseif isa(lhs, Symbol) # variable assignment site
            expr.args[2] = :($(T)($(expr.args[2])))
            return expr
        else
            error("failed to apply $T to expression $expr")
        end
    else # call site
        return :($(T)($expr))
    end
end

########
# Skip #
########

struct Skip{F} <: Directive
    func::F
end

@inline Skip(d::Directive) = Skip(unwrap(d))

@inline unwrap(s::Skip) = s.func

macro skip(f)
    return esc(annotate(Skip, f))
end

@inline (s::Skip)(a) = unwrap(s)(value(a))
@inline (s::Skip)(a, b) = unwrap(s)(value(a), value(b))
@inline (s::Skip)(a, b, c) = unwrap(s)(value(a), value(b), value(c))
@inline (s::Skip)(a, b, c, d) = unwrap(s)(value(a), value(b), value(c), value(d))
@inline (s::Skip)(args...) = unwrap(s)(value.(args)...)

##########
# Record #
##########

struct Record{F} <: Directive
    func::F
end

@inline Record(d::Directive) = Record(unwrap(d))

@inline unwrap(r::Record) = r.func

macro record(f)
    return esc(annotate(Record, f))
end

@inline (r::Record)(args...) = Skip(r)(args...)

# unary #
#-------#

@inline function (r::Record)(a::Tracked)
    t = tape(a)
    b = track(Skip(r)(a), t)
    op = remix(genre(t), Operation(unwrap(r), a, b))
    push!(t, op)
    return b
end

# binary #
#--------#

for A in (:Any, :Tracked), B in (:Any, :Tracked)
    A === B && A === Any && continue
    @eval begin
        @inline function (r::Record)(a::$A, b::$B)
            t = tape(a, b)
            c = track(Skip(r)(a, b), t)
            op = remix(genre(t), Operation(unwrap(r), (a, b), c))
            push!(t, op)
            return c
        end
    end
end

# ternary #
#---------#

for A in (:Any, :Tracked), B in (:Any, :Tracked), C in (:Any, :Tracked)
    A === B && B === C && A === Any && continue
    @eval begin
        @inline function (r::Record)(a::$A, b::$B, c::$C)
            t = tape(a, b, c)
            d = track(Skip(r)(a, b, c), t)
            op = remix(genre(t), Operation(unwrap(r), (a, b, c), d))
            push!(t, op)
            return d
        end
    end
end
