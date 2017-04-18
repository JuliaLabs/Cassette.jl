##############
# Annotation #
##############

abstract type Annotation <: Function end

########
# Skip #
########

struct Skip{F} <: Annotation
    f::F
end

@inline Skip(f::Skip) = f

@inline (skip::Skip{F}){F}(a) = skip.f(value(a))
@inline (skip::Skip{F}){F}(a, b) = skip.f(value(a), value(b))
@inline (skip::Skip{F}){F}(a, b, c) = skip.f(value(a), value(b), value(c))
@inline (skip::Skip{F}){F}(a, b, c, d) = skip.f(value(a), value(b), value(c), value(d))
@inline (skip::Skip{F}){F}(args...) = skip.f(value.(args)...)

macro skip(f)
    return esc(annotate(Skip, f))
end

#############
# utilities #
#############

#=
works for the following formats:
- `@annotation(f)(args...)`
- `@annotation f(args...) = ...`
- `@annotation f = (args...) -> ...`
=#
function annotate(::Type{T}, expr) where T<:Annotation
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
