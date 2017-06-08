##########
# Disarm #
##########

struct Disarm{F} <: Function
    func::F
end

@inline Disarm(d::Disarm) = d

@inline disarm(f) = Disarm(f)

@inline func(d::Disarm) = d.func

@inline (d::Disarm{<:Any})(a) = func(d)(untrack(a))
@inline (d::Disarm{<:Any})(a, b) = func(d)(untrack(a), untrack(b))
@inline (d::Disarm{<:Any})(a, b, c) = func(d)(untrack(a), untrack(b), untrack(c))
@inline (d::Disarm{<:Any})(a, b, c, d) = func(d)(untrack(a), untrack(b), untrack(c), untrack(d))
@inline (d::Disarm{<:Any})(a, b, c, d, e) = func(d)(untrack(a), untrack(b), untrack(c), untrack(d), untrack(e))
@inline (d::Disarm{<:Any})(a, b, c, d, e, others...) = func(d)(untrack(a), untrack(b), untrack(c), untrack(d), untrack(e), untrack.(others)...)

#########################
# Expression Generation #
#########################

interpolated_variable(x::ValueNote) = Symbol("x_" * idstring(value(x)))
interpolated_variable(x) = x

function toexpr(output::ValueNote)
    body = Expr(:block)
    args = Symbol[]
    rewind!(output) do x
        y = interpolated_variable(x)
        if !(isroot(x))
            p = parent(x)
            push!(body.args, :($y = $(value(p))($(interpolated_variable.(parent(p)...))))
        elseif isa(x, ValueNote)
            in(y, args) || push!(args, y)
        end
    end
    reverse!(body.args)
    reverse!(args)
    return args, body
end
