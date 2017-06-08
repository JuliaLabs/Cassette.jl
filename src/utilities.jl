###########
# Untrack #
###########

struct Untrack{F} <: Function
    func::F
end

@inline Untrack(u::Untrack) = u

@inline (u::Untrack{<:Any})(a) = u.func(untrack(a))
@inline (u::Untrack{<:Any})(a, b) = u.func(untrack(a), untrack(b))
@inline (u::Untrack{<:Any})(a, b, c) = u.func(untrack(a), untrack(b), untrack(c))
@inline (u::Untrack{<:Any})(a, b, c, d) = u.func(untrack(a), untrack(b), untrack(c), untrack(d))
@inline (u::Untrack{<:Any})(a, b, c, d, e) = u.func(untrack(a), untrack(b), untrack(c), untrack(d), untrack(e))
@inline (u::Untrack{<:Any})(a, b, c, d, e, others...) = u.func(untrack(a), untrack(b), untrack(c), untrack(d), untrack(e), untrack.(others)...)

#########################
# Expression Generation #
#########################

interpolated_variable(x::ValueNote) = Symbol("x_" * idstring(untrack(x)))
interpolated_variable(x) = x

function toexpr(output::ValueNote)
    body = Expr(:block)
    args = Symbol[]
    rewind!(output) do x
        y = interpolated_variable(x)
        if !(isroot(x))
            p = x.parent
            push!(body.args, :($y = $(p.func)($(interpolated_variable.(p.input)...))))
        elseif isa(x, ValueNote)
            in(y, args) || push!(args, y)
        end
    end
    reverse!(body.args)
    reverse!(args)
    return args, body
end
