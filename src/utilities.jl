##########
# Disarm #
##########

struct Disarm{F} <: Function
    func::F
end

@inline Disarm(d::Disarm) = d

@inline disarm(f) = Disarm(f)

@inline func(d::Disarm) = d.func

@inline (f::Disarm{<:Any})(a) = func(f)(value(a))
@inline (f::Disarm{<:Any})(a, b) = func(f)(value(a), value(b))
@inline (f::Disarm{<:Any})(a, b, c) = func(f)(value(a), value(b), value(c))
@inline (f::Disarm{<:Any})(a, b, c, d) = func(f)(value(a), value(b), value(c), value(d))
@inline (f::Disarm{<:Any})(a, b, c, d, e) = func(f)(value(a), value(b), value(c), value(d), value(e))
@inline (f::Disarm{<:Any})(a, b, c, d, e, others...) = func(f)(value(a), value(b), value(c), value(d), value(e), value.(others)...)

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
            push!(body.args, :($y = $(value(p))($(interpolated_variable.(parent(p)...)))))
        elseif isa(x, ValueNote)
            in(y, args) || push!(args, y)
        end
    end
    reverse!(body.args)
    reverse!(args)
    return args, body
end
