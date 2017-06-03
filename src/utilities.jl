#########################
# Expression Generation #
#########################

interpolated_variable(x::ValueNote) = Symbol("x_" * idstring(untrack(x)))
interpolated_variable(x) = x

function toexpr(output::ValueNote)
    body = Expr(:block)
    args = Symbol[]
    walkback(output) do x, hasparent
        y = interpolated_variable(x)
        if hasparent
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
