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
