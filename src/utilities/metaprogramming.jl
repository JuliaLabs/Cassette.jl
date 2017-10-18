##############
# Predicates #
##############

is_call(x) = isa(x, Expr) && (x.head == :call)

function is_method_definition(x)
    if isa(x, Expr)
        if x.head == :function
            return true
        elseif x.head == :(=) && isa(x.args[1], Expr)
            lhs = x.args[1]
            if lhs.head == :where
                lhs = lhs.args[1]
            end
            return lhs.head == :call
        end
    end
    return false
end

###############################
# Subexpression Match/Replace #
###############################

replace_match!(f, ismatch, x) = x

replace_match!(f, ismatch, code_info::CodeInfo) = (replace_match!(f, ismatch, code_info.code); code_info)

function replace_match!(f, ismatch, lines::AbstractArray)
    for i in eachindex(lines)
        line = lines[i]
        if ismatch(line)
            lines[i] = f(line)
        elseif isa(line, Expr)
            replace_match!(f, ismatch, line.args)
        end
    end
    return lines
end

function transform_call!(f, call::Expr)
    call.args[1] = f(replace_calls!(f, Any[call.args[1]])[])
    for i in 2:length(call.args)
        call.args[i] = replace_calls!(f, Any[call.args[i]])[]
    end
    return call
end

replace_calls!(f, x) = replace_match!(call -> transform_call!(f, call), is_call, x)

replace_slotnumbers!(f, x) = replace_match!(f, s -> isa(s, SlotNumber), x)

#################
# Miscellaneous #
#################

unquote(x) = x
unquote(x::QuoteNode) = x.value
unquote(x::Expr) = x.head == :quote ? first(x.args) : x

function unqualify_name(e::Expr)
    @assert e.head == :(.)
    return unqualify_name(last(e.args))
end

unqualify_name(name::Symbol) = name
