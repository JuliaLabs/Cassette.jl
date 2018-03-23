#########################
# Expression Predicates #
#########################

is_call(x) = Base.Meta.isexpr(x, :call) && (x.args[1] !== GlobalRef(Core, :tuple))

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

############################
# Expression Match/Replace #
############################

function replace_match!(replace, ismatch, x)
    if ismatch(x)
        return replace(x)
    elseif isa(x, Array)
        for i in eachindex(x)
            x[i] = replace_match!(replace, ismatch, x[i])
        end
    elseif isa(x, Expr)
        replace_match!(replace, ismatch, x.args)
    end
    return x
end

############
# Julia IR #
############

function fix_labels_and_gotos!(code::Vector)
    changes = Dict{Int,Int}()
    for (i, stmnt) in enumerate(code)
        if isa(stmnt, LabelNode)
            code[i] = LabelNode(i)
            changes[stmnt.label] = i
        end
    end
    for (i, stmnt) in enumerate(code)
        if isa(stmnt, GotoNode)
            code[i] = GotoNode(get(changes, stmnt.label, stmnt.label))
        elseif Base.Meta.isexpr(stmnt, :enter)
            stmnt.args[1] = get(changes, stmnt.args[1], stmnt.args[1])
        elseif Base.Meta.isexpr(stmnt, :gotoifnot)
            stmnt.args[2] = get(changes, stmnt.args[2], stmnt.args[2])
        end
    end
    return code
end

function copy_prelude_code(code::Vector)
    prelude_code = Any[]
    for stmnt in code
        if isa(stmnt, Nothing) || isa(stmnt, NewvarNode)
            push!(prelude_code, stmnt)
        else
            break
        end
    end
    return prelude_code
end

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
