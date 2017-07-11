###################
# CodeInfo Lookup #
###################

#=
Historically, `code_lowered(f, types)` requires `f` to be the function instance. That
interface is just a holdover from the days where `typeof(f) === Function`; nowadays, the
function type + argument type signature is a unique identifier of a method. Thus, we can do
the following, which can be called from a generated function.
=#

function code_info_from_type_signature(::Type{S}, world = typemax(UInt)) where {S<:Tuple}
    methods = methods_from_type_signature(S, world)
    if length(methods) != 1
        return nothing
    else
        return code_info_from_method_info(first(methods))
    end
end

methods_from_type_signature(::Type{S}, world = typemax(UInt)) where {S<:Tuple} = Base._methods_by_ftype(S, -1, world)

function code_info_from_method_info(method_info)
    code_info = Base.uncompressed_ast(method_info[3])
    static_params = method_info[2]
    replace_static_parameters!(code_info, static_params)
    return code_info
end

################################
# Static Parameter Replacement #
################################

function replace_static_parameters!(code_info::CodeInfo, static_params)
    return replace_static_parameters!(code_info.code, static_params)
end

function replace_static_parameters!(lines::Vector, static_params)
    for i in eachindex(lines)
        line = lines[i]
        if isa(line, Expr)
            if line.head == :static_parameter
                lines[i] = static_params[line.args[1]]
            else
                replace_static_parameters!(line.args, static_params)
            end
        end
    end
end

####################
# Call Replacement #
####################

replace_calls!(f, code_info::CodeInfo) = replace_calls!(f, code_info.code)

function replace_calls!(f, lines::Vector)
    for line in lines
        isa(line, Expr) && replace_calls!(f, line)
    end
    return lines
end

function replace_calls!(f, ast::Expr)
    if ast.head == :call && (ast.args[1] != GlobalRef(Core, :apply_type))
        callable = ast.args[1]
        isa(callable, Expr) && replace_calls!(f, callable)
        ast.args[1] = f(callable)
        child_indices = 2:length(ast.args)
    else
        child_indices = 1:length(ast.args)
    end
    for i in child_indices
        child = ast.args[i]
        if isa(child, Expr)
            replace_calls!(f, child)
        end
    end
    return ast
end
