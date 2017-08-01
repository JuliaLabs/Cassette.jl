###################
# CodeInfo Lookup #
###################

#=
Historically, `code_lowered(f, types)` requires `f` to be the function instance. That
interface is just a holdover from the days where `typeof(f) === Function`; nowadays, the
function type + argument type signature is a unique identifier of a method. Thus, we can do
the following, which can be called from a generated function.
=#
methods_from_type_signature(::Type{S}, world::UInt = typemax(UInt)) where {S<:Tuple} = Base._methods_by_ftype(S, -1, world)

function code_info_from_type_signature(::Type{S}, arg_names::Vector, world::UInt = typemax(UInt)) where {S<:Tuple}
    methods = methods_from_type_signature(S, world)
    if length(methods) != 1
        return nothing
    else
        return code_info_from_method_info(first(methods), arg_names, world)
    end
end

function code_info_from_method_info(method_info, arg_names::Vector, world::UInt = typemax(UInt))
    type_signature, raw_static_params, method = method_info

    # extract CodeInfo from method
    local code_info::CodeInfo
    if isdefined(method, :generator)
        method_instance = Core.Inference.code_for_method(method, type_signature, raw_static_params, world, false)
        code_info = Core.Inference.get_staged(method_instance)
    else
        if isa(method.source, CodeInfo)
            code_info = copy_code_info(method.source)
        else
            code_info = Base.uncompressed_ast(method)
        end
    end

    # prepare static parameters for substitution
    static_params = Any[]
    for param in raw_static_params
        if isa(param, Symbol) || isa(param, SSAValue) || isa(param, Slot)
            push!(static_params, QuoteNode(param))
        else
            push!(static_params, param)
        end
    end

    # substitute static parameters/varargs
    body = Expr(:block)
    body.args = code_info.code
    if method.isva
        nargs = method.nargs
        new_nargs = length(arg_names) + 1
        new_slotnames = code_info.slotnames[1:(nargs - 1)]
        new_slotflags = code_info.slotflags[1:(nargs - 1)]
        for i in nargs:new_nargs
            push!(new_slotnames, arg_names[i - 1])
            push!(new_slotflags, 0x00)
        end
        append!(new_slotnames, code_info.slotnames[(nargs + 1):end])
        append!(new_slotflags, code_info.slotflags[(nargs + 1):end])
        offset = new_nargs - nargs
        vararg_tuple = Expr(:call, GlobalRef(Core, :tuple), [SlotNumber(i) for i in nargs:new_nargs]...)
        new_slots = Any[SlotNumber(i) for i in 1:(method.nargs - 1)]
        push!(new_slots, vararg_tuple)
        Base.Core.Inference.substitute!(body, new_nargs, new_slots, type_signature, Any[static_params...], offset)
        code_info.slotnames = new_slotnames
        code_info.slotflags = new_slotflags
    else
        Base.Core.Inference.substitute!(body, 0, Any[], type_signature, Any[static_params...], 0)
    end
    return code_info
end

function copy_code_info(old_code_info::CodeInfo)
    new_code_info = ccall(:jl_copy_code_info, Ref{CodeInfo}, (Any,), old_code_info)
    new_code_info.code = Base.Core.Inference.copy_exprargs(old_code_info.code)
    new_code_info.slotnames = copy(old_code_info.slotnames)
    new_code_info.slotflags = copy(old_code_info.slotflags)
    return new_code_info
end

###################################
# Subexpression Match Replacement #
###################################

replace_match!(f, ismatch, x) = x
replace_match!(f, ismatch, code_info::CodeInfo) = (replace_match!(f, ismatch, code_info.code); code_info)
replace_match!(f, ismatch, ast::Expr) = (replace_match!(f, ismatch, ast.args); ast)

function replace_match!(f, ismatch, lines::Array)
    for i in eachindex(lines)
        line = lines[i]
        if ismatch(line)
            lines[i] = f(line)
        else
            replace_match!(f, ismatch, line)
        end
    end
    return lines
end

####################
# Call Replacement #
####################

function is_replaceable_call(x)
    if isa(x, Expr) && (x.head == :call)
        if isa(x.args[1], GlobalRef)
            return x.args[1].mod != Core
        else
            return true
        end
    end
    return false
end

function transform_call!(f, call::Expr)
    call.args[1] = f(replace_calls!(f, call.args[1]))
    for i in 2:length(call.args)
        replace_calls!(f, call.args[i])
    end
    return call
end

replace_calls!(f, x) = replace_match!(call -> transform_call!(f, call), is_replaceable_call, x)

##########################
# SlotNumber Replacement #
##########################

replace_slotnumbers!(f, x) = replace_match!(f, s -> isa(s, SlotNumber), x)
