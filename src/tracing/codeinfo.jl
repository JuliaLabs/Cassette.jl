###################
# CodeInfo Lookup #
###################

function lookup_code_info(::Type{S}, arg_names::Vector,
                          debug::Bool = false,
                          world::UInt = typemax(UInt)) where {S<:Tuple}
    return lookup_code_info0(S, arg_names, debug, world)
end

function lookup_code_info0(::Type{S}, arg_names::Vector,
                           debug::Bool = false,
                           world::UInt = typemax(UInt)) where {S<:Tuple}
    if debug
        println("-----------------------------------")
        println("TYPE SIGNATURE: ", S)
    end

    # retrieve initial Method + CodeInfo
    methods = Base._methods_by_ftype(S, -1, world)
    length(methods) == 1 || return nothing
    type_signature, raw_static_params, method = first(methods)
    method_instance = Core.Inference.code_for_method(method, type_signature, raw_static_params, world, false)
    code_info = Core.Inference.retrieve_code_info(method_instance)
    isa(code_info, CodeInfo) || return nothing

    if debug
        println("METHOD: ", method)
        println("OLD CODEINFO: ", code_info)
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
        Base.Core.Inference.substitute!(body, new_nargs, new_slots, type_signature, static_params, offset)
        code_info.slotnames = new_slotnames
        code_info.slotflags = new_slotflags
    else
        Base.Core.Inference.substitute!(body, 0, Any[], type_signature, static_params, 0)
    end

    debug && println("NEW CODEINFO: $code_info")

    return code_info
end

function lookup_method_instance(::Type{S}, world::UInt = typemax(UInt)) where {S}
    methods = Base._methods_by_ftype(S, -1, world)
    length(methods) == 1 || return nothing
    type_signature, raw_static_params, method = first(methods)
    return Core.Inference.code_for_method(method, type_signature, raw_static_params, world, false)
end

function lookup_code_info1(::Type{S}, world::UInt = typemax(UInt)) where {S}
    method_instance = lookup_method_instance(S, world)
    infstate = Core.Inference.typeinf_frame(method_instance, true, true, Core.Inference.InferenceParams(world))
    code_info = infstate.src
    code_info.ssavaluetypes = length(code_info.ssavaluetypes)
    return code_info
end

function lookup_code_info2(::Type{S}, world::UInt = typemax(UInt)) where {S}
    method_instance = lookup_method_instance(S, world)
    infstate = Core.Inference.InferenceState(method_instance, true, false, Core.Inference.InferenceParams(world))
    Core.Inference.inlining_pass!(infstate)
    code_info = infstate.src
    code_info.ssavaluetypes = length(code_info.ssavaluetypes)
    return code_info
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
