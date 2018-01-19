const OVERDUB_ARGS_SYMBOL = gensym("cassette_overdub_arguments")

const BEGIN_OVERDUB_REGION = gensym("cassette_begin_overdub_region")

# Return the CodeInfo method body for signature `S` and `world`,
# if it exists in the method table. Otherwise, return `nothing`.
function lookup_method_body(::Type{S};
                            world::UInt = typemax(UInt),
                            debug::Bool = false) where {S<:Tuple}
    if debug
        Core.println("-----------------------------------")
        Core.println("LOOKING UP CODEINFO FOR:")
        Core.println("\tSIGNATURE: ", S)
        Core.println("\tWORLD: ", world)
    end
    S.parameters[1].name.module === Core.Compiler && return nothing

    # retrieve initial Method + CodeInfo
    _methods = Base._methods_by_ftype(S, -1, world)
    length(_methods) == 1 || return nothing
    type_signature, raw_static_params, method = first(_methods)
    method_instance = Core.Compiler.code_for_method(method, type_signature, raw_static_params, world, false)
    method_signature = method.sig
    static_params = Any[raw_static_params...]
    code_info = Core.Compiler.retrieve_code_info(method_instance)
    isa(code_info, CodeInfo) || return nothing
    code_info = Core.Compiler.copy_code_info(code_info)
    debug && Core.println("FOUND METHOD: ", sprint(show, method))
    debug && Core.println("FOUND CODEINFO: ", sprint(show, code_info))

    # substitute static parameters
    body = Expr(:block)
    body.args = code_info.code
    Core.Compiler.substitute!(body, 0, Any[], method_signature, static_params, 0, :propagate)

    # construct new slotnames/slotflags, offset non-self slotnumbers
    code_info.slotnames = Any[code_info.slotnames[1], OVERDUB_ARGS_SYMBOL, code_info.slotnames[2:end]...]
    code_info.slotflags = Any[code_info.slotflags[1], 0x00,                code_info.slotflags[2:end]...]
    replace_match!(x -> isa(x, SlotNumber) || isa(x, NewvarNode), code_info.code) do x
        if isa(x, NewvarNode) && x.slot.id > 1
            return NewvarNode(SlotNumber(x.slot.id + 1))
        elseif x.id > 1
            return SlotNumber(x.id + 1)
        end
        return x
    end

    # construct new `code_info.code` in which original arguments are properly destructured
    new_code = copy_prelude_code(code_info.code)
    prelude_end = length(new_code)
    n_actual_args = fieldcount(S) - 1
    n_method_args = Int64(method.nargs) - 1
    for i in 1:n_method_args
        slot = i + 2
        actual_argument = Expr(:call, GlobalRef(Core, :getfield), SlotNumber(2), i)
        push!(new_code, :($(SlotNumber(slot)) = $actual_argument))
        code_info.slotflags[slot] |= 0x01 << 0x01 # make sure the "assigned" bitflag is set
    end
    if method.isva
        isempty(new_code) || pop!(new_code) # remove the slot reassignment that we're replacing
        final_arguments = Expr(:call, GlobalRef(Core, :tuple))
        for i in n_method_args:n_actual_args
            ssaval = SSAValue(code_info.ssavaluetypes)
            actual_argument = Expr(:call, GlobalRef(Core, :getfield), SlotNumber(2), i)
            push!(new_code, :($ssaval = $actual_argument))
            push!(final_arguments.args, ssaval)
            code_info.ssavaluetypes += 1
        end
        push!(new_code, :($(SlotNumber(n_method_args + 2)) = $final_arguments))
    end
    push!(new_code, BEGIN_OVERDUB_REGION)
    append!(new_code, code_info.code[(prelude_end + 1):end])
    code_info.code = fix_labels_and_gotos!(new_code)
    return code_info
end
