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
        return code_info_from_method_info(first(methods), arg_names)
    end
end

function code_info_from_method_info(method_info, arg_names::Vector)
    typesig, static_params, method = method_info
    Core.Inference.code_for_method(method, typesig, static_params, typemax(UInt), false)
    code_info = Base.uncompressed_ast(method)
    body = Expr(:block)
    body.args = code_info.code
    if method.isva
        # TODO: verify the correctness of this
        nargs = method.nargs
        new_nargs = length(arg_names + 1)
        new_slotnames = code_info.slotnames[1:(nargs - 1)]
        new_slotflags = code_info.slotflags[1:(nargs - 1)]
        for i in nargs:new_nargs
            push!(new_slotnames, arg_names[i - 1])
            push!(new_slotflags, 0x00)
        end
        append!(new_slotnames, code_info.slotnames[(nargs + 1):end])
        append!(new_slotflags, code_info.slotflags[(nargs + 1):end])
        offset = new_nargs - nargs
        vararg_tuple = Expr(:call, GlobalRef(Core, :tuple), [SlotNumber(i) for i in new_slotrange]...)
        argexprs = Any[SlotNumber(i) for i in 1:(method.nargs - 1)]
        push!(argexprs, vararg_tuple)
        Base.Core.Inference.substitute!(body, new_nargs, argexprs, typesig, Any[static_params...], offset)
        code_info.slotnames = new_slotnames
        code_info.slotflags = new_slotflags
    else
        Base.Core.Inference.substitute!(body, 0, Any[], typesig, Any[static_params...], 0)
    end
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

is_replaceable_call(x) = isa(x, Expr) && (x.head == :call) && (x.args[1] != GlobalRef(Core, :apply_type))

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

############
# Old Code #
############

# function replace_static_parameters!(code_info::CodeInfo, static_params)
#     return replace_static_parameters!(code_info.code, static_params)
# end
#
# function replace_static_parameters!(lines::Vector, static_params)
#     for i in eachindex(lines)
#         line = lines[i]
#         if isa(line, Expr)
#             if line.head == :static_parameter
#                 lines[i] = static_params[line.args[1]]
#             else
#                 replace_static_parameters!(line.args, static_params)
#             end
#         end
#     end
# end
#
# function last_arg_index(code_info::CodeInfo)
#     slotnames = code_info.slotnames
#     slotflags = code_info.slotflags
#     result = 1
#     for i in 2:length(slotnames)
#         flag = slotflags[i]
#         name = slotnames[i]
#         if flag == 0x00 || (findnext(slotnames, name, i + 1) != 0)
#             result += 1
#         else
#             break
#         end
#     end
#     return result
# end
#
# function munge_arguments!(code_info::CodeInfo, arg_names::Vector{Symbol})
#     slotnames = code_info.slotnames
#     arg_count = length(arg_names)
#     last_arg_i = last_arg_index(code_info)
#     for i in 2:(last_arg_i - 1)
#         slotnames[i] = arg_names[i-1]
#     end
#     if (last_arg_i - 1) < arg_count # true if the last arg is a vararg
#         slotflags = code_info.slotflags
#         old_slotnames_length = length(slotnames)
#         new_slot_range = last_arg_i:(arg_count + 1)
#         # insert new arguments in proper slots
#         for i in new_slot_range
#             insert!(slotnames, i, arg_names[i - 1])
#             insert!(slotflags, i, 0x00)
#         end
#         # replace old SlotNumbers with new SlotNumbers
#         slotoffset = length(slotnames) - old_slotnames_length
#         replace_slotnumbers!(code_info) do sn
#             if sn.id >= last_arg_i
#                 return SlotNumber(sn.id + slotoffset)
#             else
#                 return sn
#             end
#         end
#         # reassign the vararg to its actual list of arguments
#         new_slotnumber_tuple = Expr(:call, GlobalRef(Core, :tuple), [SlotNumber(i) for i in new_slot_range]...)
#         vararg_reassignment = Expr(:(=), SlotNumber(last_arg_i + slotoffset), new_slotnumber_tuple)
#         insert!(code_info.code, 2, vararg_reassignment)
#     else
#         slotnames[last_arg_i] = arg_names[last_arg_i - 1]
#     end
#     return code_info
# end
#
# function munge_arguments!(code_info::CodeInfo, arg_names::Vector{Symbol})
#     new_slotnames = code_info.slotnames
#     old_slotnames = copy(new_slotnames)
#     empty!(new_slotnames)
#     push!(new_slotnames, first(old_slotnames))
#     append!(new_slotnames, arg_names)
#     if length(new_slotnames) == length(old_slotnames)
#         return code_info
#     elseif length(new_slotnames) > length(old_slotnames)
#         # This can happen when code_info's original
#         # method takes a splatted vararg as its final
#         # argument. When this happens, we have to splat
#         # the new arguments into the old vararg's name
#         # via a normal tuple assignment. This also entails
#         # fixing code_info's SlotNumbers and slotflags,
#         # since they are now out of order.
#
#         # Place the vararg name at the end of new_slotnames
#         push!(new_slotnames, last(old_slotnames))
#
#         # Replace the vararg's old SlotNumber with the new SlotNumber
#         old_vararg_slotnumber = SlotNumber(length(old_slotnames))
#         new_vararg_slotnumber = SlotNumber(length(new_slotnames))
#         replace_slotnumbers!(code_info) do slotnumber
#             if slotnumber == old_vararg_slotnumber
#                 return new_vararg_slotnumber
#             end
#             return slotnumber
#         end
#
#         # Munge the slotflags for the new slots
#         new_slot_range = length(old_slotnames):(length(new_slotnames) - 1)
#
#         old_vararg_slotflag = pop!(code_info.slotflags)
#         for _ in new_slot_range
#             # TODO: THESE AREN'T THE RIGHT FLAGS, JUST DUMMY VALUES
#             push!(code_info.slotflags, 0x00)
#         end
#         push!(code_info.slotflags, old_vararg_slotflag)
#
#         # Reassign the vararg to its actual list of arguments
#         new_slotnumber_tuple = Expr(:call, GlobalRef(Core, :tuple), [SlotNumber(i) for i in new_slot_range]...)
#         vararg_reassignment = Expr(:(=), new_vararg_slotnumber, new_slotnumber_tuple)
#         insert!(code_info.code, 2, vararg_reassignment)
#     else
#         error("""
#               not enough symbols in `arg_names` for `code_info` slots
#               `arg_names`: $arg_names
#               `code_info.slotnames`: $old_slotnames
#               `code_info`: $code_info
#               """)
#     end
#     return code_info
# end
#
# replace_calls!(f, code_info::CodeInfo) = replace_calls!(f, code_info.code)
#
# function replace_calls!(f, lines::Vector)
#     for line in lines
#         isa(line, Expr) && replace_calls!(f, line)
#     end
#     return lines
# end
#
# function replace_calls!(f, ast::Expr)
#     if ast.head == :call && (ast.args[1] != GlobalRef(Core, :apply_type))
#         callable = ast.args[1]
#         isa(callable, Expr) && replace_calls!(f, callable)
#         ast.args[1] = f(callable)
#         child_indices = 2:length(ast.args)
#     else
#         child_indices = 1:length(ast.args)
#     end
#     for i in child_indices
#         child = ast.args[i]
#         if isa(child, Expr)
#             replace_calls!(f, child)
#         end
#     end
#     return ast
# end
