#########################
# contextual operations #
#########################

@inline prehook(::AbstractContext, ::Vararg{Any}) = nothing
@inline posthook(::AbstractContext, ::Vararg{Any}) = nothing
@inline is_user_primitive(::AbstractContext, ::Vararg{Any}) = false
@inline is_core_primitive(ctx::AbstractContext, args...) = _is_core_primitive(ctx, args...)
@inline execution(::AbstractContext, f, args...) = f(args...)

@generated function _is_core_primitive(::AbstractContext, args...)
    # TODO: this is slow, we should try to check whether the reflection is possible
    # without going through the whole process of actually computing it
    if isa(reflect(args), Reflection) # TODO unboxtype `args`
        result = :(false)
    else
        result = :(true)
    end
    return quote
        $(Expr(:meta, :inline))
        $(result)
    end
end

###################
# overdub_execute #
###################

@inline function overdub_execute(ctx::AbstractContext, args...)
    prehook(ctx, args...)
    if is_user_primitive(ctx, args...)
        output = execution(ctx, args...)
    else
        output = overdub_recurse(ctx, args...)
    end
    posthook(ctx, output, args...)
    return output
end

###################
# overdub_recurse #
###################

const OVERDUB_CTX_SYMBOL = gensym("overdub_context")
const OVERDUB_ARGS_SYMBOL = gensym("overdub_arguments")

# Note that this pass emits code in which LHS SSAValues are not monotonically increasing.
# This currently isn't a problem, but in the future, valid IR might require monotonically
# increasing LHS SSAValues, in which case we'll have to add an extra SSA-remapping pass to
# this function.
function overdub_recurse_pass!(reflection::Reflection,
                               context_type::DataType,
                               pass_type::DataType = UnusedPass)
    signature = reflection.signature
    method = reflection.method
    static_params = reflection.static_params
    code_info = reflection.code_info

    # execute user-provided pass (is a no-op by default)
    code_info = pass_type(context_type, signature, code_info)

    # construct new slotnames/slotflags for added slots
    code_info.slotnames = Any[:overdub_recurse, OVERDUB_CTX_SYMBOL, OVERDUB_ARGS_SYMBOL, code_info.slotnames...]
    code_info.slotflags = UInt8[0x00, 0x00, 0x00, code_info.slotflags...]
    n_overdub_slots = 3
    overdub_ctx_slot = SlotNumber(2)
    overdub_args_slot = SlotNumber(3)

    # substitute static parameters and offset slotnumbers by number of added slots
    code_expr = Expr(:block)
    code_expr.args = code_info.code
    Core.Compiler.substitute!(code_expr, 0, Any[], method.sig, static_params, n_overdub_slots, :propagate)

    # Instantiate a new code array containing the same preceding `Nothing`s, `NewvarNode`s,
    # etc. as `code_info`'s code array. The rest of this pass will translate statements from
    # `code_info.code` to `overdubbed_code`, instead of updating `code_info.code` in-place
    # (just for the sake of convenience). Then, at the end, we'll set `code_info.code` to
    # `overdubbed_code`.
    overdubbed_code = copy_prelude_code(code_info.code)
    prelude_length = length(overdubbed_code)

    # destructure the generated argument slots into the overdubbed method's argument slots.
    n_actual_args = fieldcount(signature)
    n_method_args = Int(method.nargs)
    for i in 1:n_method_args
        slot = i + n_overdub_slots
        actual_argument = Expr(:call, GlobalRef(Core, :getfield), overdub_args_slot, i)
        push!(overdubbed_code, :($(SlotNumber(slot)) = $actual_argument))
        code_info.slotflags[slot] = 0x18 # this slot is now an "SSA slot"
    end

    # If `method` is a varargs method, we have to destructure the original method call's
    # trailing arguments into a tuple and assign that tuple to the expected argument slot.
    if method.isva
        # remove the final slot reassignment leftover from the previous destructuring
        isempty(overdubbed_code) || pop!(overdubbed_code)
        final_arguments = Expr(:call, GlobalRef(Core, :tuple))
        for i in n_method_args:n_actual_args
            ssaval = SSAValue(code_info.ssavaluetypes)
            actual_argument = Expr(:call, GlobalRef(Core, :getfield), overdub_args_slot, i)
            push!(overdubbed_code, :($ssaval = $actual_argument))
            push!(final_arguments.args, ssaval)
            code_info.ssavaluetypes += 1
        end
        push!(overdubbed_code, :($(SlotNumber(n_method_args + n_overdub_slots)) = $final_arguments))
    end

    # Scan the IR for `Module`s in the first argument position for `GlobalRef`s. For every
    # unique such `Module`, make a new `SSAValue` at the top of the method body corresponding
    # to `Cassette.fetch_tagged_module` called with the given context and module. Then,
    # replace all `GlobalRef`-loads with the corresponding `Cassette._tagged_global_ref`
    # invocation. All `GlobalRef`-stores must be preserved as-is, but need a follow-up
    # statement calling `Cassette._tagged_global_ref_set_meta!` on the relevant arguments.
    # TODO

    # For the rest of the statements in `code_info.code`, intercept every applicable call
    # expression and replace it with a corresponding call to `Cassette.overdub_execute`.
    for i in (prelude_length + 1):length(code_info.code)
        stmnt = code_info.code[i]
        replace_match!(is_call, stmnt) do call
            call.args = Any[GlobalRef(Cassette, :overdub_execute), overdub_ctx_slot, call.args...]
            return call
        end
        push!(overdubbed_code, stmnt)
    end

    # Replace `new` expressions with calls to `Cassette.tagged_new`.
    # TODO

    code_info.code = fix_labels_and_gotos!(overdubbed_code)
    code_info.method_for_inference_limit_heuristics = method
    reflection.code_info = code_info
    return reflection
end

# `args` is `(typeof(original_function), map(typeof, original_args_tuple)...)`
function overdub_recurse_generator(pass_type, self, context_type, args::Tuple)
    try
        reflection = reflect(args) # TODO unboxtype `args`
        if isa(reflection, Reflection)
            overdub_recurse_pass!(reflection, context_type, pass_type)
            body = reflection.code_info
            @safe_debug "returning overdubbed CodeInfo" args body
        else
            body = quote
                $(Expr(:meta, :inline))
                $Cassette.execution($OVERDUB_CTX_SYMBOL, $OVERDUB_ARGS_SYMBOL...)
            end
            @safe_debug "no CodeInfo found; executing as primitive" args
        end
        return body
    catch err
        @safe_error "error compiling" args context=context_type
        errmsg = "ERROR COMPILING $args IN CONTEXT $(context_type): \n" * sprint(showerror, err)
        return quote
            error($errmsg)
        end
    end
end

function overdub_recurse_definition(pass, line, file)
    return quote
        function overdub_recurse($OVERDUB_CTX_SYMBOL::AbstractContext{pass}, $OVERDUB_ARGS_SYMBOL...) where {pass<:$pass}
            $(Expr(:meta,
                   :generated,
                   Expr(:new,
                        Core.GeneratedFunctionStub,
                        :overdub_recurse_generator,
                        Any[:overdub_recurse, OVERDUB_CTX_SYMBOL, OVERDUB_ARGS_SYMBOL],
                        Any[:pass],
                        line,
                        QuoteNode(Symbol(file)),
                        true)))
        end
    end
end

@eval $(overdub_recurse_definition(:UnusedPass, @__LINE__, @__FILE__))
