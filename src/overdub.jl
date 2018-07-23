#########################
# contextual operations #
#########################

@inline prehook(::Context, ::Vararg{Any}) = nothing

@inline posthook(::Context, ::Vararg{Any}) = nothing

struct RecurseInstead end
@inline execute(ctx::Context, args...) = RecurseInstead()

@inline fallback(ctx::Context, args...) = call(ctx, args...)

@inline call(::ContextWithTag{Nothing}, f, args...) = f(args...)
@inline call(context::Context, f, args...) = untag(f, context)(ntuple(i -> untag(args[i], context), Val(nfields(args)))...)

# TODO: This is currently needed to force the compiler to specialize on the type arguments
# to `Core.apply_type`. In the future, it would be best for Julia's compiler to better handle
# varargs calls to such functions with type arguments, or at least provide a better way to
# force specialization on the type arguments.
@inline call(::ContextWithTag{Nothing}, f::typeof(Core.apply_type), ::Type{A}, ::Type{B}) where {A,B} = f(A, B)
@inline call(::Context, f::typeof(Core.apply_type), ::Type{A}, ::Type{B}) where {A,B} = f(A, B)

@inline canrecurse(ctx::Context, f, args...) = !isa(untag(f, ctx), Core.Builtin)

###########
# overdub #
###########

# An alternative approach is to define `execute(args...) = recurse(args...)` by default,
# instead of using the `RecurseInstead` sentinel type. While cleaner, that approach triggers
# the compiler's recursion limiting heuristic. This sentinel type + control flow approach
# avoids that recursion, and thus avoids related inference problems.
@inline function overdub(ctx::Context, args...)
    prehook(ctx, args...)
    output = execute(ctx, args...)
    output = isa(output, RecurseInstead) ? recurse(ctx, args...) : output
    posthook(ctx, output, args...)
    return output
end

# This is essentially implementing:
#
# function overdub(ctx::Context, ::typeof(Core._apply), f, args...)
#     return overdub(ctx, f, apply_args(ctx, args...)...)
# end
#
# but the extra indirection of calling `overdub` there triggers the compiler's recursion
# limiting heuristic. This implementation avoids that problem by manually inlining the
# above expression by a single call level.
@inline function overdub(ctx::Context, ::typeof(Core._apply), f, _args...)
    args = apply_args(ctx, _args...)
    prehook(ctx, f, args...)
    output = execute(ctx, f, args...)
    output = isa(output, RecurseInstead) ? recurse(ctx, f, args...) : output
    posthook(ctx, output, f, args...)
    return output
end

@inline apply_args(::ContextWithTag{Nothing}, args...) = Core._apply(Core.tuple, args...)
@inline apply_args(ctx::Context, args...) = tagged_apply_args(ctx, args...)

###########
# recurse #
###########

const RECURSE_CTX_SYMBOL = gensym("recurse_context")
const RECURSE_ARGS_SYMBOL = gensym("recurse_arguments")

# The `recurse` pass has four intertwined tasks:
#   1. Apply the user-provided pass, if one is given
#   2. Munge the reflection-generated IR into a valid form for returning from
#      `recurse_generator` (i.e. add new argument slots, substitute static
#      parameters, destructure overdub arguments into underlying method slots, etc.)
#   3. Translate all function calls to `overdub` calls
#   4. If tagging is enabled, do the necessary IR transforms for the metadata tagging system
function recurse_pass!(reflection::Reflection,
                       context_type::DataType,
                       pass_type::DataType = NoPass)
    signature = reflection.signature
    method = reflection.method
    static_params = reflection.static_params
    code_info = reflection.code_info

    #=== 1. Execute user-provided pass (is a no-op by default) ===#

    code_info = pass_type(context_type, signature, code_info)

    #=== 2. Munge the code into a valid form for `recurse_generator` ===#

    # construct new slotnames/slotflags for added slots
    code_info.slotnames = Any[:recurse, RECURSE_CTX_SYMBOL, RECURSE_ARGS_SYMBOL, code_info.slotnames...]
    code_info.slotflags = UInt8[0x00, 0x00, 0x00, code_info.slotflags...]
    n_overdub_slots = 3
    overdub_ctx_slot = SlotNumber(2)
    overdub_args_slot = SlotNumber(3)

    # For the sake of convenience, the rest of this pass will translate `code_info`'s fields
    # into these overdubbed equivalents instead of updating `code_info` in-place. Then, at
    # the end of the pass, we'll reset `code_info` fields accordingly.
    overdubbed_code = Any[]
    overdubbed_codelocs = Int32[]

    # destructure the generated argument slots into the overdubbed method's argument slots.
    n_actual_args = fieldcount(signature)
    n_method_args = Int(method.nargs)
    for i in 1:n_method_args
        slot = i + n_overdub_slots
        actual_argument = Expr(:call, GlobalRef(Core, :getfield), overdub_args_slot, i)
        push!(overdubbed_code, :($(SlotNumber(slot)) = $actual_argument))
        push!(overdubbed_codelocs, code_info.codelocs[1])
        code_info.slotflags[slot] = 0x18 # this slot is now an "SSA slot"
    end

    # If `method` is a varargs method, we have to restructure the original method call's
    # trailing arguments into a tuple and assign that tuple to the expected argument slot.
    if method.isva
        if !isempty(overdubbed_code)
            # remove the final slot reassignment leftover from the previous destructuring
            pop!(overdubbed_code)
            pop!(overdubbed_codelocs)
        end
        if has_tagging_enabled(context_type)
            trailing_arguments = Expr(:call, GlobalRef(Cassette, :_tagged_new_tuple_unsafe), overdub_ctx_slot)
        else
            trailing_arguments = Expr(:call, GlobalRef(Core, :tuple))
        end
        for i in n_method_args:n_actual_args
            push!(overdubbed_code, Expr(:call, GlobalRef(Core, :getfield), overdub_args_slot, i))
            push!(overdubbed_codelocs, code_info.codelocs[1])
            push!(trailing_arguments.args, SSAValue(length(overdubbed_code)))
        end
        push!(overdubbed_code, Expr(:(=), SlotNumber(n_method_args + n_overdub_slots), trailing_arguments))
        push!(overdubbed_codelocs, code_info.codelocs[1])
    end

    #=== 3. Translate function calls to `overdub` calls ===#

    # substitute static parameters, offset slot numbers by number of added slots, and
    # offset statement indices by the number of additional statements
    Base.Meta.partially_inline!(code_info.code, Any[], method.sig, static_params,
                                n_overdub_slots, length(overdubbed_code), :propagate)

    # For the rest of the statements in `code_info.code`, intercept every applicable call
    # expression and replace it with a corresponding call to `Cassette.overdub`.
    for i in 1:length(code_info.code)
        stmnt = code_info.code[i]
        replaceable = Base.Meta.isexpr(stmnt, :foreigncall) ? view(stmnt.args, 2:length(stmnt.args)) : stmnt
        replace_match!(is_call, replaceable) do call
            call.args = Any[GlobalRef(Cassette, :overdub), overdub_ctx_slot, call.args...]
            return call
        end
        push!(overdubbed_code, stmnt)
        push!(overdubbed_codelocs, code_info.codelocs[i])
    end

    #=== 4. IR transforms for the metadata tagging system ===#

    if has_tagging_enabled(context_type)
        # changemap = fill(0, length(code_info.code))

        # TODO: Scan the IR for `Module`s in the first argument position for `GlobalRef`s.
        # For every unique such `Module`, make a new `SSAValue` at the top of the method body
        # corresponding to `Cassette.fetch_tagged_module` called with the given context and
        # module. Then, replace all `GlobalRef`-loads with the corresponding
        # `Cassette._tagged_global_ref` invocation. All `GlobalRef`-stores must be preserved
        # as-is, but need a follow-up statement calling
        # `Cassette._tagged_global_ref_set_meta!` on the relevant arguments.

        replace_match!(is_new, overdubbed_code) do x
            return Expr(:call, GlobalRef(Cassette, :tagged_new), overdub_ctx_slot, x.args...)
        end

        # TODO: appropriately untag all `gotoifnot` conditionals

        # TODO: appropriately untag all `ccall` arguments

        # Core.Compiler.renumber_ir_elements!(overdubbed_code, changemap)
    end

    #=== 5. Set `code_info`/`reflection` fields accordingly ===#

    code_info.code = overdubbed_code
    code_info.codelocs = overdubbed_codelocs
    code_info.ssavaluetypes = length(overdubbed_code)
    code_info.method_for_inference_limit_heuristics = method
    reflection.code_info = code_info

    return reflection
end

# `args` is `(typeof(original_function), map(typeof, original_args_tuple)...)`
function recurse_generator(pass_type, self, context_type, args::Tuple)
    if !(nfields(args) > 1 && args[1] <: Core.Builtin)
        try
            untagged_args = ((untagtype(args[i], context_type) for i in 1:nfields(args))...,)
            reflection = reflect(untagged_args)
            if isa(reflection, Reflection)
                recurse_pass!(reflection, context_type, pass_type)
                body = reflection.code_info
                @safe_debug "returning overdubbed CodeInfo" args body
                return body
            end
        catch err
            errmsg = "ERROR COMPILING $args IN CONTEXT $(context_type): \n" * sprint(showerror, err)
            return quote
                error($errmsg)
            end
        end
    end
    @safe_debug "no CodeInfo found; executing via fallback" args
    return quote
        $(Expr(:meta, :inline))
        $Cassette.fallback($RECURSE_CTX_SYMBOL, $RECURSE_ARGS_SYMBOL...)
    end
end

function recurse_definition(pass, line, file)
    return quote
        function recurse($RECURSE_CTX_SYMBOL::ContextWithPass{pass}, $RECURSE_ARGS_SYMBOL...) where {pass<:$pass}
            $(Expr(:meta,
                   :generated,
                   Expr(:new,
                        Core.GeneratedFunctionStub,
                        :recurse_generator,
                        Any[:recurse, RECURSE_CTX_SYMBOL, RECURSE_ARGS_SYMBOL],
                        Any[:pass],
                        line,
                        QuoteNode(Symbol(file)),
                        true)))
        end
    end
end

@eval $(recurse_definition(:NoPass, @__LINE__, @__FILE__))
