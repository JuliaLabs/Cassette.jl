#########################
# contextual operations #
#########################

@inline prehook(::Context, ::Vararg{Any}) = nothing

@inline posthook(::Context, ::Vararg{Any}) = nothing

struct OverdubInstead end
@inline execute(ctx::Context, args...) = OverdubInstead()

@inline fallback(ctx::Context, args...) = call(ctx, args...)

@inline call(::ContextWithTag{Nothing}, f, args...) = f(args...)
@inline call(context::Context, f, args...) = untag(f, context)(ntuple(i -> untag(args[i], context), Val(nfields(args)))...)

# TODO: This is currently needed to force the compiler to specialize on the type arguments
# to `Core.apply_type`. In the future, it would be best for Julia's compiler to better handle
# varargs calls to such functions with type arguments, or at least provide a better way to
# force specialization on the type arguments.
@inline call(::ContextWithTag{Nothing}, f::typeof(Core.apply_type), ::Type{A}, ::Type{B}) where {A,B} = f(A, B)
@inline call(::Context, f::typeof(Core.apply_type), ::Type{A}, ::Type{B}) where {A,B} = f(A, B)

@inline canoverdub(ctx::Context, f, args...) = !isa(untag(f, ctx), Core.Builtin)

###########
# overdub #
###########

const OVERDUB_CTX_SYMBOL = gensym("overdub_context")
const OVERDUB_ARGS_SYMBOL = gensym("overdub_arguments")
const OVERDUB_TMP_SYMBOL = gensym("overdub_tmp")

# The `overdub` pass has four intertwined tasks:
#   1. Apply the user-provided pass, if one is given
#   2. Munge the reflection-generated IR into a valid form for returning from
#      `recurse_generator` (i.e. add new argument slots, substitute static
#      parameters, destructure overdub arguments into underlying method slots, etc.)
#   3. Replace all calls of the form `output = f(args...)` with:
#      ```
#      prehook(ctx, f, args...)
#      tmp = execute(ctx, f, args...)
#      isa(tmp, OverdubInstead) ? overdub(ctx, f, args...) : tmp
#      posthook(ctx, f, args...)
#      output = tmp
#      ```
#   4. If tagging is enabled, do the necessary IR transforms for the metadata tagging system
function overdub_pass!(reflection::Reflection,
                       context_type::DataType,
                       pass_type::DataType = NoPass)
    signature = reflection.signature
    method = reflection.method
    static_params = reflection.static_params
    code_info = reflection.code_info

    # TODO: This `iskwfunc` is part of a hack that `overdub_pass!` implements in order to fix
    # jrevels/Cassette.jl#48. These assumptions made by this hack are quite fragile, so we
    # should eventually get Base to expose a standard/documented API for this. Here, we see
    # this hack's first assumption: that `Core.kwfunc(f)` is going to return a function whose
    # type name is prefixed by `#kw##`. More assumptions for this hack will be commented on
    # as we go.
    iskwfunc = startswith(String(signature.parameters[1].name.name), "#kw##")
    istaggingenabled = has_tagging_enabled(context_type)

    #=== execute user-provided pass (is a no-op by default) ===#

    if !iskwfunc
        code_info = pass_type(context_type, signature, code_info)
    end

    #=== munge the code into a valid form for `overdub_generator` ===#

    # construct new slotnames/slotflags for added slots
    code_info.slotnames = Any[:overdub, OVERDUB_CTX_SYMBOL, OVERDUB_ARGS_SYMBOL, code_info.slotnames..., OVERDUB_TMP_SYMBOL]
    code_info.slotflags = UInt8[0x00, 0x00, 0x00, code_info.slotflags..., 0x00]
    n_prepended_slots = 3
    overdub_ctx_slot = SlotNumber(2)
    overdub_args_slot = SlotNumber(3)
    overdub_tmp_slot = SlotNumber(length(code_info.slotnames))

    # For the sake of convenience, the rest of this pass will translate `code_info`'s fields
    # into these overdubbed equivalents instead of updating `code_info` in-place. Then, at
    # the end of the pass, we'll reset `code_info` fields accordingly.
    overdubbed_code = Any[]
    overdubbed_codelocs = Int32[]

    # destructure the generated argument slots into the overdubbed method's argument slots.
    n_actual_args = fieldcount(signature)
    n_method_args = Int(method.nargs)
    for i in 1:n_method_args
        slot = i + n_prepended_slots
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
        push!(overdubbed_code, Expr(:(=), SlotNumber(n_method_args + n_prepended_slots), trailing_arguments))
        push!(overdubbed_codelocs, code_info.codelocs[1])
    end

    original_arg_slots = [SlotNumber(i + n_prepended_slots) for i in 1:n_method_args]

    #=== finish initialization of `overdubbed_code`/`overdubbed_codelocs` ===#

    # substitute static parameters, offset slot numbers by number of added slots, and
    # offset statement indices by the number of additional statements
    Base.Meta.partially_inline!(code_info.code, Any[], method.sig, static_params,
                                n_prepended_slots, length(overdubbed_code), :propagate)

    old_code_start_index = length(overdubbed_code) + 1

    append!(overdubbed_code, code_info.code)
    append!(overdubbed_codelocs, code_info.codelocs)

    #=== TODO: perform tagged module transformation if tagging is enabled ===#

    # Scan the IR for `Module`s in the first argument position for `GlobalRef`s.
    # For every unique such `Module`, make a new `SSAValue` at the top of the method body
    # corresponding to `Cassette.fetch_tagged_module` called with the given context and
    # module. Then, replace all `GlobalRef`-loads with the corresponding
    # `Cassette._tagged_global_ref` invocation. All `GlobalRef`-stores must be preserved
    # as-is, but need a follow-up statement calling `Cassette._tagged_global_ref_set_meta!`
    # on the relevant arguments.

    #=== TODO: untag all `ccall` SSAValue arguments if tagging is enabled ===#

    #=== untag `gotoifnot` conditionals if tagging is enabled ===#

    # this sentinel is consumed by in a call-replacement pass below; we use
    # it so that we don't accidentally overdub the calls we've inserted
    untag_call_sentinel = :REPLACE_ME_WITH_CASSETTE_UNTAG
    if istaggingenabled && !iskwfunc
        insert_ir_elements!(overdubbed_code, overdubbed_codelocs, 1,
                            (x, i) -> Base.Meta.isexpr(x, :gotoifnot),
                            (x, i) -> [
                                Expr(:call, untag_call_sentinel, x.args[1], overdub_ctx_slot),
                                Expr(:gotoifnot, SSAValue(i), x.args[2])
                            ])
    end

    #=== replace `Expr(:call, ...)` with `Expr(:call, :overdub, ...)` calls ===#

    if iskwfunc
        # Another assumption of this `iskwfunc` hack is that the second to last statement in
        # the lowered IR for `Core.kwfunc(f)` is the call to the "underlying" non-kwargs form
        # of `f`. Thus, we `overdub` that call instead of replacing it with `call`.
        for i in 1:length(overdubbed_code)
            stmt = overdubbed_code[i]
            replacewith = i === (length(overdubbed_code) - 1) ? :overdub : :call
            if Base.Meta.isexpr(stmt, :(=))
                replacein = stmt.args
                replaceat = 2
            else
                replacein = overdubbed_code
                replaceat = i
            end
            stmt = replacein[replaceat]
            if Base.Meta.isexpr(stmt, :call)
                replacein[replaceat] = Expr(:call, GlobalRef(Cassette, replacewith), overdub_ctx_slot, stmt.args...)
            end
        end
    else
        predicate = (x, i) -> begin
            i >= old_code_start_index || return false
            stmt = Base.Meta.isexpr(x, :(=)) ? x.args[2] : x
            return Base.Meta.isexpr(stmt, :call) && stmt.args[1] !== untag_call_sentinel
        end
        itemfunc = (x, i) -> begin
            callstmt = Base.Meta.isexpr(x, :(=)) ? x.args[2] : x
            execstmt = Expr(:call, GlobalRef(Cassette, :execute), overdub_ctx_slot, callstmt.args...)
            overdubstmt = Expr(:call, GlobalRef(Cassette, :overdub), overdub_ctx_slot, callstmt.args...)
            return [
                Expr(:call, GlobalRef(Cassette, :prehook), overdub_ctx_slot, callstmt.args...),
                Expr(:(=), overdub_tmp_slot, execstmt),
                Expr(:call, GlobalRef(Core, :isa), overdub_tmp_slot, GlobalRef(Cassette, :OverdubInstead)),
                Expr(:gotoifnot, SSAValue(i + 2), i + 6),
                Expr(:(=), overdub_tmp_slot, overdubstmt),
                Expr(:call, GlobalRef(Cassette, :posthook), overdub_ctx_slot, overdub_tmp_slot, callstmt.args...),
                Base.Meta.isexpr(x, :(=)) ? Expr(:(=), x.args[1], overdub_tmp_slot) : overdub_tmp_slot
            ]
        end
        insert_ir_elements!(overdubbed_code, overdubbed_codelocs, 6, predicate, itemfunc)
    end

    #=== replace `untag_call_sentinel` with `GlobalRef(Cassette, :untag)` ===#

    if istaggingenabled && !iskwfunc
        replace_match!(x ->  Base.Meta.isexpr(x, :call) && x.args[1] == untag_call_sentinel, overdubbed_code) do x
            return Expr(:call, GlobalRef(Cassette, :untag), x.args[2:end]...)
        end
    end

    #=== replace `Expr(:new, ...)` with `Expr(:call, :tagged_new)` if tagging is enabled ===#

    if istaggingenabled && !iskwfunc
        replace_match!(x -> Base.Meta.isexpr(x, :new), overdubbed_code) do x
            return Expr(:call, GlobalRef(Cassette, :tagged_new), overdub_ctx_slot, x.args...)
        end
    end

    #=== set `code_info`/`reflection` fields accordingly ===#

    code_info.code = overdubbed_code
    code_info.codelocs = overdubbed_codelocs
    code_info.ssavaluetypes = length(overdubbed_code)
    code_info.method_for_inference_limit_heuristics = method
    reflection.code_info = code_info

    return reflection
end

# `args` is `(typeof(original_function), map(typeof, original_args_tuple)...)`
function overdub_generator(pass_type, self, context_type, args::Tuple)
    if !(nfields(args) > 0 && args[1] <: Core.Builtin)
        try
            untagged_args = ((untagtype(args[i], context_type) for i in 1:nfields(args))...,)
            reflection = reflect(untagged_args)
            if isa(reflection, Reflection)
                overdub_pass!(reflection, context_type, pass_type)
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
        $Cassette.fallback($OVERDUB_CTX_SYMBOL, $OVERDUB_ARGS_SYMBOL...)
    end
end

@inline apply_args(::ContextWithTag{Nothing}, args...) = Core._apply(Core.tuple, args...)
@inline apply_args(ctx::Context, args...) = tagged_apply_args(ctx, args...)

function overdub_definition(pass, line, file)
    return quote
        function overdub($OVERDUB_CTX_SYMBOL::ContextWithPass{pass}, $OVERDUB_ARGS_SYMBOL...) where {pass<:$pass}
            $(Expr(:meta,
                   :generated,
                   Expr(:new,
                        Core.GeneratedFunctionStub,
                        :overdub_generator,
                        Any[:overdub, OVERDUB_CTX_SYMBOL, OVERDUB_ARGS_SYMBOL],
                        Any[:pass],
                        line,
                        QuoteNode(Symbol(file)),
                        true)))
        end
        @inline function overdub(ctx::ContextWithPass{pass}, ::typeof(Core._apply), f, _args...) where {pass<:$pass}
            args = apply_args(ctx, _args...)
            prehook(ctx, f, args...)
            output = execute(ctx, f, args...)
            output = isa(output, OverdubInstead) ? overdub(ctx, f, args...) : output
            posthook(ctx, output, f, args...)
            return output
        end
    end
end

@eval $(overdub_definition(:NoPass, @__LINE__, @__FILE__))
