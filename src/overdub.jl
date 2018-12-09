##############
# Reflection #
##############

mutable struct Reflection
    signature::DataType
    method::Method
    static_params::Vector{Any}
    code_info::CodeInfo
    world::UInt
end

if VERSION < v"1.1.0-DEV.762"
    copy_code_info(code_info) = Core.Compiler.copy_code_info(code_info)
else
    copy_code_info(code_info) = copy(code_info)
end

# Return `Reflection` for signature `sigtypes` and `world`, if possible. Otherwise, return `nothing`.
function reflect(@nospecialize(sigtypes::Tuple), world::UInt = typemax(UInt); method_only = false)
    if length(sigtypes) > 2 && sigtypes[1] === typeof(invoke)
        @assert sigtypes[3] <: Type{<:Tuple}
        sigtypes = (sigtypes[2], sigtypes[3].parameters[1].parameters...)
    end
    # This works around a subtyping bug. Basically, callers can deconstruct upstream
    # `UnionAll` types in such a way that results in a type with free type variables, in
    # which case subtyping can just break.
    #
    # God help you if you try to use a type parameter here (e.g. `::Type{S} where S<:Tuple`)
    # instead of this nutty workaround, because the compiler can just rewrite `S` into
    # whatever it thinks is "type equal" to the actual provided value. In other words, if
    # `S` is defined as e.g. `f(::Type{S}) where S`, and you call `f(T)`, you should NOT
    # assume that `S === T`. If you did, SHAME ON YOU. It doesn't matter that such an
    # assumption holds true for essentially all other kinds of values. I haven't counted in
    # a while, but I'm pretty sure I have ~40+ hellish years of Julia experience, and this
    # still catches me every time. Who even uses this crazy language?
    S = Tuple{map(s -> Core.Compiler.has_free_typevars(s) ? typeof(s.parameters[1]) : s, sigtypes)...}
    (S.parameters[1]::DataType).name.module === Core.Compiler && return nothing
    _methods = Base._methods_by_ftype(S, -1, world)
    method_index = 0
    for i in 1:length(_methods)
        if _methods[i][1] === S
            method_index = i
            break
        end
    end
    method_index === 0 && return nothing
    type_signature, raw_static_params, method = _methods[method_index]
    method_only && return method
    method_instance = Core.Compiler.code_for_method(method, type_signature, raw_static_params, world, false)
    method_instance === nothing && return nothing
    method_signature = method.sig
    static_params = Any[raw_static_params...]
    code_info = Core.Compiler.retrieve_code_info(method_instance)
    isa(code_info, CodeInfo) || return nothing
    code_info = copy_code_info(code_info)
    return Reflection(S, method, static_params, code_info, world)
end

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
#      output = overdub(ctx, f, args...)
#      posthook(ctx, f, args...)
#      ```
#   4. If tagging is enabled, do the necessary IR transforms for the metadata tagging system
function overdub_pass!(reflection::Reflection,
                       context_type::DataType,
                       pass_type::DataType = NoPass,
                       is_invoke::Bool = false)
    signature = reflection.signature
    method = reflection.method
    static_params = reflection.static_params
    code_info = reflection.code_info

    # TODO: This `iskwfunc` is part of a hack that `overdub_pass!` implements in order to fix
    # jrevels/Cassette.jl#48. The assumptions made by this hack are quite fragile, so we
    # should eventually get Base to expose a standard/documented API for this. Here, we see
    # this hack's first assumption: that `Core.kwfunc(f)` is going to return a function whose
    # type name is prefixed by `#kw##`. More assumptions for this hack will be commented on
    # as we go.
    iskwfunc = startswith(String(signature.parameters[1].name.name), "#kw##")
    istaggingenabled = hastagging(context_type)

    #=== execute user-provided pass (is a no-op by default) ===#

    if !iskwfunc
        code_info = pass_type(context_type, signature, code_info)
    end

    #=== munge the code into a valid form for `overdub_generator` ===#

    # NOTE: The slotflags set by this pass are set according to what makes sense based on the
    # compiler's actual `@code_lowered` output in practice, since this real-world output does
    # not seem to match Julia's developer documentation.

    # construct new slotnames/slotflags for added slots
    code_info.slotnames = Any[:overdub, OVERDUB_CTX_SYMBOL, OVERDUB_ARGS_SYMBOL, code_info.slotnames..., OVERDUB_TMP_SYMBOL]
    code_info.slotflags = UInt8[0x00, 0x00, 0x00, code_info.slotflags..., 0x00]
    n_prepended_slots = 3
    overdub_ctx_slot = SlotNumber(2)
    overdub_args_slot = SlotNumber(3)
    overdub_tmp_slot = SlotNumber(length(code_info.slotnames))
    invoke_offset = is_invoke ? 2 : 0

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
        actual_argument = Expr(:call, GlobalRef(Core, :getfield), overdub_args_slot, i + invoke_offset)
        push!(overdubbed_code, :($(SlotNumber(slot)) = $actual_argument))
        push!(overdubbed_codelocs, code_info.codelocs[1])
        code_info.slotflags[slot] |= 0x02 # ensure this slotflag has the "assigned" bit set
    end

    # If `method` is a varargs method, we have to restructure the original method call's
    # trailing arguments into a tuple and assign that tuple to the expected argument slot.
    if method.isva
        if !isempty(overdubbed_code)
            # remove the final slot reassignment leftover from the previous destructuring
            pop!(overdubbed_code)
            pop!(overdubbed_codelocs)
        end
        if hastagging(context_type)
            trailing_arguments = Expr(:call, GlobalRef(Cassette, :_tagged_new_tuple_unsafe), overdub_ctx_slot)
        else
            trailing_arguments = Expr(:call, GlobalRef(Core, :tuple))
        end
        for i in n_method_args:n_actual_args
            push!(overdubbed_code, Expr(:call, GlobalRef(Core, :getfield), overdub_args_slot, i + invoke_offset))
            push!(overdubbed_codelocs, code_info.codelocs[1])
            push!(trailing_arguments.args, SSAValue(length(overdubbed_code)))
        end
        push!(overdubbed_code, Expr(:(=), SlotNumber(n_method_args + n_prepended_slots), trailing_arguments))
        push!(overdubbed_codelocs, code_info.codelocs[1])
    end

    #=== finish initialization of `overdubbed_code`/`overdubbed_codelocs` ===#

    # substitute static parameters, offset slot numbers by number of added slots, and
    # offset statement indices by the number of additional statements
    Base.Meta.partially_inline!(code_info.code, Any[], method.sig, static_params,
                                n_prepended_slots, length(overdubbed_code), :propagate)

    original_code_start_index = length(overdubbed_code) + 1

    append!(overdubbed_code, code_info.code)
    append!(overdubbed_codelocs, code_info.codelocs)

    #=== perform tagged module transformation if tagging is enabled ===#

    if istaggingenabled && !iskwfunc
        # find `GlobalRef`s in IR and set up tagged module replacements
        modules = Any[]
        original_code_region = view(overdubbed_code, original_code_start_index:length(overdubbed_code))
        replace_match!(x -> isa(x, GlobalRef), original_code_region) do x
            m = GlobalRef(parentmodule(x.mod), nameof(x.mod))
            i = findfirst(isequal(m), modules)
            if isa(i, Nothing)
                push!(modules, m)
                i = length(modules)
            end
            return Expr(:replaceglobalref, original_code_start_index + i - 1, x)
        end
        for i in 1:length(modules)
            modules[i] = Expr(:call, Expr(:nooverdub, GlobalRef(Cassette, :fetch_tagged_module)), overdub_ctx_slot, modules[i])
        end

        # insert `fetch_tagged_module`s at the `original_code_start_index`
        insert_statements!(overdubbed_code, overdubbed_codelocs,
                            (x, i) -> i == original_code_start_index ? length(modules) + 1 : nothing,
                            (x, i) -> [modules..., x])

        # append `tagged_globalref_set_meta!` to `GlobalRef() = ...` statements
        insert_statements!(overdubbed_code, overdubbed_codelocs,
                            (x, i) -> begin
                                if (i > original_code_start_index &&
                                    Base.Meta.isexpr(x, :(=)) &&
                                    Base.Meta.isexpr(x.args[1], :replaceglobalref))
                                    return 3
                                end
                                return nothing
                            end,
                            (x, i) -> begin
                                lhs, rhs = x.args
                                tagmodssa = SSAValue(lhs.args[1])
                                globalref = lhs.args[2]
                                name = QuoteNode(globalref.name)
                                return [
                                    rhs,
                                    Expr(:(=), globalref, Expr(:call, Expr(:nooverdub, GlobalRef(Cassette, :untag)), SSAValue(i), overdub_ctx_slot)),
                                    Expr(:call, Expr(:nooverdub, GlobalRef(Cassette, :tagged_globalref_set_meta!)), overdub_ctx_slot, tagmodssa, name, SSAValue(i))
                                ]
                            end)

        # replace `GlobalRef`-loads with `Cassette.tagged_globalref`
        original_code_start_index += length(modules)
        insert_statements!(overdubbed_code, overdubbed_codelocs,
                            (x, i) -> begin
                                i >= original_code_start_index || return nothing
                                stmt = Base.Meta.isexpr(x, :(=)) ? x.args[2] : x
                                Base.Meta.isexpr(stmt, :replaceglobalref) && return 1
                                if isa(stmt, Expr) # Base.Meta.isexpr(stmt, :call) || Base.Meta.isexpr(stmt, :new) || Base.Meta.isexpr(stmt, :return)
                                    count = 0
                                    for arg in stmt.args
                                        if Base.Meta.isexpr(arg, :replaceglobalref)
                                            count += 1
                                        end
                                    end
                                    count > 0 && return count + 1
                                end
                                return nothing
                            end,
                            (x, i) -> begin
                                items = Any[]
                                stmt = Base.Meta.isexpr(x, :(=)) ? x.args[2] : x
                                if Base.Meta.isexpr(stmt, :replaceglobalref)
                                    tagmodssa = SSAValue(stmt.args[1])
                                    globalref = stmt.args[2]
                                    name = QuoteNode(globalref.name)
                                    result = Expr(:call, Expr(:nooverdub, GlobalRef(Cassette, :tagged_globalref)), overdub_ctx_slot, tagmodssa, name, globalref)
                                elseif isa(stmt, Expr) # Base.Meta.isexpr(stmt, :call) || Base.Meta.isexpr(stmt, :new) || Base.Meta.isexpr(stmt, :return)
                                    result = Expr(stmt.head)
                                    for arg in stmt.args
                                        if Base.Meta.isexpr(arg, :replaceglobalref)
                                            tagmodssa = SSAValue(arg.args[1])
                                            globalref = arg.args[2]
                                            name = QuoteNode(globalref.name)
                                            push!(result.args, SSAValue(i + length(items)))
                                            push!(items, Expr(:call, Expr(:nooverdub, GlobalRef(Cassette, :tagged_globalref)), overdub_ctx_slot, tagmodssa, name, globalref))
                                        else
                                            push!(result.args, arg)
                                        end
                                    end
                                end
                                if Base.Meta.isexpr(x, :(=))
                                    result = Expr(:(=), x.args[1], result)
                                end
                                push!(items, result)
                                return items
                            end)
    end

    #=== untag all `foreigncall` SSAValue/SlotNumber arguments if tagging is enabled ===#

    if istaggingenabled && !iskwfunc
        insert_statements!(overdubbed_code, overdubbed_codelocs,
                            (x, i) -> begin
                                stmt = Base.Meta.isexpr(x, :(=)) ? x.args[2] : x
                                if Base.Meta.isexpr(stmt, :foreigncall)
                                    count = 0
                                    for arg in stmt.args
                                        if isa(arg, SSAValue) || isa(arg, SlotNumber)
                                            count += 1
                                        end
                                    end
                                    count > 0 && return count + 1
                                end
                                return nothing
                            end,
                            (x, i) -> begin
                                items = Any[]
                                stmt = Base.Meta.isexpr(x, :(=)) ? x.args[2] : x
                                result = Expr(:foreigncall)
                                for arg in stmt.args
                                    if isa(arg, SSAValue) || isa(arg, SlotNumber)
                                        push!(result.args, SSAValue(i + length(items)))
                                        push!(items, Expr(:call, Expr(:nooverdub, GlobalRef(Cassette, :untag)), arg, overdub_ctx_slot))
                                    else
                                        push!(result.args, arg)
                                    end
                                end
                                if Base.Meta.isexpr(x, :(=))
                                    result = Expr(:(=), x.args[1], result)
                                end
                                push!(items, result)
                                return items
                            end)
    end

    #=== untag `gotoifnot` conditionals if tagging is enabled ===#

    if istaggingenabled && !iskwfunc
        insert_statements!(overdubbed_code, overdubbed_codelocs,
                            (x, i) -> Base.Meta.isexpr(x, :gotoifnot) ? 2 : nothing,
                            (x, i) -> [
                                Expr(:call, Expr(:nooverdub, GlobalRef(Cassette, :untag)), x.args[1], overdub_ctx_slot),
                                Expr(:gotoifnot, SSAValue(i), x.args[2])
                            ])
    end

    #=== replace `Expr(:new, ...)` with `Expr(:call, :tagged_new)` if tagging is enabled ===#

    if istaggingenabled && !iskwfunc
        replace_match!(x -> Base.Meta.isexpr(x, :new), overdubbed_code) do x
            return Expr(:call, Expr(:nooverdub, GlobalRef(Cassette, :tagged_new)), overdub_ctx_slot, x.args...)
        end
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
        stmtcount = (x, i) -> begin
            i >= original_code_start_index || return nothing
            stmt = Base.Meta.isexpr(x, :(=)) ? x.args[2] : x
            if Base.Meta.isexpr(stmt, :call) && !(Base.Meta.isexpr(stmt.args[1], :nooverdub))
                return 4
            end
            return nothing
        end
        newstmts = (x, i) -> begin
            callstmt = Base.Meta.isexpr(x, :(=)) ? x.args[2] : x
            execstmt = Expr(:call, GlobalRef(Cassette, :execute), overdub_ctx_slot, callstmt.args...)
            overdubstmt = Expr(:call, GlobalRef(Cassette, :overdub), overdub_ctx_slot, callstmt.args...)
            return [
                Expr(:call, GlobalRef(Cassette, :prehook), overdub_ctx_slot, callstmt.args...),
                Expr(:(=), overdub_tmp_slot, overdubstmt),
                Expr(:call, GlobalRef(Cassette, :posthook), overdub_ctx_slot, overdub_tmp_slot, callstmt.args...),
                Base.Meta.isexpr(x, :(=)) ? Expr(:(=), x.args[1], overdub_tmp_slot) : overdub_tmp_slot
            ]
        end
        insert_statements!(overdubbed_code, overdubbed_codelocs, stmtcount, newstmts)
    end

    #=== unwrap all `Expr(:nooverdub)`s ===#

    replace_match!(x -> x.args[1], x -> Base.Meta.isexpr(x, :nooverdub), overdubbed_code)

    #=== replace all `Expr(:contextslot)`s ===#

    replace_match!(x -> overdub_ctx_slot, x -> Base.Meta.isexpr(x, :contextslot), overdubbed_code)

    #=== set `code_info`/`reflection` fields accordingly ===#

    if code_info.method_for_inference_limit_heuristics === nothing
        underlying_method = method

        # TODO: This doesn't seem to improve our test case any, though
        # it should have some advantages in theory...
        # There is still an issue with constant-propping through `getfield`
        # that has been nested overdubbed. Do an InferenceResult cache check
        # to see where it gets inferred as Union{$element_types...} instead
        # of correctly inferred.

        # underlying_type_parameters = (signature.parameters...,)
        # while isa(underlying_method, Method) && underlying_method.name == :overdub
        #     if length(underlying_type_parameters) > 2
        #         underlying_type_parameters = (underlying_type_parameters[3:end]...,)
        #         result = reflect(underlying_type_parameters, reflection.world; method_only = true)
        #         result === nothing && break # keep previous method token
        #         underlying_method = result
        #     else
        #         break
        #     end
        # end

        code_info.method_for_inference_limit_heuristics = underlying_method
    end

    code_info.code = overdubbed_code
    code_info.codelocs = overdubbed_codelocs
    code_info.ssavaluetypes = length(overdubbed_code)
    reflection.code_info = code_info

    return reflection
end

pass_type_from_context_type(::Type{<:Context{<:Any,<:Any,P}}) where {P} = P

# `args` is `(typeof(original_function), map(typeof, original_args_tuple)...)`
function __overdub_generator__(self, context_type, args::Tuple)
    pass_type = pass_type_from_context_type(context_type)
    if nfields(args) > 0
        is_builtin = args[1] <: Core.Builtin
        is_invoke = args[1] === typeof(Core.invoke)
        if !is_builtin || is_invoke
            try
                untagged_args = ((untagtype(args[i], context_type) for i in 1:nfields(args))...,)
                reflection = reflect(untagged_args)
                if isa(reflection, Reflection)
                    overdub_pass!(reflection, context_type, pass_type, is_invoke)
                    return reflection.code_info
                end
            catch err
                errmsg = "ERROR COMPILING $args IN CONTEXT $(context_type): \n" * sprint(showerror, err)
                return quote
                    error($errmsg)
                end
            end
        end
    end
    return quote
        $(Expr(:meta, :inline))
        $Cassette.fallback($OVERDUB_CTX_SYMBOL, $OVERDUB_ARGS_SYMBOL...)
    end
end

@inline apply_args(::ContextWithTag{Nothing}, args...) = Core._apply(Core.tuple, args...)
@inline apply_args(ctx::Context, args...) = tagged_apply_args(ctx, args...)

function overdub end

function overdub_definition(line, file)
    return quote
        function $Cassette.overdub($OVERDUB_CTX_SYMBOL, $OVERDUB_ARGS_SYMBOL...)
            $(Expr(:meta,
                   :generated,
                   Expr(:new,
                        Core.GeneratedFunctionStub,
                        :__overdub_generator__,
                        Any[:overdub, OVERDUB_CTX_SYMBOL, OVERDUB_ARGS_SYMBOL],
                        Any[],
                        line,
                        QuoteNode(Symbol(file)),
                        true)))
        end
    end
end

@eval $(overdub_definition(@__LINE__, @__FILE__))

@doc(
"""
```
overdub(context::Context, f, args...)
```

Execute `f(args...)` overdubbed with respect to `context`.

More specifically, execute `f(args...)`, but with every internal method invocation `g(x...)`
replaced by statements similar to the following:

```
begin
    prehook(context, g, x...)
    tmp = overdub(context, g, x...)
    tmp = isa(tmp, Cassette.OverdubInstead) ? overdub(context, g, x...) : tmp
    posthook(context, tmp, g, x...)
    tmp
end
```

If Cassette cannot retrieve lowered IR for the method body of `f(args...)` (as determined by
`canoverdub(context, f, args...)`), then `overdub(context, f, args...)` will directly
translate to a call to `fallback(context, f, args...)`.

Additionally, for every method body encountered in execute trace, apply the compiler pass
associated with `context` if one exists. Note that this user-provided pass is performed on
the method IR before method invocations are transformed into the form specified above. See
the [`@pass`](@ref) macro for further details.

If `Cassette.hastagging(typeof(context))`, then a number of additional passes are run in
order to accomodate tagged value propagation:

- `Expr(:new)` is replaced with a call to `Cassette.tagged_new`
- conditional values passed to `Expr(:gotoifnot)` are untagged
- arguments to `Expr(:foreigncall)` are untagged
- load/stores to external module bindings are intercepted by the tagging system
""",
overdub)

"""
```
Cassette.@overdub(ctx, expression)
```

A convenience macro for executing `expression` within the context `ctx`. This macro roughly
expands to `Cassette.overdub(ctx, () -> expression)`.

See also: [`overdub`](@ref)
"""
macro overdub(ctx, expr)
    return :($Cassette.overdub($(esc(ctx)), () -> $(esc(expr))))
end

"""
```
execute(context::Context, f, args...)
```

Overload this Cassette method w.r.t. a given context in order to define a new contextual
execution primitive for that context.

To understand when/how this method is called, see the documentation for [`overdub`](@ref).

Invoking `execute` immediately returns `Cassette.OverdubInstead()` by default.

See also: [`overdub`](@ref), [`prehook`](@ref), [`posthook`](@ref), [`fallback`](@ref)
"""
const execute = overdub # TODO: deprecate `execute`
