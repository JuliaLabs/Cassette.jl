##############
# Reflection #
##############

"""
    Cassette.Reflection

A struct representing the information retrieved via `Cassette.reflect`.

A `Reflection` is essentially just a convenient bundle of information about a
specific method invocation.

# Fields

- `signature`: the invocation signature (in `Tuple{...}` type form) for the invoked method.

- `method`: the `Method` object associated with the invoked method.

- `static_params`: a `Vector` representing the invoked method's static parameter list.

- `code_info`: the `CodeInfo` object associated with the invoked method.
"""
mutable struct Reflection
    signature::DataType
    method::Method
    static_params::Vector{Any}
    code_info::CodeInfo
end

if VERSION < v"1.1.0-DEV.762"
    copy_code_info(code_info) = Core.Compiler.copy_code_info(code_info)
else
    copy_code_info(code_info) = copy(code_info)
end

if VERSION < v"1.2.0-DEV.573"
    specialize_method(method, metharg, methsp, world, force) = Core.Compiler.code_for_method(method, metharg, methsp, world, force)
else
    specialize_method(method, metharg, methsp, world, force) = Core.Compiler.specialize_method(method, metharg, methsp, force)
end



# Return `Reflection` for signature `sigtypes` and `world`, if possible. Otherwise, return `nothing`.
function reflect(@nospecialize(sigtypes::Tuple), world::UInt = typemax(UInt))
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
    method_instance = specialize_method(method, type_signature, raw_static_params, world, false)
    method_instance === nothing && return nothing
    method_signature = method.sig
    static_params = Any[raw_static_params...]
    code_info = Core.Compiler.retrieve_code_info(method_instance)
    isa(code_info, CodeInfo) || return nothing
    code_info = copy_code_info(code_info)
    return Reflection(S, method, static_params, code_info)
end

###########
# overdub #
###########

"""
    Cassette.OVERDUB_CONTEXT_NAME

The variable name bound to `overdub`'s `Context` argument in its `@generated`
method definition.

This binding can be used to manually reference/destructure `overdub` arguments
within `Expr` thunks emitted by user-provided passes.

See also: [`OVERDUB_ARGUMENTS_NAME`](@ref), [`@pass`](@ref), [`overdub`](@ref)
"""
const OVERDUB_CONTEXT_NAME = gensym("overdub_context")

"""
    Cassette.OVERDUB_ARGUMENTS_NAME

The variable name bound to `overdub`'s tuple of non-`Context` arguments in its
`@generated` method definition.

This binding can be used to manually reference/destructure `overdub` arguments
within `Expr` thunks emitted by user-provided passes.

See also: [`OVERDUB_CONTEXT_NAME`](@ref), [`@pass`](@ref), [`overdub`](@ref)
"""
const OVERDUB_ARGUMENTS_NAME = gensym("overdub_arguments")

# The `overdub` pass has four intertwined tasks:
#   1. Apply the user-provided pass, if one is given
#   2. Munge the reflection-generated IR into a valid form for returning from
#      `overdub_generator` (i.e. add new argument slots, substitute static
#      parameters, destructure overdub arguments into underlying method slots, etc.)
#   3. Perform the statement replacement central to the overdubbing pass (see `overdub` docstring)
#   4. If tagging is enabled, do the necessary IR transforms for the metadata tagging system
function overdub_pass!(reflection::Reflection,
                       context_type::DataType,
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
        code_info = passtype(context_type)(context_type, reflection)
        isa(code_info, Expr) && return code_info
    end

    #=== munge the code into a valid form for `overdub_generator` ===#

    # NOTE: The slotflags set by this pass are set according to what makes sense based on the
    # compiler's actual `@code_lowered` output in practice, since this real-world output does
    # not seem to match Julia's developer documentation.

    # construct new slotnames/slotflags for added slots
    code_info.slotnames = Any[:overdub, OVERDUB_CONTEXT_NAME, OVERDUB_ARGUMENTS_NAME, code_info.slotnames...]
    code_info.slotflags = UInt8[0x00, 0x00, 0x00, code_info.slotflags...]
    n_prepended_slots = 3
    overdub_ctx_slot = SlotNumber(2)
    overdub_args_slot = SlotNumber(3)
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
            x.mod === Core && in(x.name, (:tuple, :_apply)) && return x
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
        # of `f`. Thus, we `recurse` that call instead of replacing it with `call`.
        for i in 1:length(overdubbed_code)
            stmt = overdubbed_code[i]
            replacewith = i === (length(overdubbed_code) - 1) ? :recurse : :call
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
        arehooksenabled = hashooks(context_type)
        stmtcount = (x, i) -> begin
            i >= original_code_start_index || return nothing
            isassign = Base.Meta.isexpr(x, :(=))
            stmt = isassign ? x.args[2] : x
            if Base.Meta.isexpr(stmt, :call) && !(Base.Meta.isexpr(stmt.args[1], :nooverdub))
                isapplycall = is_ir_element(stmt.args[1], GlobalRef(Core, :_apply), overdubbed_code)
                if isapplycall && arehooksenabled
                    return 7
                elseif isapplycall
                    return 2 + isassign
                elseif arehooksenabled
                    return 4
                else
                    return 1 + isassign
                end
            end
            return nothing
        end
        newstmts = (x, i) -> begin
            callstmt = Base.Meta.isexpr(x, :(=)) ? x.args[2] : x
            isapplycall = is_ir_element(callstmt.args[1], GlobalRef(Core, :_apply), overdubbed_code)
            if isapplycall && arehooksenabled
                callf = callstmt.args[2]
                callargs = callstmt.args[3:end]
                stmts = Any[
                    Expr(:call, GlobalRef(Core, :tuple), overdub_ctx_slot),
                    Expr(:call, GlobalRef(Core, :tuple), callf),
                    Expr(:call, GlobalRef(Core, :_apply), GlobalRef(Cassette, :prehook), SSAValue(i), SSAValue(i + 1), callargs...),
                    Expr(:call, GlobalRef(Core, :_apply), GlobalRef(Cassette, :overdub), SSAValue(i), SSAValue(i + 1), callargs...),
                    Expr(:call, GlobalRef(Core, :tuple), SSAValue(i + 3)),
                    Expr(:call, GlobalRef(Core, :_apply), GlobalRef(Cassette, :posthook), SSAValue(i), SSAValue(i + 4), SSAValue(i + 1), callargs...),
                    Base.Meta.isexpr(x, :(=)) ? Expr(:(=), x.args[1], SSAValue(i + 3)) : SSAValue(i + 3)
                ]
            elseif isapplycall
                callf = callstmt.args[2]
                callargs = callstmt.args[3:end]
                stmts = Any[
                    Expr(:call, GlobalRef(Core, :tuple), overdub_ctx_slot, callf),
                    Expr(:call, GlobalRef(Core, :_apply), GlobalRef(Cassette, :overdub), SSAValue(i), callargs...),
                ]
                Base.Meta.isexpr(x, :(=)) && push!(stmts, Expr(:(=), x.args[1], SSAValue(i + 1)))
            elseif arehooksenabled
                stmts = Any[
                    Expr(:call, GlobalRef(Cassette, :prehook), overdub_ctx_slot, callstmt.args...),
                    Expr(:call, GlobalRef(Cassette, :overdub), overdub_ctx_slot, callstmt.args...),
                    Expr(:call, GlobalRef(Cassette, :posthook), overdub_ctx_slot, SSAValue(i + 1), callstmt.args...),
                    Base.Meta.isexpr(x, :(=)) ? Expr(:(=), x.args[1], SSAValue(i + 1)) : SSAValue(i + 1)
                ]
            else
                stmts = Any[
                    Expr(:call, GlobalRef(Cassette, :overdub), overdub_ctx_slot, callstmt.args...),
                ]
                Base.Meta.isexpr(x, :(=)) && push!(stmts, Expr(:(=), x.args[1], SSAValue(i)))
            end
            return stmts
        end
        insert_statements!(overdubbed_code, overdubbed_codelocs, stmtcount, newstmts)
    end

    #=== unwrap all `Expr(:nooverdub)`s ===#

    replace_match!(x -> x.args[1], x -> Base.Meta.isexpr(x, :nooverdub), overdubbed_code)

    #=== replace all `Expr(:contextslot)`s ===#

    replace_match!(x -> overdub_ctx_slot, x -> Base.Meta.isexpr(x, :contextslot), overdubbed_code)

    #=== set `code_info`/`reflection` fields accordingly ===#

    if code_info.method_for_inference_limit_heuristics === nothing
        code_info.method_for_inference_limit_heuristics = method
    end

    code_info.code = overdubbed_code
    code_info.codelocs = overdubbed_codelocs
    code_info.ssavaluetypes = length(overdubbed_code)
    reflection.code_info = code_info

    return reflection
end

@eval _overdub_fallback($OVERDUB_CONTEXT_NAME, $OVERDUB_ARGUMENTS_NAME...) = fallback($OVERDUB_CONTEXT_NAME, $OVERDUB_ARGUMENTS_NAME...)

const OVERDUB_FALLBACK = begin
    code_info = reflect((typeof(_overdub_fallback), Any, Vararg{Any})).code_info
    code_info.inlineable = true
    code_info
end

# `args` is `(typeof(original_function), map(typeof, original_args_tuple)...)`
function __overdub_generator__(self, context_type, args::Tuple)
    if nfields(args) > 0
        is_builtin = args[1] <: Core.Builtin
        is_invoke = args[1] === typeof(Core.invoke)
        if !is_builtin || is_invoke
            try
                untagged_args = ((untagtype(args[i], context_type) for i in 1:nfields(args))...,)
                reflection = reflect(untagged_args)
                if isa(reflection, Reflection)
                    result = overdub_pass!(reflection, context_type, is_invoke)
                    isa(result, Expr) && return result
                    return reflection.code_info
                end
            catch err
                errmsg = "ERROR COMPILING $args IN CONTEXT $(context_type): \n" * sprint(showerror, err)
                errmsg *= "\n" .* repr("text/plain", stacktrace(catch_backtrace()))
                return quote
                    error($errmsg)
                end
            end
        end
    end
    return copy_code_info(OVERDUB_FALLBACK)
end

function overdub end

function recurse end

recurse(ctx::Context, ::typeof(Core._apply), f, args...) = Core._apply(recurse, (ctx, f), args...)

function overdub_definition(line, file)
    return quote
        function $Cassette.overdub($OVERDUB_CONTEXT_NAME::$Cassette.Context, $OVERDUB_ARGUMENTS_NAME...)
            $(Expr(:meta, :generated_only))
            $(Expr(:meta,
                   :generated,
                   Expr(:new,
                        Core.GeneratedFunctionStub,
                        :__overdub_generator__,
                        Any[:overdub, OVERDUB_CONTEXT_NAME, OVERDUB_ARGUMENTS_NAME],
                        Any[],
                        line,
                        QuoteNode(Symbol(file)),
                        true)))
        end
        function $Cassette.recurse($OVERDUB_CONTEXT_NAME::$Cassette.Context, $OVERDUB_ARGUMENTS_NAME...)
            $(Expr(:meta, :generated_only))
            $(Expr(:meta,
                   :generated,
                   Expr(:new,
                        Core.GeneratedFunctionStub,
                        :__overdub_generator__,
                        Any[:recurse, OVERDUB_CONTEXT_NAME, OVERDUB_ARGUMENTS_NAME],
                        Any[],
                        line,
                        QuoteNode(Symbol(file)),
                        true)))
        end
    end
end

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
    overdub(context, g, x...) # %n
    posthook(context, %n, g, x...)
    %n
end
```

Otherwise, if Cassette cannot retrieve lowered IR for the method body of `f(args...)`,
then `fallback(context, f, args...)` will be called instead. Cassette's [`canrecurse`](@ref)
function is a useful utility for checking if this will occur.

If the injected `prehook`/`posthook` statements are not needed for your use
case, you can disable their injection via the [`disablehooks`](@ref) function.

Additionally, for every method body encountered in the execution trace, apply the compiler pass
associated with `context` if one exists. Note that this user-provided pass is performed on
the method IR before method invocations are transformed into the form specified above. See
the [`@pass`](@ref) macro for further details.

If `Cassette.hastagging(typeof(context))`, then a number of additional passes are run in
order to accomodate tagged value propagation:

- `Expr(:new)` is replaced with a call to `Cassette.tagged_new`
- conditional values passed to `Expr(:gotoifnot)` are untagged
- arguments to `Expr(:foreigncall)` are untagged
- load/stores to external module bindings are intercepted by the tagging system

The default definition of `overdub` is to recursively enter the given function
and continue overdubbing, but one can interrupt/redirect this recursion by
overloading `overdub` w.r.t. a given context and/or method signature to define
new contextual execution primitives. For example:

```
julia> using Cassette

julia> Cassette.@context Ctx;

julia> Cassette.overdub(::Ctx, ::typeof(sin), x) = cos(x)

julia> Cassette.overdub(Ctx(), x -> sin(x) + cos(x), 1) == 2 * cos(1)
true
```

See also: [`recurse`](@ref), [`prehook`](@ref), [`posthook`](@ref)
""",
overdub)

@doc(
"""
```
recurse(context::Context, f, args...)
```

Execute `f(args...)` overdubbed with respect to `context`.

This method performs exactly the same transformation as the default
[`overdub`](@ref) transformation, but is not meant to be overloaded. Thus, one
can call `recurse` to "continue" recursively overdubbing a function when calling
`overdub` directly on that function might've dispatched to a contextual
primitive.

To illustrate why `recurse` might be useful, consider the following example
which utilizes `recurse` as part of a Cassette-based memoization implementation
for the classic Fibonacci function:

```
using Cassette: Cassette, @context, overdub, recurse

fib(x) = x < 3 ? 1 : fib(x - 2) + fib(x - 1)
fibtest(n) = fib(2 * n) + n

@context MemoizeCtx

function Cassette.overdub(ctx::MemoizeCtx, ::typeof(fib), x)
    result = get(ctx.metadata, x, 0)
    if result === 0
        result = recurse(ctx, fib, x)
        ctx.metadata[x] = result
    end
    return result
end
```

See Cassette's Contextual Dispatch documentation for more details and examples.
""",
recurse)

"""
```
Cassette.@overdub(ctx, expression)
```

A convenience macro for executing `expression` within the context `ctx`. This
macro roughly expands to `Cassette.recurse(ctx, () -> expression)`.

See also: [`overdub`](@ref), [`recurse`](@ref)
"""
macro overdub(ctx, expr)
    return :(recurse($(esc(ctx)), () -> $(esc(expr))))
end
