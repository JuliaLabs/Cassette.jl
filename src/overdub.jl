#########################
# contextual operations #
#########################

struct OverdubInstead end

"""
```
prehook(context::Context, f, args...)
```

Overload this Cassette method w.r.t. a given context in order to define a new contextual
prehook for that context.

To understand when/how this method is called, see the documentation for [`overdub`](@ref).

Invoking `prehook` is a no-op by default (it immediately returns `nothing`).

See also: [`overdub`](@ref), [`posthook`](@ref), [`execute`](@ref), [`fallback`](@ref)

# Examples

Simple trace logging:

```
julia> Cassette.@context PrintCtx;

julia> Cassette.prehook(::PrintCtx, f, args...) = println(f, args)

julia> Cassette.overdub(PrintCtx(), /, 1, 2)
float(1,)
AbstractFloat(1,)
Float64(1,)
sitofp(Float64, 1)
float(2,)
AbstractFloat(2,)
Float64(2,)
sitofp(Float64, 2)
/(1.0, 2.0)
div_float(1.0, 2.0)
0.5
```

Counting the number of method invocations with one or more arguments of a given type:

```
julia> mutable struct Count{T}
           count::Int
       end

julia> Cassette.@context CountCtx;

julia> Cassette.prehook(ctx::CountCtx{Count{T}}, f, arg::T, args::T...) where {T} = (ctx.metadata.count += 1)

# count the number of calls of the form `f(::Float64, ::Float64...)`
julia> ctx = CountCtx(metadata = Count{Float64}(0));

julia> Cassette.overdub(ctx, /, 1, 2)
0.5

julia> ctx.metadata.count
2
```
"""
@inline prehook(::Context, ::Vararg{Any}) = nothing

"""
```
posthook(context::Context, output, f, args...)
```

Overload this Cassette method w.r.t. a given context in order to define a new contextual
posthook for that context.

To understand when/how this method is called, see the documentation for [`overdub`](@ref).

Invoking `posthook` is a no-op by default (it immediately returns `nothing`).

See also: [`overdub`](@ref), [`prehook`](@ref), [`execute`](@ref), [`fallback`](@ref)

# Examples

Simple trace logging:

```
julia> Cassette.@context PrintCtx;

julia> Cassette.posthook(::PrintCtx, output, f, args...) = println(output, " = ", f, args)

julia> Cassette.overdub(PrintCtx(), /, 1, 2)
1.0 = sitofp(Float64, 1)
1.0 = Float64(1,)
1.0 = AbstractFloat(1,)
1.0 = float(1,)
2.0 = sitofp(Float64, 2)
2.0 = Float64(2,)
2.0 = AbstractFloat(2,)
2.0 = float(2,)
0.5 = div_float(1.0, 2.0)
0.5 = /(1.0, 2.0)
0.5
```

Accumulate the sum of all numeric scalar outputs encountered in the trace:

```
julia> mutable struct Accum
           x::Number
       end

julia> Cassette.@context AccumCtx;

julia> Cassette.posthook(ctx::AccumCtx{Accum}, out::Number, f, args...) = (ctx.metadata.x += out)

julia> ctx = AccumCtx(metadata = Accum(0));

julia> Cassette.overdub(ctx, /, 1, 2)
0.5

julia> ctx.metadata.x
13.0
```
"""
@inline posthook(::Context, ::Vararg{Any}) = nothing

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
@inline execute(ctx::Context, args...) = OverdubInstead()

"""
```
fallback(context::Context, f, args...)
```

Overload this Cassette method w.r.t. a given context in order to define a new contextual
execution fallback for that context.

To understand when/how this method is called, see the documentation for [`overdub`](@ref) and
[`canoverdub`](@ref).

By default, invoking `fallback(context, f, args...)` will simply call `f(args...)` (with all
arguments automatically untagged, if `hastagging(typeof(context))`).

See also:  [`canoverdub`](@ref), [`overdub`](@ref), [`execute`](@ref), [`prehook`](@ref), [`posthook`](@ref)
"""
@inline fallback(ctx::Context, args...) = call(ctx, args...)

@inline call(::ContextWithTag{Nothing}, f, args...) = f(args...)
@inline call(context::Context, f, args...) = untag(f, context)(ntuple(i -> untag(args[i], context), Val(nfields(args)))...)

# TODO: This is currently needed to force the compiler to specialize on the type arguments
# to `Core.apply_type`. In the future, it would be best for Julia's compiler to better handle
# varargs calls to such functions with type arguments, or at least provide a better way to
# force specialization on the type arguments.
@inline call(::ContextWithTag{Nothing}, f::typeof(Core.apply_type), ::Type{A}, ::Type{B}) where {A,B} = f(A, B)
@inline call(::Context, f::typeof(Core.apply_type), ::Type{A}, ::Type{B}) where {A,B} = f(A, B)

"""
```
canoverdub(context::Context, f, args...)
```

Return `true` if `f(args...)` has a lowered IR representation that Cassette can overdub,
return `false` otherwise.

Alternatively, but equivalently:

Return `false` if `overdub(context, f, args...)` directly translates to
`fallback(context, f, args...)`, return `true` otherwise.

Note that unlike `execute`, `fallback`, etc., this function is not intended to be overloaded.

See also:  [`overdub`](@ref), [`fallback`](@ref), [`execute`](@ref)
"""
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
            push!(overdubbed_code, Expr(:call, GlobalRef(Core, :getfield), overdub_args_slot, i))
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
                return 7
            end
            return nothing
        end
        newstmts = (x, i) -> begin
            callstmt = Base.Meta.isexpr(x, :(=)) ? x.args[2] : x
            execstmt = Expr(:call, GlobalRef(Cassette, :execute), overdub_ctx_slot, callstmt.args...)
            overdubstmt = Expr(:call, GlobalRef(Cassette, :overdub), overdub_ctx_slot, callstmt.args...)
            return [
                Expr(:call, GlobalRef(Cassette, :prehook), overdub_ctx_slot, callstmt.args...),
                Expr(:(=), overdub_tmp_slot, execstmt),
                Expr(:call, GlobalRef(Core, :isa), overdub_tmp_slot, GlobalRef(Cassette, :OverdubInstead)),
                Expr(:gotoifnot, SSAValue(i + 2), i + 5),
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
                return body
            end
        catch err
            errmsg = "ERROR COMPILING $args IN CONTEXT $(context_type): \n" * sprint(showerror, err)
            return quote
                error($errmsg)
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
    tmp = execute(context, g, x...)
    tmp = isa(tmp, Cassette.OverdubInstead) ? overdub(context, g, args...) : tmp
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
