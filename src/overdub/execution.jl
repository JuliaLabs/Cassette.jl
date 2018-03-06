#########
# Phase #
#########

abstract type Phase end

struct Execute <: Phase end

struct Transform <: Phase end

proceed(::Execute) = Transform()
proceed(::Transform) = Execute()

###############
# TraceConfig #
###############

get_world_age() = ccall(:jl_get_tls_world_age, UInt, ()) # ccall(:jl_get_world_counter, UInt, ())

abstract type AbstractPass end

struct TraceConfig{C<:Context,M,w,d,P<:Union{Unused,AbstractPass}}
    context::C
    metadata::M
    world::Val{w}
    debug::Val{d}
    pass::P
end

const AnyTraceConfig{world} = TraceConfig{<:Context,<:Any,world}

function TraceConfig(context::Context;
                     metadata = Unused(),
                     world::Val = Val(get_world_age()),
                     debug::Val = Val(false),
                     pass = Unused())
    return TraceConfig(context, metadata, world, debug, pass)
end

@inline prehook(::AnyTraceConfig{w}, ::Vararg{Any}) where {w} = nothing
@inline posthook(::AnyTraceConfig{w}, ::Vararg{Any}) where {w} = nothing
@inline isprimitive(::AnyTraceConfig{w}, ::Vararg{Any}) where {w} = Val(false)
@inline execution(c::AnyTraceConfig{w}, f, args...) where {w} = mapcall(x -> unbox(c.context, x), f, args...)

###########
# Overdub #
###########

struct Overdub{P<:Phase,F,C<:TraceConfig}
    phase::P
    func::F
    config::C
    function Overdub(phase::P, func, config::C) where {P,C}
        F = Core.Typeof(func) # this yields `Type{T}` instead of `UnionAll` for constructors
        return new{P,F,C}(phase, func, config)
    end
end

@inline overdub(::Type{C}, f; phase = Execute(), kwargs...) where {C<:Context} = overdub(C(f), f; phase = phase, kwargs...)
@inline overdub(ctx::Context, f; phase = Execute(), kwargs...) = Overdub(phase, f, TraceConfig(ctx; kwargs...))

@inline proceed(o::Overdub, f = o.func) = Overdub(proceed(o.phase), f, o.config)

@inline context(o::Overdub) = o.config.context

@inline func(o::Overdub) = o.func
@inline func(f) = f

Base.show(io::IO, o::Overdub{P}) where {P} = print("Overdub{$(P.name.name)}($(typeof(context(o)).name), $(func(o)))")

####################
# Overdub{Execute} #
####################

@inline prehook_overdub(o::Overdub{Execute}, args...) = prehook(o.config, o.func, args...)
@inline posthook_overdub(o::Overdub{Execute}, args...) = posthook(o.config, o.func, args...)
@inline isprimitive_overdub(o::Overdub{Execute}, args...) = isprimitive(o.config, o.func, args...)

@inline execute(o::Overdub{Execute}, args...) = execute(isprimitive_overdub(o, args...), o, args...)
@inline execute(::Val{true}, o::Overdub{Execute}, args...) = execution(o.config, o.func, args...)
@inline execute(::Val{false}, o::Overdub{Execute}, args...) = proceed(o)(args...)

@inline function (o::Overdub{Execute})(args...)
    prehook_overdub(o, args...)
    output = execute(o, args...)
    posthook_overdub(o, output, args...)
    return output
end

######################
# Overdub{Transform} #
######################

# Note that this approach emits code in which LHS SSAValues are not
# monotonically increasing. This currently isn't a problem, but in
# the future, valid IR might require monotonically increasing LHS
# SSAValues, in which case we'll have to add an extra SSA-remapping
# pass to this function.
function overdub_pass!(method_body::CodeInfo)
    # set up new SSAValues
    self_ssa = SSAValue(method_body.ssavaluetypes)
    method_body.ssavaluetypes += 1
    ctx_ssa = SSAValue(method_body.ssavaluetypes)
    greatest_ssa_value = method_body.ssavaluetypes

    # set up replacement code
    new_code = copy_prelude_code(method_body.code)
    prelude_end = length(new_code)
    push!(new_code, :($self_ssa = $(GlobalRef(Cassette, :func))($(SlotNumber(1)))))
    push!(new_code, :($ctx_ssa = $(GlobalRef(Cassette, :context))($(SlotNumber(1)))))
    in_overdub_region = false

    # fill in replacement code
    for i in (prelude_end + 1):length(method_body.code)
        stmnt = method_body.code[i]
        if stmnt === BEGIN_OVERDUB_REGION
            in_overdub_region = true
        else
            # replace `SlotNumber(1)` references with `self_ssa`
            replace_match!(s -> self_ssa, s -> isa(s, SlotNumber) && s.id == 1, stmnt)
            if in_overdub_region
                # replace calls with overdubbed calls
                replace_match!(s -> is_call(s), stmnt) do call
                    greatest_ssa_value += 1
                    new_ssa_value = SSAValue(greatest_ssa_value)
                    new_ssa_stmnt = Expr(:(=), new_ssa_value, Expr(:call, GlobalRef(Cassette, :proceed), SlotNumber(1), call.args[1]))
                    push!(new_code, new_ssa_stmnt)
                    call.args[1] = new_ssa_value
                    return call
                end
            end
            push!(new_code, stmnt)
        end
    end

    # replace all `new` expressions with calls to `Cassette._newbox`
    replace_match!(x -> isa(x, Expr) && x.head === :new, new_code) do x
        return Expr(:call, GlobalRef(Cassette, :_newbox), ctx_ssa, x.args...)
    end

    method_body.code = fix_labels_and_gotos!(new_code)
    method_body.ssavaluetypes = greatest_ssa_value + 1
    return method_body
end

function overdub_transform_call_generator(::Type{F}, ::Type{C}, ::Type{M}, world, debug, pass, f, args) where {F,C,M}
    ftype = unbox(C, F)
    atypes = Tuple(unbox(C, T) for T in args)
    signature = Tuple{ftype,atypes...}
    try
        method_body = lookup_method_body(signature; world = world, debug = debug, pass = pass)
        if isa(method_body, CodeInfo)
            method_body = overdub_pass!(method_body)
            method_body.inlineable = true
            method_body.signature_for_inference_heuristics = Core.svec(ftype, atypes, world)
            debug && Core.println("RETURNING OVERDUBBED CODEINFO: ", sprint(show, method_body))
        else
            method_body = quote
                $(Expr(:meta, :inline))
                $Cassette.execute(Val(true), $Cassette.proceed(f), $(OVERDUB_ARGS_SYMBOL)...)
            end
            debug && Core.println("NO CODEINFO FOUND; EXECUTING AS PRIMITIVE")
        end
        return method_body
    catch err
        errmsg = "ERROR COMPILING $signature IN CONTEXT $C: " * sprint(showerror, err)
        Core.println(errmsg) # in case the returned body doesn't get reached
        return quote
            error($errmsg)
        end
    end
end

function overdub_transform_call_definition(pass, line, file)
    return quote
        function (f::$Cassette.Overdub{$Cassette.Transform,F,$Cassette.TraceConfig{C,M,world,debug,pass}})($(OVERDUB_ARGS_SYMBOL)...) where {F,C,M,world,debug,pass<:$pass}
            $(Expr(:meta,
                   :generated,
                   Expr(:new,
                        Core.GeneratedFunctionStub,
                        :overdub_transform_call_generator,
                        Any[:f, OVERDUB_ARGS_SYMBOL],
                        Any[:F, :C, :M, :world, :debug, :pass],
                        @__LINE__,
                        QuoteNode(Symbol(file)),
                        true)))
        end
    end
end

eval(overdub_transform_call_definition(:Unused, @__LINE__, @__FILE__))
