#########
# Phase #
#########

abstract type Phase end

struct Execute <: Phase end

struct Intercept <: Phase end

############
# Settings #
############

get_world_age() = ccall(:jl_get_tls_world_age, UInt, ()) # ccall(:jl_get_world_counter, UInt, ())

struct Settings{C<:Context,M,w,d,P}
    context::C
    metadata::M
    world::Val{w}
    debug::Val{d}
    pass::P
end

function Settings(context::Context;
                  metadata = Unused(),
                  world::Val = Val(get_world_age()),
                  debug::Val = Val(false),
                  pass = Unused())
    return Settings(context, metadata, world, debug, pass)
end

#####################
# Execution Methods #
#####################

@inline _hook(::Val{w}, args...) where {w} = nothing
@inline hook(settings::Settings{C,M,w}, f, args...) where {C,M,w} = _hook(settings.world, settings.context, settings.metadata, f, args...)

@inline _execution(::Val{w}, ctx, meta, f, args...) where {w} = mapcall(x -> unbox(ctx, x), f, args...)
@inline execution(settings::Settings{C,M,w}, f, args...) where {C,M,w} = _execution(settings.world, settings.context, settings.metadata, f, args...)

@inline _isprimitive(::Val{w}, args...) where {w} = Val(false)
@inline isprimitive(settings::Settings{C,M,w}, f, args...) where {C,M,w} = _isprimitive(settings.world, settings.context, settings.metadata, f, args...)

###########
# Overdub #
###########

struct Overdub{P<:Phase,F,S<:Settings}
    phase::P
    func::F
    settings::S
    function Overdub(phase::P, func, settings::S) where {P,S}
        F = Core.Typeof(func) # this yields `Type{T}` instead of `UnionAll` for constructors
        return new{P,F,S}(phase, func, settings)
    end
end

@inline overdub(::Type{C}, f; kwargs...) where {C<:Context} = overdub(C(f), f; kwargs...)
@inline overdub(ctx::Context, f; kwargs...) = Overdub(Execute(), f, Settings(ctx; kwargs...))

@inline intercept(o::Overdub{Intercept}, f) = Overdub(Execute(), f, o.settings)

@inline context(o::Overdub) = o.settings.context

@inline hook(o::Overdub, args...) = hook(o.settings, o.func, args...)

@inline isprimitive(o::Overdub, args...) = isprimitive(o.settings, o.func, args...)

@inline execute(o::Overdub, args...) = execute(isprimitive(o, args...), o, args...)
@inline execute(::Val{true}, o::Overdub, args...) = execution(o.settings, o.func, args...)
@inline execute(::Val{false}, o::Overdub, args...) = Overdub(Intercept(), o.func, o.settings)(args...)

@inline func(o::Overdub) = o.func
@inline func(f) = f

Base.show(io::IO, o::Overdub{P}) where {P} = print("Overdub{$(P.name.name)}($(typeof(context(o)).name), $(func(o)))")

####################
# Overdub{Execute} #
####################

@inline (o::Overdub{Execute})(args...) = (hook(o, args...); execute(o, args...))

######################
# Overdub{Intercept} #
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
                    new_ssa_stmnt = Expr(:(=), new_ssa_value, Expr(:call, GlobalRef(Cassette, :intercept), SlotNumber(1), call.args[1]))
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

function _overdub_generator(::Type{F}, ::Type{C}, ::Type{M}, world, debug, pass, f, args) where {F,C,M}
    ftype = unbox(C, F)
    atypes = Tuple(unbox(C, T) for T in args)
    signature = Tuple{ftype,atypes...}
    try
        method_body = lookup_method_body(signature, world, debug)
        if isa(method_body, CodeInfo)
            if !(pass <: Unused)
                method_body = pass(signature, method_body)
            end
            method_body = overdub_pass!(method_body)
            method_body.inlineable = true
            method_body.signature_for_inference_heuristics = Core.svec(ftype, atypes, world)
            debug && Core.println("RETURNING OVERDUBBED CODEINFO: ", sprint(show, method_body))
        else
            method_body = quote
                $(Expr(:meta, :inline))
                $Cassette.execute(Val(true), f, $(OVERDUB_ARGS_SYMBOL)...)
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

@eval function (f::Overdub{Intercept,F,Settings{C,M,world,debug,pass}})($(OVERDUB_ARGS_SYMBOL)...) where {F,C,M,world,debug,pass}
    $(Expr(:meta,
           :generated,
           Expr(:new,
                Core.GeneratedFunctionStub,
                :_overdub_generator,
                Any[:f, OVERDUB_ARGS_SYMBOL],
                Any[:F, :C, :M, :world, :debug, :pass],
                @__LINE__,
                QuoteNode(Symbol(@__FILE__)),
                true)))
end
