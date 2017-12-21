#########
# Phase #
#########

abstract type Phase end

struct Execute <: Phase end

struct Intercept <: Phase end

#########
# World #
#########

struct World{w} end

World() = World{get_world_age()}()

get_world_age() = ccall(:jl_get_tls_world_age, UInt, ()) # ccall(:jl_get_world_counter, UInt, ())

############
# Settings #
############

struct Settings{C<:Context,M,w,d}
    context::C
    metadata::M
    world::World{w}
    debug::Val{d}
    function Settings(context::C,
                      metadata::M = nothing,
                      world::World{w} = World(),
                      debug::Val{d} = Val(false)) where {C,M,w,d}
        return new{C,M,w,d}(context, metadata, world, debug)
    end
end

#####################
# Execution Methods #
#####################

@inline _hook(::World{w}, args...) where {w} = nothing
@inline hook(settings::Settings{C,M,w}, f, args...) where {C,M,w} = _hook(settings.world, settings.context, settings.metadata, f, args...)

@inline _execution(::World{w}, ctx, meta, f, args...) where {w} = mapcall(x -> unwrap(ctx, x), f, args...)
@inline execution(settings::Settings{C,M,w}, f, args...) where {C,M,w} = _execution(settings.world, settings.context, settings.metadata, f, args...)

@inline _isprimitive(::World{w}, args...) where {w} = Val(false)
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

@inline overdub(::Type{C}, f, metadata = nothing) where {C<:Context} = overdub(C(f), f, metadata)
@inline overdub(ctx::Context, f, metadata = nothing) = Overdub(Execute(), f, Settings(ctx, metadata))

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

##################
# default passes #
##################

# Replace all calls with `Overdub{Execute}` calls.
# Note that this approach emits code in which LHS SSAValues are not
# monotonically increasing. This currently isn't a problem, but in
# the future, valid IR might require monotonically increasing LHS
# SSAValues, in which case we'll have to add an extra SSA-remapping
# pass to this function.
function overdub_calls!(method_body::CodeInfo)
    greatest_ssa_value = method_body.ssavaluetypes
    self = SSAValue(greatest_ssa_value)
    new_code = Any[nothing, :($self = $(GlobalRef(Cassette, :func))($(SlotNumber(1))))]
    label_map = Dict{Int,Int}()
    # Replace calls with overdubbed calls, and replace
    # SlotNumber(1) references with the underlying function.
    # Also, fix LabelNodes and record the changes in a map that
    # we'll use in a future pass to
    for i in 2:length(method_body.code)
        stmnt = method_body.code[i]
        replace_match!(s -> self, s -> isa(s, SlotNumber) && s.id == 1, stmnt)
        replace_match!(is_call, stmnt) do call
            greatest_ssa_value += 1
            new_ssa_value = SSAValue(greatest_ssa_value)
            new_ssa_stmnt = Expr(:(=), new_ssa_value, Expr(:call, GlobalRef(Cassette, :intercept), SlotNumber(1), call.args[1]))
            push!(new_code, new_ssa_stmnt)
            call.args[1] = new_ssa_value
            return call
        end
        push!(new_code, stmnt)
        if isa(stmnt, LabelNode) && stmnt.label != length(new_code)
            new_code[end] = LabelNode(length(new_code))
            label_map[stmnt.label] = length(new_code)
        end
    end
    # label positions might be messed up now due to
    # the added SSAValues, so we have to fix them using
    # the label map we built up during the earlier pass
    for i in 2:length(new_code)
        stmnt = new_code[i]
        if isa(stmnt, GotoNode)
            new_code[i] = GotoNode(get(label_map, stmnt.label, stmnt.label))
        elseif isa(stmnt, Expr) && stmnt.head == :gotoifnot
            stmnt.args[2] = get(label_map, stmnt.args[2], stmnt.args[2])
        end
    end
    method_body.code = new_code
    method_body.ssavaluetypes = greatest_ssa_value + 1
    return method_body
end

# replace all `new` expressions with calls to `Cassette.wrapper_new`
function overdub_new!(method_body::CodeInfo)
    replace_match!(x -> isa(x, Expr) && x.head === :new, method_body) do x
        ctx = Expr(:call, GlobalRef(Cassette, :context), SlotNumber(1))
        return Expr(:call, GlobalRef(Cassette, :wrapper_new), ctx, x.args...)
    end
    return method_body
end

############################
# Overdub Call Definitions #
############################

# Overdub{Execute} #
#------------------#

@inline (o::Overdub{Execute})(args...) = (hook(o, args...); execute(o, args...))

# Overdub{Intercept} #
#--------------------#

for N in 0:MAX_ARGS
    arg_names = [Symbol("_CASSETTE_$i") for i in 2:(N+1)]
    arg_types = [:(unwrap(C, $T)) for T in arg_names]
    @eval begin
        @generated function (f::Overdub{Intercept,F,Settings{C,M,world,debug}})($(arg_names...)) where {F,C,M,world,debug}
            signature = Tuple{unwrap(C, F),$(arg_types...)}
            method_body = lookup_method_body(signature, $arg_names, world, debug)
            if isa(method_body, CodeInfo)
                method_body = overdub_new!(overdub_calls!(getpass(C, M)(signature, method_body)))
                method_body.inlineable = true
            else
                arg_names = $arg_names
                method_body = quote
                    $(Expr(:meta, :inline))
                    $Cassette.execute(Val(true), f, $(arg_names...))
                end
            end
            debug && Core.println("RETURNING Overdub(...) BODY: ", method_body)
            return method_body
        end
    end
end
