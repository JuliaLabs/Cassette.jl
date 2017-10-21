########
# Mode #
########

abstract type Mode end

struct Execute <: Mode end

struct Intercept <: Mode end

############
# Settings #
############

struct Settings{C<:Context,M,w,d}
    context::C
    metadata::M
    world::Val{w}
    debug::Val{d}
    function Settings(context::C,
                      metadata::M = nothing,
                      world::Val{w} = Val(get_world_age())
                      debug::Val{d} = Val(false)) where {C,M,w,d}
        return new{C,M,w,d}(context, metadata, world, debug)
    end
end

get_world_age() = ccall(:jl_get_tls_world_age, UInt, ()) # ccall(:jl_get_world_counter, UInt, ())

@inline hook(settings::Settings{C,M,w}, f, args...) where {C,M,w}
    return hook(settings.context, settings.meta, f, args...)
end

@inline execution(settings::Settings{C,M,w}, f, args...) where {C,M,w}
    return execution(settings.context, settings.meta, f, args...)
end

@generated function isprimitive(settings::Settings{C,M,w}, f::F, args...) where {C,M,w,F}
    if F <: Core.Builtin
        body = :(Val(true))
    else
        body = :($Cassette.isprimitive(settings.context, settings.meta, f, args...))
    end
    return quote
        $(Expr(:meta, :inline))
        $(body)
    end
end

###########
# Overdub #
###########

struct Overdub{M<:Mode,F,S<:Settings}
    mode::M
    func::F
    settings::S
    function Overdub(mode::M, func, settings::S) where {M,S}
        F = Core.Typeof(func) # this yields `Type{T}` instead of `UnionAll` for constructors
        return new{M,F,S}(func, settings)
    end
end

@inline hook(o::Overdub, args...) = hook(o.settings, o.func, args...)

@inline isprimitive(o::Overdub, args...) = isprimitive(o.settings, o.func, args...)

@inline execute(o::Overdub, args...) = execute(isprimitive(o, args...), o, args...)
@inline execute(::Val{true}, o::Overdub, args...) = execution(o.settings, o.func, args...)
@inline execute(::Val{false}, o::Overdub, args...) = Overdub(Intercept(), o.func, o.settings)(args...)

##################
# default passes #
##################

# replace all calls with `Overdub{Execute}` calls
function overdub_calls!(method_body::CodeInfo)
    replace_calls!(method_body) do call
        if !(isa(call, SlotNumber) && call.id == 1)
            return :($Cassette.Overdub($(Execute()), $(call), $(SlotNumber(0)).settings))
        end
        return call
    end
    replace_slotnumbers!(method_body) do sn
        if sn.id == 1
            return :($(sn).func)
        elseif sn.id == 0
            return SlotNumber(1)
        else
            return sn
        end
    end
    return method_body
end

# replace all `new` expressions with calls to `Cassette.wrapper_new`
function overdub_new!(method_body::CodeInfo)
    replace_match!(x -> isa(x, Expr) && x.head === :new, method_body) do x
        ctx = Expr(:call, GlobalRef(Core, :getfield), SlotNumber(1), QuoteNode(:context))
        return Expr(:call, GlobalRef(Cassette, :wrapper_new), ctx, x.args...)
    end
    return method_body
end

############################
# Overdub Call Definitions #
############################

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
                method_body = overdub_new!(overdub_calls!(pass(C, M)(signature, method_body)))
                method_body.inlineable = true
            else
                method_body = quote
                    $(Expr(:meta, :inline))
                    $Cassette.Overdub($(Execute()), $f.func, $f.settings)($(arg_names...))
                end
            end
            debug && println("RETURNING Overdub(...) BODY: ", method_body)
            return method_body
        end
    end
end

# Overdub{Execute} #
#------------------#

@inline (o::Overdub{Execute})(args...) = (hook(o, args...); execute(o, args...))
