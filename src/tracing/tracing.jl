#########
# Trace #
#########

struct Trace{C<:AbstractContext,debug}
    context::C
    flag::Val{debug}
end

Trace(context) = Trace(context, Val(false))

function intercept_calls!(code_info, f_name::Symbol, arg_names::Vector, debug::Bool = false)
    if isa(code_info, CodeInfo)
        replace_calls!(code_info) do call
            if !(isa(call, SlotNumber) && call.id == 1)
                return :($(Cassette.Intercepted)($(SlotNumber(0)), $call))
            else
                return call
            end
        end
        replace_slotnumbers!(code_info) do sn
            if sn.id == 1
                return :($(Cassette.unbox)($sn))
            elseif sn.id == 0
                return SlotNumber(1)
            else
                return sn
            end
        end
        debug && println("AFTER CALL INTERCEPTION: ", code_info)
        return code_info
    else
        expr = quote
            $(Expr(:meta, :inline))
            $(Cassette.Intercepted)($f_name)($(arg_names...))
        end
        debug && println("AFTER CALL INTERCEPTION: ", expr)
        return expr
    end
end

for N in 1:MAX_ARGS
    args = [Symbol("_CASSETTE_$i") for i in 2:(N+1)]
    expr = quote
        @generated function (t::Trace{C,debug})($(args...)) where {C<:AbstractContext,debug}
            arg_types = map(T -> unbox(C, T), ($(args...),))
            signature = Tuple{unbox(C),arg_types...}
            code_info = lookup_code_info(signature, $args, debug)
            body = intercept_calls!(code_info, :t, $args, debug)
            return body
        end
    end
    @eval $expr
end

###############
# IsPrimitive #
###############

struct IsPrimitive{C<:AbstractContext}
    context::C
end

@inline (::IsPrimitive{<:AbstractContext})(input...) = Val(false)

@inline is_default_primitive(::Type{F}) where {F} = (F.name.module == Core) || (F <: Core.Builtin)

###############
# Intercepted #
###############

struct Intercepted{C<:AbstractContext,force_primitive}
    context::C
    flag::Val{force_primitive}
end

@inline Intercepted(t::Trace, f) = Intercepted(box(t.context, f), Val(false))

@inline Intercepted(t::Trace) = Intercepted(t.context, Val(true))

@inline execute(isprimitive::Val{true},  ctx::AbstractContext, input...) = ctx(input...)

@inline execute(isprimitive::Val{false}, ctx::AbstractContext, input...) = Trace(ctx)(input...)

@generated function (i::Intercepted{C,force_primitive})(input...) where {C<:AbstractContext,force_primitive}
    F = unbox(C)
    if force_primitive || is_default_primitive(F)
        return quote
            $(Expr(:meta, :inline))
            return execute(Val(true), i.context, input...)
        end
    else
        return quote
            $(Expr(:meta, :inline))
            execute(IsPrimitive(i.context)(input...), i.context, input...)
        end
    end
end
