#########
# Trace #
#########

struct Trace{C<:AbstractContext,d}
    context::C
    debug::Val{d}
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
                return :($(Cassette.unwrap)($sn))
            elseif sn.id == 0
                return SlotNumber(1)
            else
                return sn
            end
        end
        debug && println("INTERCEPTED CODEINFO: ", code_info)
        return code_info
    else
        expr = quote
            $(Expr(:meta, :inline))
            $(Cassette.Intercepted)($f_name)($(arg_names...))
        end
        debug && println("INTERCEPTED CODEINFO: ", expr)
        return expr
    end
end

for N in 1:MAX_ARGS
    args = [Symbol("_CASSETTE_$i") for i in 2:(N+1)]
    expr = quote
        @generated function (t::Trace{C,d})($(args...)) where {C<:AbstractContext,d}
            arg_types = map(T -> unwrap(C, T), ($(args...),))
            # code_info = lookup_code_info(unwrap(C).instance, arg_types, d)
            code_info = lookup_code_info(Tuple{unwrap(C),arg_types...}, $args, d)
            body = intercept_calls!(code_info, :t, $args, d)
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

struct Intercepted{C<:AbstractContext,d,p}
    context::C
    debug::Val{d}
    force_primitive::Val{p}
end

@inline Intercepted(t::Trace, f) = Intercepted(wrap(t.context, f), t.debug, Val(false))

@inline Intercepted(t::Trace) = Intercepted(t.context, t.debug, Val(true))

@inline execute(isprimitive::Val{true},  debug::Val, ctx::AbstractContext, input...) = ctx(input...)

@inline execute(isprimitive::Val{false}, debug::Val, ctx::AbstractContext, input...) = Trace(ctx, debug)(input...)

@generated function (i::Intercepted{C,d,p})(input...) where {C<:AbstractContext,d,p}
    F = unwrap(C)
    if p || is_default_primitive(F)
        return quote
            $(Expr(:meta, :inline))
            return execute(Val(true), Val($d), i.context, input...)
        end
    else
        return quote
            $(Expr(:meta, :inline))
            execute(IsPrimitive(i.context)(input...), Val($d), i.context, input...)
        end
    end
end
