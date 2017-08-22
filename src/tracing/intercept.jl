#############
# Intercept #
#############

struct Intercept{C<:AbstractContext,d}
    context::C
    debug::Val{d}
end

Intercept(context) = Intercept(context, Val(false))

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
        code_info.inlineable = true
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

for N in 0:MAX_ARGS
    arg_names = [Symbol("_CASSETTE_$i") for i in 2:(N+1)]
    @eval begin
        @generated function (i::Intercept{C,d})($(arg_names...)) where {C<:AbstractContext,d}
            arg_types = map(T -> unwrap(C, T), ($(arg_names...),))
            code_info = lookup_code_info(Tuple{unwrap(C), arg_types...}, $arg_names, d)
            body = intercept_calls!(code_info, :i, $arg_names, d)
            return body
        end
    end
end

###############
# Intercepted #
###############

struct Intercepted{C<:AbstractContext,d,p}
    context::C
    debug::Val{d}
    force_primitive::Val{p}
end

@inline Intercepted(ctx::AbstractContext) = Intercepted(ctx, Val(false), Val(false))

@inline Intercepted(i::Intercept, f) = Intercepted(wrap(i.context, f), i.debug, Val(false))

@inline Intercepted(i::Intercept) = Intercepted(i.context, i.debug, Val(true))

@inline function (i::Intercepted{C,d,p})(args...) where {C<:AbstractContext,d,p}
    Hook(i.context)(args...)
    return execute(i, args...)
end

@generated treat_as_primitive(i::Intercepted{C,d,p}, args...) where {C<:AbstractContext,d,p}
    F = unwrap(C)
    if p || (F.name.module == Core) || (F <: Core.Builtin)
        isprimitive = :(Val(true))
    else
        isprimitive = :(IsPrimitive(i.context)(args...))
    end
    return quote
        $(Expr(:meta, :inline))
        $isprimitive
    end
end

@inline execute(i::Intercepted, args...) = execute(treat_as_primitive(i), i, args...)
@inline execute(::Val{true}, i::Intercepted, args...) = i.context(args...)
@inline execute(::Val{false}, i::Intercepted, args...) = Intercept(i.context, i.debug)(args...)
