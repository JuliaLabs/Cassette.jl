###############
# Intercepted #
###############

struct Intercepted{C<:AbstractContext,force_primitive}
    context::C
    flag::Val{force_primitive}
end

@inline is_default_primitive(::Type{F}) where {F} = (F.name.module == Core) || (F <: Core.Builtin)

@generated function (i::Intercepted{C,force_primitive})(input...) where {F0,F,C<:AbstractContext{F0,F},force_primitive}
    if force_primitive || is_default_primitive(F)
        return quote
            $(Expr(:meta, :inline))
            return execute_intercepted(Val{true}(), i.context, input...)
        end
    else
        return quote
            $(Expr(:meta, :inline))
            c = unbox(i)
            isprimitive = IsPrimitive(i.context)(input...)
            execute_intercepted(isprimitive, i.context, input...)
        end
    end
end

struct IsPrimitive{C<:AbstractContext}
    context::C
end

@inline (::IsPrimitive{<:AbstractContext})(input...) = Val{false}()

@inline unbox(i::IsPrimitive) = i.context

@inline execute_intercepted(isprimitive::Val{true},  c::AbstractContext, input...) = c(input...)
@inline execute_intercepted(isprimitive::Val{false}, c::AbstractContext, input...) = Trace(c)(input...)

#########
# Trace #
#########

struct Trace{C<:AbstractContext,debug}
    context::C
    flag::Val{debug}
end

Trace(context) = Trace(context, Val{false}())

@inline intercept(t::Trace, f) = box(t.context, f) #Intercepted(box(t.context, f), Val{false}())

@inline intercept(t::Trace) = t.context #Intercepted(t.context, Val{true}())

function intercept_calls!(code_info, f_name::Symbol, arg_names::Vector, debug::Bool = false)
    if isa(code_info, CodeInfo)
        replace_calls!(code_info) do call
            if !(isa(call, SlotNumber) && call.id == 1)
                return :($(Cassette.intercept)($(SlotNumber(0)), $call))
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
            $(Cassette.intercept)($f_name)($(arg_names...))
        end
        debug && println("AFTER CALL INTERCEPTION: ", expr)
        return expr
    end
end

for N in 1:MAX_ARGS
    args = [Symbol("_CASSETTE_$i") for i in 2:(N+1)]
    expr = quote
        @generated function (t::Trace{C,debug})($(args...)) where {F0,F,C<:AbstractContext{F0,F},debug}
            arg_types = map(unbox, ($(args...),))
            signature = Tuple{F,arg_types...}
            code_info = lookup_code_info(signature, $args, debug)
            body = intercept_calls!(code_info, :t, $args, debug)
            return body
        end
    end
    @eval $expr
end
