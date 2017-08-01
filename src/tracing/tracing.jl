###############
# Intercepted #
###############

struct Intercepted{C<:AbstractContext,force_primitive}
    context::C
    flag::Val{force_primitive}
end

@generated function (i::Intercepted{C,force_primitive})(input...) where {F0,F,C<:AbstractContext{F0,F},force_primitive}
    if force_primitive || (F.name.module == Core)
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

struct Trace{C<:AbstractContext}
    context::C
end

@inline intercept_call(t::Trace, f) = Intercepted(box(t.context, f), Val{false}())

@inline intercept_primitive(t::Trace) = Intercepted(t.context, Val{true}())

function trace_body!(code_info, f_name::Symbol, arg_names::Vector)
    if isa(code_info, CodeInfo)
        replace_calls!(call -> :($(Cassette.intercept_call)($(SlotNumber(0)), $call)), code_info)
        replace_slotnumbers!(code_info) do sn
            if sn.id == 1
                return :($(Cassette.unbox)($sn))
            elseif sn.id == 0
                return SlotNumber(1)
            else
                return sn
            end
        end
        return code_info
    else
        return quote
            $(Expr(:meta, :inline))
            $(Cassette.intercept_primitive)($f_name)($(arg_names...))
        end
    end
end

for N in 1:MAX_ARGS
    args = [Symbol("_CASSETTE_$i") for i in 2:(N+1)]
    expr = quote
        @generated function (t::Trace{C})($(args...)) where {F0,F,C<:AbstractContext{F0,F}}
            arg_types = map(unbox, ($(args...),))
            signature = Tuple{F,arg_types...}
            code_info = code_info_from_type_signature(signature, $args)
            body = trace_body!(code_info, :t, $args)
            return body
        end
    end
    @eval $expr
end
