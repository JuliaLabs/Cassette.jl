#############
# Intercept #
#############

struct Intercept{C<:AbstractContext}
    callable::C
end

@inline unbox(i::Intercept) = i.callable

@inline perform(action::Val{:process}, c::AbstractContext, input...) = unboxcall(c, c, input...)
@inline perform(action::Val{:trace},   c::AbstractContext, input...) = unboxcall(c, Trace(c), input...)
@inline perform(action::Val{:skip},    c::AbstractContext, input...) = unboxcall(c, unbox(c), input...)

@generated function (i::Intercept)(input...)
    boxed_input = [:(box(c, input[$i]::$(input[i]))) for i in 1:nfields(input)]
    return quote
        $(Expr(:meta, :inline))
        c = unbox(i)
        perform(action(c, input...), c, $(boxed_input...))
    end
end

struct InterceptAction{C<:AbstractContext}
    callable::C
end

@inline unbox(i::InterceptAction) = i.callable

(::InterceptAction)(input...) = Val{:skip}()

@generated function action(i::AbstractContext{F}, input...) where {F}
    if F.name.module === Core
        return :($(Expr(:meta, :inline)); Val{:skip}())
    else
        return :($(Expr(:meta, :inline)); InterceptAction(c)(input...))
    end
end

#########
# trace #
#########

struct Trace{C<:AbstractContext}
    callable::C
end

@inline unbox(t::Trace) = t.callable

@inline intercept(t::Trace, f) = Intercept(box(unbox(t), f))

function trace_body(code_info, fname::Symbol, argnames::Vector{Symbol})
    if isa(code_info, CodeInfo)
        for i in 1:length(argnames)
            code_info.slotnames[i+1] = argnames[i]
        end
        replace_calls!(call -> :($(Cassette.intercept)($(SlotNumber(1)), $call)), code_info)
        return code_info
    else
        return quote
            $(Expr(:meta, :inline))
            $(Cassette.unboxcall)($(fname), $(argnames...))
        end
    end
end

for N in 1:MAX_ARGS
    args = [Symbol("_$i") for i in 2:(N+1)]
    expr = quote
        @generated function (t::Trace{C})($(args...)) where {F,C<:AbstractContext{F}}
            code_info = code_info_from_type_signature(Tuple{F,$(args...)})
            expr = trace_body(code_info, :t, $args)
            return expr
        end
    end
    @eval $expr
end
