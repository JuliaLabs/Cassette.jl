###############
# Intercepted #
###############

struct Intercepted{C<:AbstractContext}
    callable::C
end

@inline unbox(i::Intercepted) = i.callable

@inline perform(action::Val{:trace},   f, input...) = trace(f, input...)
@inline perform(action::Val{:process}, f, input...) = f(input...)
@inline perform(action::Val{:skip},    f, input...) = unboxcall(f, input...)

@generated function (i::Intercepted)(input...)
    boxed_input = [:(box(c, input[$i]::$(input[i]))) for i in 1:nfields(input)]
    return quote
        $(Expr(:meta, :inline))
        c = unbox(i)
        perform(action(i, input...), c, $(boxed_input...))
    end
end

struct InterceptAction{C<:AbstractContext} end

(::InterceptAction)(input...) = Val{:skip}()

@generated function action(i::Intercepted{C}, input...) where {F,C<:AbstractContext{F}}
    if F.name.module === Core
        return :($(Expr(:meta, :inline)); Val{:skip}())
    else
        return :($(Expr(:meta, :inline)); InterceptAction{C}()(input...))
    end
end

#########
# trace #
#########
# TODO: Make this 265-safe. This can be done by improving Julia's @generated function
# capabilities; we can pass the world age as a "hidden" argument and require the
# generator to return the world bounds in which it is safe.

struct Trace{C<:AbstractContext}
    callable::C
end

@inline unbox(t::Trace) = t.callable

@inline intercept(t::Trace, f) = Intercepted(box(unbox(t), f))

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
