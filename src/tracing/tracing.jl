###############
# Intercepted #
###############

struct Intercepted{C<:AbstractContext}
    callable::C
end

@inline unbox(i::Intercepted) = i.callable

@inline (i::Intercepted)(input...) = perform(action(i, input...), unbox(i), input...)

@inline perform(action::Val{:trace},   f, input...) = trace(f, input...)
@inline perform(action::Val{:process}, f, input...) = f(input...)
@inline perform(action::Val{:skip},    f, input...) = unboxcall(f, input...)

struct InterceptAction{C<:AbstractContext} end

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

@generated function trace(f::AbstractContext{F}, args...) where {F}
    boxed_args = Expr(:tuple, [:(box(f, args[$i])) for i in 1:nfields(args)])
    signature = Tuple{F,args...}
    return quote
        $(Expr(:meta, :inline))
        contextual_trace($(signature), f, $(boxed_args...))
    end
end

@generated function contextual_trace(::Type{S}, f::AbstractContext, args::AbstractContext...) where {S}
    code_info = code_info_from_type_signature(S)
    if isa(code_info, CodeInfo)
        # TODO: actually write a pass for interpolating `argnames` into `code_info`
        for i in 1:length(argnames)
            code_info.slotnames[i+1] = argnames[i]
        end
        prune_calls!(call -> :($(Cassette.Intercepted)($call)()), code_info)
        return code_info
    else
        return quote
            $(Expr(:meta, :inline))
            f(args...)
        end
    end
end
