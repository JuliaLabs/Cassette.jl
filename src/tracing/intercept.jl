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
# IsPrimitive #
###############

struct IsPrimitive{C<:AbstractContext}
    context::C
end

@inline (::IsPrimitive)(input...) = Val(false)

@inline is_default_primitive(::Type{F}) where {F} = (F.name.module == Core) || (F <: Core.Builtin)

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

@inline execute(isprimitive::Val{true}, ::Val, ctx::AbstractContext, input...) = ctx(input...)

@inline execute(isprimitive::Val{false}, debug::Val, ctx::AbstractContext, input...) = Intercept(ctx, debug)(input...)

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
            isprimitive = IsPrimitive(i.context)(input...)
            execute(isprimitive, Val($d), i.context, input...)
        end
    end
end

# Using the below instead of the above helps avoid JuliaLang/julia#5402; hopefully, that issue will be fixed before Julia 1.0 releases

#=
function generate_intercepted_body(::Type{Intercepted{C,d,p}}, f_name, arg_names) where {C<:AbstractContext,d,p}
    F = unwrap(C)
    if p || is_default_primitive(F)
        return quote
            $(Expr(:meta, :inline))
            execute(Val(true), Val($d), $(f_name).context, $(arg_names...))
        end
    else
        return quote
            $(Expr(:meta, :inline))
            isprimitive = IsPrimitive($(f_name).context)($(arg_names...))
            execute(isprimitive, Val($d), $(f_name).context, $(arg_names...))
        end
    end
end

for N in 0:MAX_ARGS
    arg_names = [Symbol("x_$i") for i in 1:N]
    @eval begin
        @inline execute(isprimitive::Val{true}, ::Val, ctx::AbstractContext, $(arg_names...)) = ctx($(arg_names...))

        @inline execute(isprimitive::Val{false}, debug::Val, ctx::AbstractContext, $(arg_names...)) = Intercept(ctx, debug)($(arg_names...))

        @generated function (i::Intercepted{C,d,p})($(arg_names...)) where {C<:AbstractContext,d,p}
            return generate_intercepted_body(i, :i, $arg_names)
        end
    end
end
=#
