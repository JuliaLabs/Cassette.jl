##############
# Directives #
##############

abstract type Directive{G<:AbstractGenre,F} end

macro defdirective(D)
    return esc(quote
        struct $(D){G<:$(AbstractGenre),F} <: $(Directive){G,F}
            callable::F
            @inline $(D){G}(callable::F) where {G<:$(AbstractGenre),F}  = new{G,F}(callable)
            @inline $(D){G}(d::$(Directive)) where {G<:$(AbstractGenre)}  = $(D){G}($(unwrap)(d))
        end
    end)
end

@inline unwrap(d::Directive) = d.callable

@inline genre(::Type{D}) where {G,D<:Directive{G}} = G()

@defdirective Play
@defdirective Record
@defdirective Replay
@defdirective Rewind

#######################
# Directive Utilities #
#######################

# unwrapcall #
#------------#

#=
This is basically Haskell's `Applicative` sequencing operation (`<*>`), except that...
    - ...the involved functor types aren't necessarily homogeneous
    - ...the final ouput isn't automatically rewrapped with a functor type
=#

@inline unwrapcall(f::F, a) where {F} = unwrap(f)(unwrap(a))
@inline unwrapcall(f::F, a, b) where {F} = unwrap(f)(unwrap(a), unwrap(b))
@inline unwrapcall(f::F, a, b, c) where {F} = unwrap(f)(unwrap(a), unwrap(b), unwrap(c))
@inline unwrapcall(f::F, a, b, c, d) where {F} = unwrap(f)(unwrap(a), unwrap(b), unwrap(c), unwrap(d))
@inline unwrapcall(f::F, a, b, c, d, e) where {F} = unwrap(f)(unwrap(a), unwrap(b), unwrap(c), unwrap(d), unwrap(e))
@inline unwrapcall(f::F, a, b, c, d, e, args...) where {F} = unwrap(f)(unwrap(a), unwrap(b), unwrap(c), unwrap(d), unwrap(e), unwrap.(args)...)

# TypeArg #
#---------#

#=
The only reason this exists is so that `DataType` arguments can be passed via splatted
method arguments without incurring an inference specialization penalty. If you'd like
to see an example, you can run the below code:

```
# recall that `unwrap` is a no-op by default
julia> f(args...) = one(Cassette.unwrap.(args)...);

# `args` will be inferred as `Tuple{DataType}`,
# and thus the result will be inferred as `Any`
julia> @code_warntype f(Int)

# `args` will be inferred as `Tuple{Cassette.TypeArg{Int}}`,
# and thus the result will be inferred as `Int`
julia> @code_warntype f(Cassette.TypeArg{Int}())
```
=#

struct TypeArg{T} end

@inline unwrap(::TypeArg{T}) where {T} = T
@inline unwrap(::Type{TypeArg{T}}) where {T} = T

@generated function typedcall(f::F, input...) where {F}
    typed_input = Any[]
    for i in 1:nfields(input)
        if input[i] <: Type && length(input[i].parameters) == 1
            type_arg = TypeArg{input[i].parameters[1]}()
            push!(typed_input, type_arg)
        else
            push!(typed_input, :(input[$i]::$(input[i])))
        end
    end
    return quote
        $(Expr(:meta, :inline))
        return f($(typed_input...))
    end
end
