##############
# ValueGenre #
##############

struct ValueGenre <: AbstractGenre end

node_eltype(::ValueGenre, value) = RealNode{ValueGenre,eltype(value),Void}
node_cache(::ValueGenre, value) = nothing

Base.similar(::Type{ValueGenre}) = ValueGenre()
Base.similar(::ValueGenre) = ValueGenre()

# If the output is a `Real` or `AbstractArray`, track `output` before returning it
@inline (hook::ForwardHook{ValueGenre})(output::Union{Real,AbstractArray}, input...) = track(output, hook.genre, FunctionNode(hook.func, input))#

# If the output is any other type (e.g. `Bool`), don't track it (since we don't have an appropriate node type)
@inline (hook::ForwardHook{ValueGenre})(value, input...) = value
