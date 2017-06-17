#########
# Cache #
#########

struct Cache{T}
    data::T
end

@inline Base.getindex(cache::Cache) = cache.data

#############
# Intercept #
#############

struct Intercept{F} <: Function
    func::F
end

@inline Intercept(i::Intercept) = i

@inline func(i::Intercept) = i.func

@inline function (i::Intercept)(input...)
    G = typeof(promote_genre(input...))
    results = Play{G}(func(i))(input...)
    return process_record(Record{G}(func(i)), input, results)
end

@inline process_record(r::Record, input::Tuple, results::Tuple{O,C}) where {O,C<:Cache} = _process_record(r, input, results[1]::O, results[2]::C)
@inline process_record(r::Record, input::Tuple, results) = _process_record(r, input, results)

@inline _process_record(r::Record, input::Tuple, output::NTuple{N,Any}, cache::Cache...) where {N} = NTuple{N}(_process_record(r, input, o, cache...) for o in output)
@inline _process_record(r::Record, input::Tuple, output, cache::Cache...) = _process_record(trackability(output), r, input, output, cache...)
@inline _process_record(::TrackabilityTrait, r::Record, input::Tuple, output, cache::Cache) = r(output, input, cache[])
@inline _process_record(::TrackabilityTrait, r::Record, input::Tuple, output) = r(output, input)
@inline _process_record(::NotTrackable, r::Record, input::Tuple, output, cache::Cache...) = output

##############
# @intercept #
##############

const AMBIGUOUS_INTERCEPT_TYPES = vcat(REAL_TYPES, ARRAY_TYPES)

macro intercept(def)
    @assert isa(def, Expr) "malformed expression for @intercept"
    @assert def.head == :call
    func = def.args[1]
    args = def.args[2:end]
    @assert all(isa(x, Symbol) for x in args) "argument names must be Symbols (not Expressions)"
    @assert (isa(func, Expr) && (func.head == :.) &&
             length(func.args) == 2) "function name must be fully qualified (e.g. `@intercept Base.sin(x)`, not `@intercept sin(x)`)"
    methods = Expr(:block)
    T = :($ValueNote)
    typed_args_list = dispatch_permutations(args, T, AMBIGUOUS_INTERCEPT_TYPES)
    for typed_args in typed_args_list
        push!(methods.args, :(@inline $(func)($(typed_args...)) = $(Intercept)($(func))($(args...))))
    end
    return esc(methods)
end

function dispatch_permutations(args, T, nonTs)
    arg_perms = Vector{Any}()
    # Count upwards in binary, essentially using each number as a bitmask to determine
    # which arguments should be `::T` for that permutation.
    for i in 1:((2^length(args)) - 1)
        proto = Any[]
        nonT_inds = Int[]
        for j in 1:length(args)
            if Bool((i >> (j - 1)) & 1)
                push!(proto, :($(args[j])::$(T)))
            else
                push!(proto, args[j])
                push!(nonT_inds, j)
            end
        end
        if isempty(nonT_inds)
            push!(arg_perms, proto)
        else
            nonTs_iter = Iterators.product((nonTs for _ in 1:length(nonT_inds))...)
            for current_nonTs in nonTs_iter
                arg_perm = copy(proto)
                for k in 1:length(current_nonTs)
                    arg_ind = nonT_inds[k]
                    arg_perm[arg_ind] = :($(proto[arg_ind])::$(current_nonTs[k]))
                end
                push!(arg_perms, arg_perm)
            end
        end
    end
    return arg_perms
end
