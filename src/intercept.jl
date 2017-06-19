#########
# Cache #
#########

struct Cache{T}
    data::T
end

@inline Base.getindex(cache::Cache) = cache.data

###########
# Primitive #
###########

struct Primitive{G<:AbstractGenre,F} <: Function
    func::SpecializedFunction{F}
    @inline Primitive{G}(func::F) where {G,F} = new{G,F}(SpecializedFunction(func))
    @inline Primitive{G}(p::Primitive) where {G} = p
end

@inline func(p::Primitive) = p.func

@inline genre(p::Primitive) = genre(typeof(p))
@inline genre(::Type{Primitive{G,F}}) where {G,F} = G()

@generated function (p::Primitive{G})(input...) where {G}
    typed_input = Expr(:tuple)
    for i in 1:nfields(input)
        if input[i] <: DataType
            type_arg = TypeArgument{input[i].parameters[1]}()
            push!(typed_input.args, type_arg)
        else
            push!(typed_input.args, :(input[$i]::$(input[i])))
        end
    end
    return quote
        $(Expr(:meta, :inline))
        typed_input = $typed_input
        results = Play{G}(func(p))(typed_input...)
        return process_record(Record{G}(func(p)), typed_input, results)
    end
end

@inline process_record(r::Record, input::Tuple, results::Tuple{O,C}) where {O,C<:Cache} = _process_record(r, input, results[1]::O, results[2]::C)
@inline process_record(r::Record, input::Tuple, results) = _process_record(r, input, results)

@inline _process_record(r::Record, input::Tuple, output::NTuple{N,Any}, cache::Cache...) where {N} = NTuple{N}(__process_record(r, input, o, cache...) for o in output)
@inline _process_record(r::Record, input::Tuple, output, cache::Cache...) = __process_record(r, input, output, cache...)

@inline __process_record(r::Record, input::Tuple, output, cache::Cache) = r(output, input, cache[])
@inline __process_record(r::Record, input::Tuple, output) = r(output, input)

#############
# Intercept #
#############

struct Intercept{G<:AbstractGenre,f,w} <: Function
    Intercept{G,f}() where {G,f} = new{G,f,ccall(:jl_get_world_counter, UInt, ())}()
end

function intercept_ast!(genre, f, types)
    method = Sugar.LazyMethod((f, types))
    ast = Sugar.replace_slots(method, Sugar.sugared(f, types, code_lowered))
    slot_names = Sugar.slotnames(method)[2:Sugar.method_nargs(method)]
    intercept_ast_calls!(ast, genre)
    return ast, slot_names
end

function intercept_ast_calls!(ast::Expr, genre)
    if ast.head == :call
        ast.args[1] = Expr(:call, :($(Cassette.Primitive){$(typeof(genre))}), ast.args[1])
        child_indices = 2:length(ast.args)
    else
        child_indices = 1:length(ast.args)
    end
    for i in child_indices
        child = ast.args[i]
        if isa(child, Expr)
            intercept_ast_calls!(child, genre)
        end
    end
    return ast
end

# TODO: check world age against i's world-age, if changed, then create a
# new Intercept with the newer world age and call that one.
@generated function (::Intercept{G,f})(__args__...) where {G,f}
    ast, slotnames = intercept_ast!(G(), f, __args__)
    if length(__args__) == 0
        arg_assigment = nothing
    elseif length(__args__) == 1
        arg_assigment = Expr(:(=), slotnames[], :(__args__[1]::$(__args__[1])))
    else
        arg_assigment = Expr(:(=), Expr(:tuple, slotnames...), :__args__)
    end
    return quote
        $(Expr(:meta, :inline))
        $(arg_assigment)
        $ast
    end
end
