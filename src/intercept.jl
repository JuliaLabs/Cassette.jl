#########
# Cache #
#########

struct Cache{T}
    data::T
end

@inline Base.getindex(cache::Cache) = cache.data

###########
# Process #
###########

struct Process{G<:AbstractGenre,F} <: Function
    func::F
    @inline Process{G}(func::F) where {G,F} = new{G,F}(func)
    @inline Process{G}(p::Process) where {G} = p
end

@inline func(p::Process) = p.func

@inline genre(p::Process) = genre(typeof(p))
@inline genre(::Type{Process{G,F}}) where {G,F} = G()

@inline function (p::Process{G})(input...) where {G}
    results = Play{G}(func(p))(input...)
    return process_record(Record{G}(func(p)), input, results)
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
    intercept_ast_calls!(genre, ast)
    return ast, slot_names
end

function intercept_ast_calls!(ast::Expr, genre)
    if ast.head == :call
        ast.args[1] = Expr(:call, :($(Cassette.Process){$(typeof(genre))}), ast.args[1])
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
@generated function (i::Intercept{G,f})(args...) where {G,f}
    ast, slotnames = intercept_ast!(G(), f, args)
    if length(args) == 0
        arg_assigment = nothing
    elseif length(args) == 1
        arg_assigment = Expr(:(=), slotnames[2], :(args[1]::$(args[1])))
    else
        arg_assigment = Expr(:(=), Expr(:tuple, slotnames[2:end]...), :args)
    end
    return quote
        $(Expr(:meta, :inline))
        $(arg_assigment)
        $ast
    end
end
