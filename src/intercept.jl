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

intercept_ast!(genre, code_info::CodeInfo) = intercept_ast!(genre, deepcopy(Expr(:block, code_info.code...)), code_info.slotnames, Symbol[])

function intercept_ast!(genre, ast::Expr, slotnames, ssanames)
    if ast.head == :call
        ast.args[1] = Expr(:call, :($(Cassette.Process){$(typeof(genre))}), ast.args[1])
        child_indices = 2:length(ast.args)
    else
        child_indices = 1:length(ast.args)
    end
    for i in child_indices
        child = ast.args[i]
        if isa(child, SlotNumber)
            ast.args[i] = slotnames[child.id]
        elseif isa(child, SSAValue)
            ssa_index = child.id + 1
            if length(ssanames) >= ssa_index
                ast.args[i] = ssanames[ssa_index]
            else
                name = gensym(ssa_index)
                push!(ssanames, name)
                ast.args[i] = name
            end
        elseif isa(child, Expr)
            intercept_ast!(genre, child, slotnames, ssanames)
        end
    end
    return ast, slotnames
end

# TODO: check world age against i's world-age, if changed, then create a
# new Intercept with the newer world age and call that one.
@generated function (i::Intercept{G,f})(args...) where {G,f}
    ast, slotnames = intercept_ast!(G(), first(code_lowered(f, args)))
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
