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

struct Process{F} <: Function
    func::F
end

@inline Process(p::Process) = p

@inline func(p::Process) = p.func

@inline function (p::Process)(input...)
    G = typeof(promote_genre(input...))
    results = Play{G}(func(p))(input...)
    return process_record(Record{G}(func(p)), input, results)
end

@inline process_record(r::Record, input::Tuple, results::Tuple{O,C}) where {O,C<:Cache} = _process_record(r, input, results[1]::O, results[2]::C)
@inline process_record(r::Record, input::Tuple, results) = _process_record(r, input, results)

@inline _process_record(r::Record, input::Tuple, output::NTuple{N,Any}, cache::Cache...) where {N} = NTuple{N}(_process_record(r, input, o, cache...) for o in output)
@inline _process_record(r::Record, input::Tuple, output, cache::Cache...) = _process_record(trackability(output), r, input, output, cache...)
@inline _process_record(::TrackabilityTrait, r::Record, input::Tuple, output, cache::Cache) = r(output, input, cache[])
@inline _process_record(::TrackabilityTrait, r::Record, input::Tuple, output) = r(output, input)
@inline _process_record(::NotTrackable, r::Record, input::Tuple, output, cache::Cache...) = output

#############
# Intercept #
#############

struct Intercept{f,w} <: Function
    Intercept{f}() where {f} = new{f,ccall(:jl_get_world_counter, UInt, ())}()
end

intercept_ast!(code_info::CodeInfo) = intercept_ast!(deepcopy(code_info.code[2]), code_info.slotnames)

intercept_ast!(ast, slotnames) = ast, slotnames

function intercept_ast!(ast::Expr, slotnames)
    if ast.head == :call
        ast.args[1] = Expr(:call, Cassette.Process, ast.args[1])
        child_indices = 2:length(ast.args)
    else
        child_indices = 1:length(ast.args)
    end
    for i in child_indices
        child = ast.args[i]
        if isa(child, SlotNumber)
            ast.args[i] = slotnames[child.id]
        else
            intercept_ast!(child, slotnames)
        end
    end
    return ast, slotnames
end

@generated function (i::Intercept{f})(args...) where f
    ast, slotnames = intercept_ast!(first(code_lowered(f, args)))
    arg_assigment = Expr(:(=), Expr(:tuple, slotnames[2:end]...), :args)
    return quote
        $(Expr(:meta, :inline))
        $(arg_assigment)
        $ast
    end
end
