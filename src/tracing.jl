####################
# ProcessPrimitive #
####################

# ProcessPrimitive #
#------------------#

struct ProcessPrimitive{G<:AbstractGenre,F} <: Directive{G,F}
    func::F
    @inline ProcessPrimitive{G}(func::F) where {G,F} = new{G,F}(func)
end

@inline function (p::ProcessPrimitive{G,F})(input...) where {G,F}
    results = Play{G}(func(p))(input...)
    return perform_record(Record{G}(func(p)), input, results)
end

@inline func(p::ProcessPrimitive) = p.func

# Cache #
#-------#

struct Cache{T}
    data::T
end

@inline Base.getindex(cache::Cache) = cache.data

# perform_record #
#----------------#

@inline perform_record(r::Record, input::Tuple, results::Tuple{O,C}) where {O,C<:Cache} = _perform_record(r, input, results[1]::O, results[2]::C)
@inline perform_record(r::Record, input::Tuple, results) = _perform_record(r, input, results)

@generated function _perform_record(r::Record, input::Tuple, output::NTuple{N,Any}, cache::Cache...) where {N}
    result = Expr(:tuple, [:(__perform_record(r, input, output[$i], cache...)) for i in 1:N]...)
    return quote
        $(Expr(:meta, :inline))
        $result
    end
end

@inline _perform_record(r::Record, input::Tuple, output, cache::Cache...) = __perform_record(r, input, output, cache...)

@inline __perform_record(r::Record, input::Tuple, output, cache::Cache) = r(output, input, cache[])
@inline __perform_record(r::Record, input::Tuple, output) = r(output, input)

#############
# Intercept #
#############

struct Intercept{G<:AbstractGenre,F,w} <: Directive{G,F}
    func::F
    @inline Intercept{G,F,w}(func::F) where {G,F,w} = new{G,F,w}(func)
    @inline Intercept{G}(func::F) where {G,F} = Intercept{G,F,trace_world_counter()}(func)
end

@generated function (i::Intercept{G,F})(input...) where {G,F}
    typed_input = Expr(:tuple)
    for i in 1:nfields(input)
        if input[i] <: Type && length(input[i].parameters) == 1
            type_arg = TypeArg{input[i].parameters[1]}()
            push!(typed_input.args, type_arg)
        else
            push!(typed_input.args, :(input[$i]::$(input[i])))
        end
    end
    return quote
        $(Expr(:meta, :inline))
        typed_input = $typed_input
        return perform_intercept(InterceptAs{G}(func(i))(typed_input...), i, typed_input...)
    end
end

@inline func(i::Intercept) = i.func

# InterceptAs Trait #
#-------------------#

struct Primitive end
struct NotPrimitive end

struct InterceptAs{G<:AbstractGenre,F} <: Directive{G,F}
    func::F
    @inline InterceptAs{G}(func::F) where {G,F} = new{G,F}(func)
end

@inline (i::InterceptAs)(args...) = Primitive()

@inline func(i::InterceptAs) = i.func

# perform_intercept #
#-------------------#

@inline perform_intercept(::Primitive, i::Intercept{G}, args...) where {G} = ProcessPrimitive{G}(func(i))(args...)

@inline perform_intercept(::NotPrimitive, i::Intercept{G,F,w}, args...) where {G,F,w} = Trace{G,F,w}(func(i))(args...)

#########
# Trace #
#########

# tracing utilities #
#-------------------#

#=
Historically, `code_lowered(f, types)` requires `f` to be the function instance. That
interface is just a holdover from the days where `typeof(f) === Function`; nowadays, the
function type + argument type signature is a unique identifier of a method. Thus, we can do
the following, which can be called (kind of unsafely) from a generated function.
=#

methods_by_type_sig(::Type{T}) where {T<:Tuple} = Base._methods_by_ftype(T, -1, typemax(UInt))

code_info_from_method_info(method_info) = (Base.uncompressed_ast(method_info[3]), method_info[2])

function wrap_subcalls_with_intercept!(lines::Vector, ::Type{G}, w) where {G<:AbstractGenre}
    for line in lines
        isa(line, Expr) && wrap_subcalls_with_intercept!(line, G, w)
    end
    return lines
end

function wrap_subcalls_with_intercept!(ast::Expr, ::Type{G}, w) where {G<:AbstractGenre}
    if ast.head == :call && (ast.args[1] != GlobalRef(Core, :apply_type))
        f = ast.args[1]
        ast.args[1] = Expr(:call, Expr(:call, GlobalRef(Core, :apply_type), Cassette.Intercept, G, :(typeof($f)), w), f)
        child_indices = 2:length(ast.args)
    else
        child_indices = 1:length(ast.args)
    end
    for i in child_indices
        child = ast.args[i]
        if isa(child, Expr)
            wrap_subcalls_with_intercept!(child, G, w)
        end
    end
    return ast
end

function replace_static_parameters!(lines::Vector, static_params)
    for i in eachindex(lines)
        line = lines[i]
        if isa(line, Expr)
            if line.head == :static_parameter
                lines[i] = static_params[line.args[1]]
            else
                replace_static_parameters!(line.args, static_params)
            end
        end
    end
end

function intercepted_code_info!(code_info::CodeInfo, static_params, ::Type{G}, w) where {G<:AbstractGenre}
    wrap_subcalls_with_intercept!(code_info.code, G, w)
    replace_static_parameters!(code_info.code, static_params)
    return code_info
end

trace_world_counter() = ccall(:jl_get_world_counter, UInt, ())

# Trace Type #
#------------#

struct Trace{G<:AbstractGenre,F,w} <: Directive{G,F}
    func::F
    @inline Trace{G,F,w}(func::F) where {G,F,w} = new{G,F,w}(func)
    @inline Trace{G}(func::F) where {G,F} = Trace{G,F,trace_world_counter()}(func)
end

@inline func(t::Trace) = t.func

# TODO: Just make a single n-ary version of `Trace(...)`. The difficulty here comes from
# matching the generator's splatted tuple argument with the "pre-splatted" arguments
# represented as numbered slots in the generated CodeInfo.

function _generated_trace_body(::Type{Trace{G,F,w}}, varsyms, argtypes...) where {G,F,w}
    method_list = methods_by_type_sig(Tuple{F,value.(argtypes)...})
    if isempty(method_list)
        # assumes that `t` and `G` are static parameters
        # of the generator who called this function!!!
        return quote
            $(Expr(:meta, :inline))
            ProcessPrimitive{G}(func(t))($(varsyms...))
        end
    else
        code_info, static_params = code_info_from_method_info(method_list[])
        return intercepted_code_info!(code_info, static_params, G, w)
    end
end

for N in 1:15
    vars = [Symbol("x_$i") for i in 1:N]
    quoted_vars = Expr(:tuple, [Expr(:quote, v) for v in vars]...)
    @eval begin
        @generated function (t::Trace{G,F,w})($(vars...)) where {G,F,w}
            return _generated_trace_body(Trace{G,F,w}, $(quoted_vars), $(vars...))
        end
    end
end
