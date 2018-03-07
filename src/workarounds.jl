# This file attempts to work around JuliaLang/julia#5402.
# ref https://github.com/jrevels/Cassette.jl/issues/5#issuecomment-341294691

for nargs in 1:MAX_ARGS
    args = [Symbol("x$i") for i in 1:nargs]
    @eval begin
        # overdub/execution.jl workarounds
        @inline prehook(cfg::TraceConfig{C,M,w}, f, $(args...)) where {C,M,w} = invoke(prehook, Tuple{TraceConfig{C,M,w},Any,Vararg{Any}}, cfg, f, $(args...))
        @inline posthook(cfg::TraceConfig{C,M,w}, f, $(args...)) where {C,M,w} = invoke(posthook, Tuple{TraceConfig{C,M,w},Any,Vararg{Any}}, cfg, f, $(args...))
        @inline execution(cfg::TraceConfig{C,M,w}, f, $(args...)) where {C,M,w} = invoke(execution, Tuple{TraceConfig{C,M,w},Any,Vararg{Any}}, cfg, f, $(args...))
        @inline is_user_primitive(cfg::TraceConfig{C,M,w}, f, $(args...)) where {C,M,w} = invoke(is_user_primitive, Tuple{TraceConfig{C,M,w},Any,Vararg{Any}}, cfg, f, $(args...))
        @inline is_core_primitive(cfg::TraceConfig{C,M,w}, f, $(args...)) where {C,M,w} = invoke(is_core_primitive, Tuple{TraceConfig{C,M,w},Any,Vararg{Any}}, cfg, f, $(args...))
        @inline execute(o::Overdub, $(args...)) = invoke(execute, Tuple{Overdub,Vararg{Any}}, o, $(args...))

        # TODO: use invoke here as well; see https://github.com/jrevels/Cassette.jl/issues/5#issuecomment-341525276
        @inline function (o::Overdub{Execute})($(args...))
            prehook(o.config, o.func, $(args...))
            output = execute(o, $(args...))
            posthook(o.config, o.func, output, $(args...))
            return output
        end

        # contextual/metadata.jl workarounds
        @inline mapcall(g, f, $(args...)) = invoke(mapcall, Tuple{Any,Any,Vararg{Any}}, g, f, $(args...))
        @inline _newbox(ctx::C, t::Type{T}, $(args...)) where {C<:Context,T} = invoke(_newbox, Tuple{C,Type{T},Vararg{Any}}, ctx, t, $(args...))
        @inline _new(t::Type{T}, $(args...)) where {T} = invoke(_new, Tuple{Type{T},Vararg{Any}}, t, $(args...))
    end
end
