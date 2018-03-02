# This file attempts to work around JuliaLang/julia#5402.
# ref https://github.com/jrevels/Cassette.jl/issues/5#issuecomment-341294691

for nargs in 1:MAX_ARGS
    args = [Symbol("x$i") for i in 1:nargs]
    @eval begin
        # overdub/execution.jl workarounds
        @inline _prehook(world::Val{w}, $(args...)) where {w} = invoke(_prehook, Tuple{Val{w},Vararg{Any}}, world, $(args...))
        @inline _posthook(world::Val{w}, $(args...)) where {w} = invoke(_posthook, Tuple{Val{w},Vararg{Any}}, world, $(args...))
        @inline _execution(world::Val{w}, ctx, meta, f, $(args...)) where {w} = invoke(_execution, Tuple{Val{w},Any,Any,Any,Vararg{Any}}, world, ctx, meta, f, $(args...))
        @inline _isprimitive(world::Val{w}, $(args...)) where {w} = invoke(_isprimitive, Tuple{Val{w},Vararg{Any}}, world, $(args...))

        @inline prehook(settings::Settings{C,M,w}, f, $(args...)) where {C,M,w} = invoke(prehook, Tuple{Settings{C,M,w},Any,Vararg{Any}}, settings, f, $(args...))
        @inline posthook(settings::Settings{C,M,w}, f, $(args...)) where {C,M,w} = invoke(posthook, Tuple{Settings{C,M,w},Any,Vararg{Any}}, settings, f, $(args...))
        @inline execution(settings::Settings{C,M,w}, f, $(args...)) where {C,M,w} = invoke(execution, Tuple{Settings{C,M,w},Any,Vararg{Any}}, settings, f, $(args...))
        @inline isprimitive(settings::Settings{C,M,w}, f, $(args...)) where {C,M,w} = invoke(isprimitive, Tuple{Settings{C,M,w},Any,Vararg{Any}}, settings, f, $(args...))

        @inline prehook(o::Overdub, $(args...)) = invoke(prehook, Tuple{Overdub,Vararg{Any}}, o, $(args...))
        @inline posthook(o::Overdub, $(args...)) = invoke(posthook, Tuple{Overdub,Vararg{Any}}, o, $(args...))
        @inline isprimitive(o::Overdub, $(args...)) = invoke(isprimitive, Tuple{Overdub,Vararg{Any}}, o, $(args...))

        @inline execute(o::Overdub, $(args...)) = invoke(execute, Tuple{Overdub,Vararg{Any}}, o, $(args...))
        @inline execute(p::Val{true}, o::Overdub, $(args...)) = invoke(execute, Tuple{Val{true},Overdub,Vararg{Any}}, p, o, $(args...))
        @inline execute(p::Val{false}, o::Overdub, $(args...)) = invoke(execute, Tuple{Val{false},Overdub,Vararg{Any}}, p, o, $(args...))

        # TODO: use invoke here as well; see https://github.com/jrevels/Cassette.jl/issues/5#issuecomment-341525276
        @inline function (o::Overdub{Execute})($(args...))
            prehook(o, $(args...))
            output = execute(o, $(args...))
            posthook(o, output, $(args...))
            return output
        end

        # contextual/metadata.jl workarounds
        @inline mapcall(g, f, $(args...)) = invoke(mapcall, Tuple{Any,Any,Vararg{Any}}, g, f, $(args...))
        @inline _newbox(ctx::C, t::Type{T}, $(args...)) where {C<:Context,T} = invoke(_newbox, Tuple{C,Type{T},Vararg{Any}}, ctx, t, $(args...))
        @inline _new(t::Type{T}, $(args...)) where {T} = invoke(_new, Tuple{Type{T},Vararg{Any}}, t, $(args...))
    end
end
