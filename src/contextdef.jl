#######################
# unhygeniec bindings #
#######################

const CONTEXT_TYPE_BINDING = Symbol("__CONTEXT__")
const CONTEXT_BINDING = Symbol("__context__")

###################
# stubs/utilities #
###################

# these stubs are only overloaded on a per-context basis
function generate_tag end
function similar_context end

# this @pure annotations has official vtjnash approval :p
Base.@pure pure_objectid(x) = objectid(x)

###########################################
# context definition generation from name #
###########################################

function generate_context_definition(Ctx)
    @assert isa(Ctx, Symbol) "context name must be a Symbol"
    CtxTag = gensym(string(Ctx, "Tag"))
    return quote
        struct $CtxTag{E,H} <: $Cassette.AbstractTag end

        $CtxTag(x) = $CtxTag($Cassette.BottomTag(), x)
        $CtxTag(::E, ::X) where {E,X} = $CtxTag{E,$Cassette.pure_objectid(X)}()

        struct $Ctx{M,T<:$CtxTag,P<:$Cassette.AbstractPass,B<:Union{Nothing,$Cassette.BindingMetaCache}} <: $Cassette.AbstractContext{T,P,B}
            metadata::M
            tag::T
            pass::P
            bindings::B # tagging functionality is considered disabled if this field is `nothing`
        end

        function $Ctx(;
                      metadata = nothing,
                      pass::$Cassette.AbstractPass = $Cassette.NoPass(),
                      tagging_enabled::Bool = false)
            bindings = tagging_enabled ? $Cassette.BindingMetaCache() : nothing
            return $Ctx(metadata, $CtxTag(nothing), pass, bindings)
        end

        $Cassette.generate_tag(ctx::$Ctx, f) = $CtxTag(f)

        function $Cassette.similar_context(ctx::$Ctx;
                                           metadata = ctx.metadata,
                                           tag = ctx.tag,
                                           pass = ctx.pass,
                                           bindings = ctx.bindings)
            return $Ctx(metadata, tag, pass, bindings)
        end

        #=== default primitives ===#

        $Cassette.@primitive function $CtxTag(x) where {__CONTEXT__<:$Ctx}
            return $CtxTag(__context__.tag, x)
        end

        $Cassette.@primitive function Core._apply(f, args...) where {__CONTEXT__<:$Ctx}
            flattened_args = Core._apply(tuple, args...)
            return $Cassette.overdub_execute(__context__, f, flattened_args...)
        end

        $Cassette.@primitive function Array{T,N}(undef::UndefInitializer, args...) where {T,N,__CONTEXT__<:$Ctx}
            return $Cassette.tagged_new(__context__, Array{T,N}, undef, args...)
        end

        $Cassette.@primitive function Base.nameof(m) where {__CONTEXT__<:$Ctx}
            return $Cassette.tagged_nameof(__context__, m)
        end

        $Cassette.@primitive function Core.getfield(x, name) where {__CONTEXT__<:$Ctx}
            return $Cassette.tagged_getfield(__context__, x, name)
        end

        $Cassette.@primitive function Core.setfield!(x, name, y) where {__CONTEXT__<:$Ctx}
            return $Cassette.tagged_setfield!(__context__, x, name, y)
        end

        $Cassette.@primitive function Core.arrayref(boundscheck, x, i) where {__CONTEXT__<:$Ctx}
            return $Cassette.tagged_arrayref(__context__, boundscheck, x, i)
        end

        $Cassette.@primitive function Core.arrayset(boundscheck, x, y, i) where {__CONTEXT__<:$Ctx}
            return $Cassette.tagged_arrayset(__context__, boundscheck, x, y, i)
        end

        $Cassette.@primitive function Base._growbeg!(x, delta) where {__CONTEXT__<:$Ctx}
            return $Cassette.tagged_growbeg!(__context__, x, delta)
        end

        $Cassette.@primitive function Base._growend!(x, delta) where {__CONTEXT__<:$Ctx}
            return $Cassette.tagged_growend!(__context__, x, delta)
        end

        $Cassette.@primitive function Base._growat!(x, i, delta) where {__CONTEXT__<:$Ctx}
            return $Cassette.tagged_growat!(__context__, x, i, delta)
        end

        $Cassette.@primitive function Base._deletebeg!(x, delta) where {__CONTEXT__<:$Ctx}
            return $Cassette.tagged_deletebeg!(__context__, x, delta)
        end

        $Cassette.@primitive function Base._deleteend!(x, delta) where {__CONTEXT__<:$Ctx}
            return $Cassette.tagged_deleteend!(__context__, x, delta)
        end

        $Cassette.@primitive function Base._deleteat!(x, i, delta) where {__CONTEXT__<:$Ctx}
            return $Cassette.tagged_deleteat!(__context__, x, i, delta)
        end
    end
end
