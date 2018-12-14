# TODO: this file is pretty sparse...

using InteractiveUtils: subtypes
using Core.Compiler: SSAValue, Const, GotoNode

function const_bool_retval(f, sig)
    ctyped = code_typed(f, sig)[1].first
    retval = ctyped.code[end]
    retval = isa(retval, GotoNode) ? ctyped.code[retval.label] : retval
    retval = Meta.isexpr(retval, :return) ? retval.args[1] : retval
    retval = isa(retval, SSAValue) ? ctyped.ssavaluetypes[retval.id] : retval
    retval = isa(retval, Const) ? retval.val : retval
    isa(retval, Bool) && return retval
    error("did not infer constant boolean return value for ", f, sig)
end

@context canrecurseTestCtx
ctx = canrecurseTestCtx()
# Test that the result value of `canrecurse` is inferred exactly for `T <: Core.Builtin`.
# Note that these aren't necessarily valid full call signatures, this is just
# testing that Cassette's `canrecurse` implementation has an inferrable
# short-circuiting `Core.Builtin` check.
for T in subtypes(Core.Builtin)
    if !(T <: typeof(Core._apply))
        @test !const_bool_retval(canrecurse, (typeof(ctx), T))
        @test !const_bool_retval(canrecurse, (typeof(ctx), typeof(Core._apply), T))
        @test !const_bool_retval(canrecurse, (typeof(ctx), typeof(Core.invoke), T))
    end
end

@test canrecurse(ctx, hypot, 1, 2)
@test canrecurse(ctx, Core.invoke, hypot, Tuple{Int,Int}, 1, 2)
@test canrecurse(ctx, Core._apply, hypot, (1, 2))
@test canrecurse(ctx, Core._apply, Core.invoke, (hypot, Tuple{Int,Int}, 1, 2))

###########

# Declaring a `@context` with the same name twice, should be a No-Op, not an error
# Code below will throw an error if this is not true

@context FooBar
@context FooBar
