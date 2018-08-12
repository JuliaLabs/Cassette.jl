# TODO: this file is pretty sparse...

using InteractiveUtils: subtypes

inferred_retval(f, sig) = code_typed(f, sig)[1].first.code[end].args[1]

@context CanOverdubTestCtx
ctx = CanOverdubTestCtx()
# test that the result value of is inferred exactly for `T <: Core.Builtin`
for T in subtypes(Core.Builtin)
    if !(T <: typeof(Core._apply))
        @test !inferred_retval(canoverdub, (typeof(ctx), T))
        @test !inferred_retval(canoverdub, (typeof(ctx), typeof(Core._apply), T))
    end
end
