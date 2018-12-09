# TODO: this file is pretty sparse...

using InteractiveUtils: subtypes

inferred_retval(f, sig) = code_typed(f, sig)[1].first.code[end].args[1]

@context CanOverdubTestCtx
ctx = CanOverdubTestCtx()
# test that the result value of `canoverdub` is inferred exactly for `T <: Core.Builtin`
for T in subtypes(Core.Builtin)
    if !(T <: typeof(Core._apply))
        @test !inferred_retval(canoverdub, (typeof(ctx), T))
        @test !inferred_retval(canoverdub, (typeof(ctx), typeof(Core._apply), T))
        @test !inferred_retval(canoverdub, (typeof(ctx), typeof(Core.invoke), T))
    end
end

@test canoverdub(ctx, hypot, 1, 2)
@test canoverdub(ctx, Core.invoke, hypot, Tuple{Int,Int}, 1, 2)
@test canoverdub(ctx, Core._apply, hypot, (1, 2))
@test canoverdub(ctx, Core._apply, Core.invoke, (hypot, Tuple{Int,Int}, 1, 2))

# Declaring a `@context` with the same name twice should be a no-op, not an
# error; this code will throw an error otherwise
@context FooBar
@context FooBar
