var documenterSearchIndex = {"docs": [

{
    "location": "index.html#",
    "page": "Introduction",
    "title": "Introduction",
    "category": "page",
    "text": ""
},

{
    "location": "index.html#Introduction-1",
    "page": "Introduction",
    "title": "Introduction",
    "category": "section",
    "text": "Hello! Welcome to Cassette\'s documentation.For an initial overview of Cassette, please see the README. Otherwise, feel free to peruse available documentation via the sidebar.It is recommended that all Cassette users read the documentation in its entirety. It is highly recommended that all Cassette users at least read the Disclaimers section before attempting to use Cassette."
},

{
    "location": "disclaimers.html#",
    "page": "Disclaimers",
    "title": "Disclaimers",
    "category": "page",
    "text": ""
},

{
    "location": "disclaimers.html#Disclaimers-(Don\'t-Say-I-Didn\'t-Warn-You)-1",
    "page": "Disclaimers",
    "title": "Disclaimers (Don\'t Say I Didn\'t Warn You)",
    "category": "section",
    "text": "Cassette can be a powerful tool for extending the Julia language, but it functions equally well as a loaded foot-gun. Here are some things one should know before using Cassette:Cassette, its API, and its documentation targets Julia package developers and/or those   interested in doing compiler research using Julia. Cassette users are expected to have   a working understanding of Julia\'s compiler, type system and metaprogramming facilities.\nPractical usage of Cassette will quite likely reveal both performance and correctness   bugs caused by either Cassette or Julia itself (especially in this early stage of   development). This is especially likely when doing nested overdubbing, or composing   multiple Cassette contexts. Please file issues on the Cassette and/or Julia issue   tracker where appropriate.\nThe performance of Cassette\'s implementation of the contextual tagging system heavily   depends on compiler improvements planned for the Julia 1.x release cycle. In theory,   given these compiler improvements, the contextual tagging system could achieve   performance comparable to alternatives (e.g. ForwardDiff\'s dual number implementation),   but for now, the contextual tagging system is quite slow and allocation-heavy.\nCassette enables interaction with many parts of the Julia compiler, a lot of which are   undocumented or sparsely documented. It is extremely easy to accidentally implement a   pass that breaks internal compiler assumptions in some subtle way. If (and when) you   run into these scenarios, it would be helpful to open an issue on the Julia issue tracker   proposing better documentation (or even a stable API) for a specific part of the   compiler.\nDue to limitations of Julia\'s current world-age mechanism, Cassette exhibits a similar   recompilation problem to the famous   JuliaLang/julia#265 (see   jrevels/Cassette.jl#6 for details). In   order to resolve this issue, an update to Julia\'s world-age mechanism is planned for the   Julia 1.x release cycle."
},

{
    "location": "whycassette.html#",
    "page": "Why Cassette?",
    "title": "Why Cassette?",
    "category": "page",
    "text": ""
},

{
    "location": "whycassette.html#Why-Cassette?-1",
    "page": "Why Cassette?",
    "title": "Why Cassette?",
    "category": "section",
    "text": "Due to its multiple dispatch, method overloading and metaprogramming capabilities, Julia has turned out to be a great language for implementing various forms of \"nonstandard execution\", enabling generically-written Julia programs to be executed with new features simply by passing new types through the code. Of course, many other languages allow this kind of polymorphism, but Julia is unique in its ability to facilitate these kinds of techniques with surprising elegance and performance. These key advantages have attracted thriving, state-of-the-art Julia implementations of numerical computing tools in regimes such as mathematical optimization and parallel programming.Over time, however, these amazing Julia tools have run up against the boundaries of purely overloading-based approaches to nonstandard execution. Cassette is an attempt to push these boundaries by giving Julia package developers the ability to fundamentally extend the language for their own purposes.Here are some specific limitations that might be overcome by using Cassette:Overloading-based approaches to nonstandard execution are ultimately thwarted by dispatch and/or structural type constraints in non-generic target programs.\nProper usage of overloading-based nonstandard execution tools require proper genericity criteria, i.e. answering \"what weird subset of Julia do I really support?\". These criteria are often difficult to correctly define and test against for both experienced developers and end-users.\nNot all relevant Julia language mechanisms are fully exposed/interceptable via method overloading (e.g. control flow, literals, bindings). Additionally, many different compiler passes require access to a wider scope of information than is locally available at individual method callsites (e.g. graph-based transformations).\nIt is often difficult to fully resolve the kinds of ambiguities that naturally result from the composition of several multiple-dispatch-based tools. Brute-force solutions end up generating an excessive number of methods and are often either code-load-order dependent or incomplete (or even both). Julia\'s promotion mechanism resolves this problem when the involved types are efficiently convertible to one another, but this is not often the case in practice (e.g. subtypes of AbstractArray)."
},

{
    "location": "whycassette.html#No,-why-the-*name*-\"Cassette\"?-1",
    "page": "Why Cassette?",
    "title": "No, why the name \"Cassette\"?",
    "category": "section",
    "text": "Cassette was originally motivated by the need for better language-level support for automatic differentiation, where the data structure used to accumulate program traces is often called a \"tape\" (since, of course, program traces really were stored on magnetic tapes back in the day)."
},

{
    "location": "overdub.html#",
    "page": "The Overdubbing Mechanism",
    "title": "The Overdubbing Mechanism",
    "category": "page",
    "text": ""
},

{
    "location": "overdub.html#The-Overdubbing-Mechanism-1",
    "page": "The Overdubbing Mechanism",
    "title": "The Overdubbing Mechanism",
    "category": "section",
    "text": "CurrentModule = CassetteThe central mechanism that drives Cassette usage is called the \"overdubbing\" mechanism.A naive usage of this mechanism looks like this:julia> using Cassette\n\njulia> Cassette.@context Ctx;\n\njulia> Cassette.overdub(Ctx(), /, 1, 2)\n0.5Okay - what did we actually just do here? From the output, it seems like we just computed 1/2...and indeed, we did! In reality, however, Cassette was doing a lot of work behind the scenes during this seemingly simple calculation. Let\'s drill down further into this example to see what\'s actually going on.First, we define a new Context type alias called Ctx via the @context macro. In practical usage, one normally defines one or more contexts specific to one\'s application of Cassette. Here, we just made a dummy one for illustrative purposes. Contexts are relatively simple to construct and understand, and are of central importance to Cassette\'s operation. I recommend skimming the Context docstring before moving forward.Next, we \"overdubbed\" a call to 1/2 w.r.t. Ctx() using the overdub function. To get a sense of what that means, let\'s look at the lowered IR for the original call:julia> @code_lowered 1/2\nCodeInfo(\n59 1 ─ %1 = (Base.float)(x)\n   │   %2 = (Base.float)(y)\n   │   %3 = %1 / %2\n   └──      return %3\n)And now let\'s look at lowered IR for the call to overdub(Ctx(), /, 1, 2)julia> @code_lowered Cassette.overdub(Ctx(), /, 1, 2)\nCodeInfo(\n59 1 ─       #self# = (Core.getfield)(##overdub_arguments#361, 1)                                                 │\n   │         x = (Core.getfield)(##overdub_arguments#361, 2)                                                      │\n   │         y = (Core.getfield)(##overdub_arguments#361, 3)                                                      │\n   │         (Cassette.prehook)(##overdub_context#360, Base.float, x)                                             │\n   │   %5  = (Cassette.overdub)(##overdub_context#360, Base.float, x)                                             │\n   │         (Cassette.posthook)(##overdub_context#360, %5, Base.float, x)                                        │\n   │   %7  = %5                                                                                                   │\n   │         (Cassette.prehook)(##overdub_context#360, Base.float, y)                                             │\n   │   %9  = (Cassette.overdub)(##overdub_context#360, Base.float, y)                                             │\n   │         (Cassette.posthook)(##overdub_context#360, %9, Base.float, y)                                        │\n   │   %11 = %9                                                                                                   │\n   │         (Cassette.prehook)(##overdub_context#360, Base.:/, %7, %11)                                          │\n   │   %13 = (Cassette.overdub)(##overdub_context#360, Base.:/, %7, %11)                                          │\n   │         (Cassette.posthook)(##overdub_context#360, %13, Base.:/, %7, %11)                                    │\n   │   %15 = %13                                                                                                  │\n   └──       return %15                                                                                           │\n)There\'s obviously a lot more going on here than in the lowered IR for 1/2, but if you squint, you might notice that the overdubbed IR is actually the original IR with a special transformation applied to it. Specifically, the overdubbed IR is the lowered IR for the given function call with all internal method invocations of the form f(args...) replaced by statements similar to the following:begin\n    Cassette.prehook(context, f, args...)\n    %n = Cassette.overdub(context, f, args...)\n    Cassette.posthook(context, %n, f, args...)\n    %n\nendIt is here that we experience our first bit of overdubbing magic: for every method call in the overdubbed trace, we obtain several extra points of overloadability that we didn\'t have before! In the following section on contextual dispatch, we\'ll explore how prehook, posthook, and even overdub itself can be overloaded to add new contextual behaviors to overdubbed programs.In the meantime, we should clarify how overdub is achieving this feat. Let\'s start by examining a \"pseudo-implementation\" of overdub:@generated function overdub(context::C, args...) where C<:Context\n    reflection = Cassette.reflect(args)\n    if isa(reflection, Cassette.Reflection)\n        Cassette.overdub_pass!(reflection, C)\n        return reflection.code_info\n    else\n        return :(Cassette.fallback(context, args...))\n    end\nendAs you can see, overdub is a @generated function, and thus returns a method body computed from the run-time types of its inputs. To actually compute this method body, overdub is doing something quite special.First, via Cassette.reflect, overdub asks Julia\'s compiler to provide it with a bunch of information about the original method call as specified by args. The result of this query is reflection, which is a Cassette.Reflection object if the compiler found lowered IR for args and nothing otherwise (e.g. if args specifies a built-in call like getfield whose implementation is not, itself, Julia code). For the former case, we execute a pass over the reflection and the lowered IR stored within (Cassette.overdub_pass!) to perform the previously presented transformation, returning the new lowered IR as a CodeInfo object. Otherwise, if reflection is not a Reflection object, then no lowered IR is available, so we simply call the context\'s fallback method (which, by default, simply calls the provided function)."
},

{
    "location": "contextualdispatch.html#",
    "page": "Contextual Dispatch",
    "title": "Contextual Dispatch",
    "category": "page",
    "text": ""
},

{
    "location": "contextualdispatch.html#Contextual-Dispatch-1",
    "page": "Contextual Dispatch",
    "title": "Contextual Dispatch",
    "category": "section",
    "text": "CurrentModule = CassetteIn the previous section, we saw how, within a given execution trace, Cassette\'s overdub mechanism transforms every method invocation of the form f(args...) into statements similar to the following:begin\n    Cassette.prehook(context, f, args...)\n    %n = Cassette.overdub(context, f, args...)\n    Cassette.posthook(context, %n, f, args...)\n    %n\nendThis transformation yields several extra points of overloadability in the form of various Cassette methods, such as prehook, posthook, and even overdub itself. Together, these methods form Cassette\'s \"contextual dispatch\" interface, so-named because it enables an extra context parameter to participate in what would normally be a simple dispatch to the underlying method call.In this section of the documentation, we\'ll go over these functions in a bit more detail.To begin, let\'s define a simple contextual prehook by overloading the prehook method w.r.t. to a dummy context:julia> using Cassette\n\njulia> Cassette.@context Ctx;\n\n# this prehook implements simple trace logging for overdubbed functions\njulia> Cassette.prehook(::Ctx, f, args...) = println(f, args)\n\njulia> Cassette.overdub(Ctx(), /, 1, 2)\nfloat(1,)\nAbstractFloat(1,)\nFloat64(1,)\nsitofp(Float64, 1)\nfloat(2,)\nAbstractFloat(2,)\nFloat64(2,)\nsitofp(Float64, 2)\n/(1.0, 2.0)\ndiv_float(1.0, 2.0)\n0.5Cool beans!Actually, there\'s a subtlety about overdub here that we should address before moving on. Why wasn\'t the first line in the trace log /(1, 2)? If the answer isn\'t obvious, recall the definition of overdub from the previous section. With that definition in mind, it makes sense that /(1, 2) is not printed in the above example, since prehook(Ctx(), /, 1, 2) is not actually ever called in the above example. If this still seems confusing, compare the output from the above example with the output generated via overdub(Ctx(), () -> 1/2).Moving on, let\'s make our prehook slightly more complicated for pedagogy\'s sake. This time around, we\'ll only print calls whose first argument matches a specific type. A nice configurable way to do this is as follows:# reset our prehook fallback for `Ctx` to a no-op\njulia> Cassette.prehook(::Ctx, f, args...) = nothing\n\n# parameterize our prehook on the type of metadata stored in our context instance\njulia> Cassette.prehook(::Ctx{Val{T}}, f, arg::T, rest...) where {T} = println(f, (arg, rest...))\n\n# construct our context instance with metadata to configure the prehook\njulia> Cassette.overdub(Ctx(metadata=Val(Int)), /, 1, 2)\nfloat(1,)\nAbstractFloat(1,)\nFloat64(1,)\nfloat(2,)\nAbstractFloat(2,)\nFloat64(2,)\n0.5\n\njulia> Cassette.overdub(Ctx(metadata=Val(DataType)), /, 1, 2)\nsitofp(Float64, 1)\nsitofp(Float64, 2)\n0.5Also of note is prehook\'s long-lost cousin posthook, with which prehook shares many similarities. In fact, these functions are so similar that we won\'t be spending too much time on posthook individually. The key difference between prehook and posthook is that posthook runs after the overdubbed invocation is executed, such that it has access to the output of the overdubbed invocation.For example, here we use posthook and prehook together to accumulate a trace that preserves nesting information:using Cassette\n\nCassette.@context TraceCtx\n\nmutable struct Trace\n    current::Vector{Any}\n    stack::Vector{Any}\n    Trace() = new(Any[], Any[])\nend\n\nfunction enter!(t::Trace, args...)\n    pair = args => Any[]\n    push!(t.current, pair)\n    push!(t.stack, t.current)\n    t.current = pair.second\n    return nothing\nend\n\nfunction exit!(t::Trace)\n    t.current = pop!(t.stack)\n    return nothing\nend\n\nCassette.prehook(ctx::TraceCtx, args...) = enter!(ctx.metadata, args...)\nCassette.posthook(ctx::TraceCtx, args...) = exit!(ctx.metadata)\n\ntrace = Trace()\nx, y, z = rand(3)\nf(x, y, z) = x*y + y*z\nCassette.overdub(TraceCtx(metadata = trace), () -> f(x, y, z))\n\n# returns `true`\ntrace.current == Any[\n    (f,x,y,z) => Any[\n        (*,x,y) => Any[(Base.mul_float,x,y)=>Any[]]\n        (*,y,z) => Any[(Base.mul_float,y,z)=>Any[]]\n        (+,x*y,y*z) => Any[(Base.add_float,x*y,y*z)=>Any[]]\n    ]\n]Next, let\'s tackle the meatiest part of the contextual dispatch interface: contextual primitives. A method invocation of the form f(args...) within a given context Ctx is a primitive w.r.t. Ctx if overdub(Ctx(), f, args...) does not recursively overdub the function calls comprising the invoked method\'s implementation. There are two cases where overdub(Ctx(), f, args...) does not correspond to recursively overdubbing f\'s implementation:f(args...) might be a built-in with no overdubbable Julia implementation (e.g. getfield), in which case overdub(Ctx(), f, args...) immediately redirects to Cassette.fallback(Ctx(), f, args...).\noverdub can be overloaded by the user such that overdub(::Ctx, ::typeof(f), ...) dispatches to a context-specific primitive definition.If this definition isn\'t exactly intuitive, never fear - the concept of a contextual primitive is more easily understood via examples. The simplest example is to define a context that simply redirects all method call of a specific type (let\'s say sin(x)) to a different method call of a specific type (let\'s say cos(x)). This can be expressed as follows:using Cassette, Test\n\nCassette.@context SinToCosCtx\n\n# Override the default recursive `overdub` implementation for `sin(x)`.\n# Note that there\'s no tricks here; this is just a normal Julia method\n# overload using the normal multiple dispatch semantics.\nCassette.overdub(::SinToCosCtx, ::typeof(sin), x) = cos(x)\n\nx = rand(10)\ny = Cassette.overdub(SinToCosCtx(), sum, i -> cos(i) + sin(i), x)\n@test y == sum(i -> 2 * cos(i), x)Pretty nifty!Here\'s a more motivating example. Below, we define a context that allows us to memoize the computation of Fibonacci numbers (many thanks to the illustrious Simon Byrne, the original author of this example):using Cassette: Cassette, @context, overdub, recurse\n\nfib(x) = x < 3 ? 1 : fib(x - 2) + fib(x - 1)\nfibtest(n) = fib(2 * n) + n\n\n@context MemoizeCtx\n\nfunction Cassette.overdub(ctx::MemoizeCtx, ::typeof(fib), x)\n    result = get(ctx.metadata, x, 0)\n    if result === 0\n        result = recurse(ctx, fib, x)\n        ctx.metadata[x] = result\n    end\n    return result\nendNote that this example uses Cassette\'s recurse function. This function is exactly equivalent to Cassette\'s default overdub implementation, but is not meant to be overloaded by users, thus allowing one to recursively overdub \"through\" invocations that might otherwise be contextual primitives.We can do some toy performance tests to see that we get the expected speedup using this implementation (skipping the warm-up calls used to compile both functions):julia> ctx = MemoizeCtx(metadata = Dict{Int,Int}());\n\njulia> @time Cassette.overdub(ctx, fibtest, 20)\n  0.000011 seconds (8 allocations: 1.547 KiB)\n102334175\n\njulia> @time Cassette.overdub(ctx, fibtest, 20)\n  0.000006 seconds (5 allocations: 176 bytes)\n102334175\n\njulia> @time fibtest(20)\n  0.276069 seconds (5 allocations: 176 bytes)\n102334175note: Note\nA bunch of reasonable default contextual primitives are generated automatically upon context definition. It is possible, of course, to simply override these defaults if necessary. For more details, see @context.Finally, to get a sense of the interaction between recurse and overdub, let\'s reimplement our previous nested tracing example using recursion instead of maintaining a stack:using Cassette\n\nCassette.@context TraceCtx\n\nfunction Cassette.overdub(ctx::TraceCtx, args...)\n    subtrace = Any[]\n    push!(ctx.metadata, args => subtrace)\n    if Cassette.canrecurse(ctx, args...)\n        newctx = Cassette.similarcontext(ctx, metadata = subtrace)\n        return Cassette.recurse(newctx, args...)\n    else\n        return Cassette.fallback(ctx, args...)\n    end\nend\n\ntrace = Any[]\nx, y, z = rand(3)\nf(x, y, z) = x*y + y*z\nCassette.overdub(TraceCtx(metadata = trace), f, x, y, z)\n\n# returns `true`\ntrace == Any[\n   (f,x,y,z) => Any[\n       (*,x,y) => Any[(Base.mul_float,x,y)=>Any[]]\n       (*,y,z) => Any[(Base.mul_float,y,z)=>Any[]]\n       (+,x*y,y*z) => Any[(Base.add_float,x*y,y*z)=>Any[]]\n   ]\n]"
},

{
    "location": "contextualpass.html#",
    "page": "Contextual Compiler Pass Injection",
    "title": "Contextual Compiler Pass Injection",
    "category": "page",
    "text": ""
},

{
    "location": "contextualpass.html#Contextual-Compiler-Pass-Injection-1",
    "page": "Contextual Compiler Pass Injection",
    "title": "Contextual Compiler Pass Injection",
    "category": "section",
    "text": "CurrentModule = CassetteIn the previous section on Cassette\'s overdubbing mechanism, we explored how Cassette can automatically transform methods\' lowered representation to insert a bunch of statements around method calls encountered while overdubbing. In the section that followed, we discussed the result of this IR transformation: Cassette\'s contextual dispatch interface, a suite of normal Julia methods that can be easily overloaded to perform the kinds of method replacement and instrumentation that would otherwise require manually implemented compiler passes.Some use cases, however, require the ability to access and/or alter properties of the execution trace that just can\'t be reached via simple method overloading, like control flow or the surrounding scope of a method call. In these cases, you probably do want to manually implement a compiler pass!To facilitate these use cases, Cassette allows users to write and inject their own arbitrary post-lowering, pre-inference compiler passes as part of the overdubbing process. This feature of Cassette is called \"contextual pass injection\". As we did in the preceding sections, we\'ll be using the classic \"trial-by-fire\" technique to better understand this feature.Note that the following example was originally inspired by jrevels/Cassette.jl#66.Let\'s say you wanted to use Cassette to \"slice\" various separable subcomputations out from an overall computation. For a specific example, let\'s say you wanted to implement a tool that takes a Julia function and strips out calls to println encountered in the trace. When this function returns, we also want to return a callback that executes all the println calls that we stripped out. How would you implement this with Cassette?Well, it\'s not too hard to achieve this via the contextual dispatch interface:using Cassette\n\nCassette.@context Ctx\n\nmutable struct Callback\n    f::Any\nend\n\nfunction Cassette.overdub(ctx::Ctx, ::typeof(println), args...)\n    previous = ctx.metadata.f\n    ctx.metadata.f = () -> (previous(); println(args...))\n    return nothing\nendWe can check our implementation using the following test case:julia> begin\n           a = rand(3)\n           b = rand(3)\n           function add(a, b)\n               println(\"I\'m about to add $a + $b\")\n               c = a + b\n               println(\"c = $c\")\n               return c\n           end\n           add(a, b)\n       end\nI\'m about to add [0.457465, 0.62078, 0.954555] + [0.0791336, 0.744041, 0.976194]\nc = [0.536599, 1.36482, 1.93075]\n3-element Array{Float64,1}:\n 0.5365985032259399\n 1.3648210555868863\n 1.9307494378914405\n\njulia> ctx = Ctx(metadata = Callback(() -> nothing));\n\njulia> c = Cassette.overdub(ctx, add, a, b)\n3-element Array{Float64,1}:\n 0.5365985032259399\n 1.3648210555868863\n 1.9307494378914405\n\njulia> ctx.metadata.f()\nI\'m about to add [0.457465, 0.62078, 0.954555] + [0.0791336, 0.744041, 0.976194]\nc = [0.536599, 1.36482, 1.93075]This is pretty cool, but also a bit limited. First of all, what if we want to move more than just println invocations into our callback, e.g. what if we want to slice the construction of println\'s arguments as well? Another potential issue is that this implementation requires an explicit Any barrier, preventing the compiler from inferring callback construction (note, however, that this does not prevent inferring invocation of the callback). This is possibly desirable in some cases, since you\'re being easier on the compiler, but what if you really wanted to expose Julia\'s type inference to callback construction?To resolve issues like these, we\'ll need to dig deeper than contextual dispatch and implement an actual compiler pass. First, let\'s go over a high-level description of the pass we\'ll implement. Essentially, we want this method:function add(a, b)\n    println(\"I\'m about to add $a + $b\")\n    c = a + b\n    println(\"c = $c\")\n    return c\nend...to become something akin to the following when overdubbed:function overdub(ctx::Ctx, add, a, b)\n    _callback_ = ctx.metadata\n    _, _callback_ = overdub(ctx, println, _callback_, \"I\'m about to add $a + $b\")\n    c, _callback_ = overdub(ctx, +, _callback_, a, b)\n    _, _callback_ = overdub(ctx, println, _callback_, \"c = $c\")\n    return c, _callback_\nendNote that I reduced a lot of the contextual dispatch boilerplate, such that the above is essentially pseudocode.Here, we can overload Ctx\'s overdub method with the following definitions:function Cassette.overdub(ctx::SliceCtx, f, callback, args...)\n    if Cassette.canrecurse(ctx, f, args...)\n        _ctx = Cassette.similarcontext(ctx, metadata = callback)\n        return Cassette.recurse(_ctx, f, args...) # return result, callback\n    else\n        return Cassette.fallback(ctx, f, args...), callback\n    end\nend\n\nfunction Cassette.overdub(ctx::SliceCtx, ::typeof(println), callback, args...)\n    return nothing, () -> (callback(); println(args...))\nendThis, then, essentially accumulates the same closure we were accumulating before, but does so in a way where......in theory, there is no longer any barrier to the inference of the closure construction.\n...the pass itself determines the \"capture region\" manually, such that one could just alter it   to do e.g. linear dependence analysis to capture println argument construction code an arbitrary   number of degrees out from the actual println invocation.Next, let\'s list the steps our compiler pass will actually need to perform in order to actually accomplish the above:At the beginning of each method body, insert something like _callback_ = context.metadata\nChange every method invocation of the form f(args...) to f(_callback_, args...).\nChange every return statement of the form return x to return (x, _callback_)\nEnsure the output of every method invocation is properly destructured into the original assignment slot/SSAValue and the _callback_ slot.Okay! Now that we have a high-level description of our pass, let\'s look at the code that implements it. I highly recommend reading the documentation for @pass and insert_statements! before trying to understand this code.using Cassette\nusing Core: CodeInfo, SlotNumber, SSAValue\n\nCassette.@context SliceCtx\n\nfunction Cassette.overdub(ctx::SliceCtx, f, callback, args...)\n    if Cassette.canrecurse(ctx, f, args...)\n        _ctx = Cassette.similarcontext(ctx, metadata = callback)\n        return Cassette.recurse(_ctx, f, args...) # return result, callback\n    else\n        return Cassette.fallback(ctx, f, args...), callback\n    end\nend\n\nfunction Cassette.overdub(ctx::SliceCtx, ::typeof(println), callback, args...)\n    return nothing, () -> (callback(); println(args...))\nend\n\nfunction sliceprintln(::Type{<:SliceCtx}, reflection::Cassette.Reflection)\n    ir = reflection.code_info\n    callbackslotname = gensym(\"callback\")\n    push!(ir.slotnames, callbackslotname)\n    push!(ir.slotflags, 0x00)\n    callbackslot = SlotNumber(length(ir.slotnames))\n    getmetadata = Expr(:call, Expr(:nooverdub, GlobalRef(Core, :getfield)), Expr(:contextslot), QuoteNode(:metadata))\n\n    # insert the initial `callbackslot` assignment into the IR.\n    Cassette.insert_statements!(ir.code, ir.codelocs,\n                                (stmt, i) -> i == 1 ? 2 : nothing,\n                                (stmt, i) -> [Expr(:(=), callbackslot, getmetadata), stmt])\n\n    # replace all calls of the form `f(args...)` with `f(callback, args...)`, taking care to\n    # properly handle Core._apply calls and destructure the returned `(result, callback)`\n    # into the appropriate statements\n    Cassette.insert_statements!(ir.code, ir.codelocs,\n                                (stmt, i) -> begin\n                                    i > 1 || return nothing # don\'t slice the callback assignment\n                                    stmt = Base.Meta.isexpr(stmt, :(=)) ? stmt.args[2] : stmt\n                                    if Base.Meta.isexpr(stmt, :call)\n                                        isapply = Cassette.is_ir_element(stmt.args[1], GlobalRef(Core, :_apply), ir.code)\n                                        return 3 + isapply\n                                    end\n                                    return nothing\n                                end,\n                                (stmt, i) -> begin\n                                    items = Any[]\n                                    callstmt = Base.Meta.isexpr(stmt, :(=)) ? stmt.args[2] : stmt\n                                    callssa = SSAValue(i)\n                                    if Cassette.is_ir_element(callstmt.args[1], GlobalRef(Core, :_apply), ir.code)\n                                        push!(items, Expr(:call, Expr(:nooverdub, GlobalRef(Core, :tuple)), callbackslot))\n                                        push!(items, Expr(:call, callstmt.args[1], callstmt.args[2], SSAValue(i), callstmt.args[3:end]...))\n                                        callssa = SSAValue(i + 1)\n                                    else\n                                        push!(items, Expr(:call, callstmt.args[1], callbackslot, callstmt.args[2:end]...))\n                                    end\n                                    push!(items, Expr(:(=), callbackslot, Expr(:call, Expr(:nooverdub, GlobalRef(Core, :getfield)), callssa, 2)))\n                                    result = Expr(:call, Expr(:nooverdub, GlobalRef(Core, :getfield)), callssa, 1)\n                                    if Base.Meta.isexpr(stmt, :(=))\n                                        result = Expr(:(=), stmt.args[1], result)\n                                    end\n                                    push!(items, result)\n                                    return items\n                                end)\n\n    # replace return statements of the form `return x` with `return (x, callback)`\n    Cassette.insert_statements!(ir.code, ir.codelocs,\n                                  (stmt, i) -> Base.Meta.isexpr(stmt, :return) ? 2 : nothing,\n                                  (stmt, i) -> begin\n                                      return [\n                                          Expr(:call, Expr(:nooverdub, GlobalRef(Core, :tuple)), stmt.args[1], callbackslot)\n                                          Expr(:return, SSAValue(i))\n                                      ]\n                                  end)\n    return ir\nend\n\nconst sliceprintlnpass = Cassette.@pass sliceprintlnHere\'s how to invoke this new implementation on the above test case:julia> begin\n           a = rand(3)\n           b = rand(3)\n           function add(a, b)\n               println(\"I\'m about to add $a + $b\")\n               c = a + b\n               println(\"c = $c\")\n               return c\n           end\n           add(a, b)\n       end\nI\'m about to add [0.325019, 0.19358, 0.200598] + [0.195759, 0.653, 0.498859]\nc = [0.520778, 0.84658, 0.699457]\n3-element Array{Float64,1}:\n 0.5207782045663867\n 0.846579992552251\n 0.6994565474128307\n\njulia> ctx = SliceCtx(pass=sliceprintlnpass, metadata = () -> nothing);\n\njulia> result, callback = Cassette.@overdub(ctx, add(a, b))\n([0.520778, 0.84658, 0.699457], getfield(Main, Symbol(\"##4#5\")){getfield(Main, Symbol(\"##4#5\")){getfield(Main, Symbol(\"##18#19\")),Tuple{String}},Tuple{String}}(getfield(Main, Symbol(\"##4#5\")){getfield(Main, Symbol(\"##18#19\")),Tuple{String}}(getfield(Main, Symbol(\"##18#19\"))(), (\"I\'m about to add [0.325019, 0.19358, 0.200598] + [0.195759, 0.653, 0.498859]\",)), (\"c = [0.520778, 0.84658, 0.699457]\",)))\n\njulia> callback()\nI\'m about to add [0.325019, 0.19358, 0.200598] + [0.195759, 0.653, 0.498859]\nc = [0.520778, 0.84658, 0.699457]"
},

{
    "location": "contextualtagging.html#",
    "page": "Contextual Tagging of Values",
    "title": "Contextual Tagging of Values",
    "category": "page",
    "text": "Documentation Coming Soon!"
},

{
    "location": "api.html#",
    "page": "Cassette API Documentation",
    "title": "Cassette API Documentation",
    "category": "page",
    "text": ""
},

{
    "location": "api.html#Cassette.Context",
    "page": "Cassette API Documentation",
    "title": "Cassette.Context",
    "category": "type",
    "text": "Context{N<:Cassette.AbstractContextName,\n        M<:Any,\n        P<:Cassette.AbstractPass,\n        T<:Union{Nothing,Cassette.Tag},\n        B<:Union{Nothing,Cassette.BindingMetaDictCache},\n        H<:Union{Nothing,Cassette.DisableHooks}}\n\nA type representing a Cassette execution context. This type is normally interacted with through type aliases constructed via Cassette.@context:\n\njulia> Cassette.@context MyCtx\nCassette.Context{nametype(MyCtx),M,P,T,B,H} where H<:Union{Nothing,DisableHooks}\n                                            where B<:Union{Nothing,IdDict{Module,Dict{Symbol,BindingMeta}}}\n                                            where P<:AbstractPass\n                                            where T<:Union{Nothing,Tag}\n                                            where M\n\nConstructors\n\nGiven a context type alias named e.g. MyCtx, an instance of the type can be constructed via:\n\nMyCtx(; metadata = nothing, pass = Cassette.NoPass())\n\nTo construct a new context instance using an existing context instance as a template, see the similarcontext function.\n\nTo enable contextual tagging for a given context instance, see the enabletagging function.\n\nFields\n\nname::N<:Cassette.AbstractContextName: a parameter used to disambiguate different   contexts for overloading purposes (e.g. distinguishes MyCtx from other Context type   aliases).\nmetadata::M<:Any: trace-local metadata as provided to the context constructor\ntag::T<:Union{Nothing,Tag}: the tag object that is attached to values when they are   tagged w.r.t. the context instance\npass::P<:Cassette.AbstractPass: the Cassette pass that will be applied to all method   bodies encountered during contextual execution (see the @pass macro for details).\nbindingscache::B<:Union{Nothing,BindingMetaDictCache}}: storage for metadata associated   with tagged module bindings\nhooktoggle::H<:Union{Nothing,DisableHooks}: configuration toggle for disabling   the overdub pass\'s prehook/posthook injection (see disablehooks   for details)\n\n\n\n\n\n"
},

{
    "location": "api.html#Cassette.similarcontext",
    "page": "Cassette API Documentation",
    "title": "Cassette.similarcontext",
    "category": "function",
    "text": "similarcontext(context::Context;\n               metadata = context.metadata,\n               pass = context.pass)\n\nReturn a copy of the given context, where the copy\'s metadata and/or pass fields are replaced with those provided via the corresponding keyword arguments.\n\n\n\n\n\n"
},

{
    "location": "api.html#Cassette.disablehooks",
    "page": "Cassette API Documentation",
    "title": "Cassette.disablehooks",
    "category": "function",
    "text": "disablehooks(context::Cassette.Context)\n\nReturn of copy of the given context with prehook/posthook injection disabled for the context. Disabling hook injection can reduce IR bloat in scenarios where these hooks are not being utilized.\n\n\n\n\n\n"
},

{
    "location": "api.html#Cassette.enabletagging",
    "page": "Cassette API Documentation",
    "title": "Cassette.enabletagging",
    "category": "function",
    "text": "enabletagging(context::Cassette.Context, f)\n\nReturn a copy of the given context with the tagging system enabled for the contextual execution of f.\n\nCassette uses the type of f to generate the tag field of the returned instance.\n\nNote that it is generally unsafe to use the returned instance to contextually execute functions other than f. Specifically, in cases of nested contextual execution where both inner and outer contexts employ the tagging system, improper application of the tagged system could cause (for example) separate contexts to erroneously interfere with each other\'s metadata propagation.\n\nSee also: hastagging\n\n\n\n\n\n"
},

{
    "location": "api.html#Cassette.hastagging",
    "page": "Cassette API Documentation",
    "title": "Cassette.hastagging",
    "category": "function",
    "text": "hastagging(::Type{<:Cassette.Context})\n\nReturns true if the given type indicates that the contextual tagging system is enabled for context instances of the type, returns false otherwise.\n\nExample\n\njulia> Cassette.@context MyCtx;\n\njulia> ctx = MyCtx();\n\njulia> Cassette.hastagging(typeof(ctx))\nfalse\n\njulia> ctx = Cassette.enabletagging(ctx, sum);\n\njulia> Cassette.hastagging(typeof(ctx))\ntrue\n\nSee also: enabletagging\n\n\n\n\n\n"
},

{
    "location": "api.html#Cassette.@context",
    "page": "Cassette API Documentation",
    "title": "Cassette.@context",
    "category": "macro",
    "text": "Cassette.@context Ctx\n\nDefine a new Cassette context type with the name Ctx. In reality, Ctx is simply a type alias for Cassette.Context{Cassette.nametype(Ctx)}.\n\nNote that Cassette.overdub is automatically overloaded w.r.t. Ctx to define several primitives by default. A full list of these default primitives can be obtained by running:\n\nmethods(Cassette.overdub, (Ctx, Vararg{Any}))\n\nNote also that many of the default primitives\' signatures only match when contextual tagging is enabled.\n\nSee also: Context\n\n\n\n\n\n"
},

{
    "location": "api.html#Cassette.overdub",
    "page": "Cassette API Documentation",
    "title": "Cassette.overdub",
    "category": "function",
    "text": "overdub(context::Context, f, args...)\n\nExecute f(args...) overdubbed with respect to context.\n\nMore specifically, execute f(args...), but with every internal method invocation g(x...) replaced by statements similar to the following:\n\nbegin\n    prehook(context, g, x...)\n    overdub(context, g, x...) # %n\n    posthook(context, %n, g, x...)\n    %n\nend\n\nOtherwise, if Cassette cannot retrieve lowered IR for the method body of f(args...), then fallback(context, f, args...) will be called instead. Cassette\'s canrecurse function is a useful utility for checking if this will occur.\n\nIf the injected prehook/posthook statements are not needed for your use case, you can disable their injection via the disablehooks function.\n\nAdditionally, for every method body encountered in the execution trace, apply the compiler pass associated with context if one exists. Note that this user-provided pass is performed on the method IR before method invocations are transformed into the form specified above. See the @pass macro for further details.\n\nIf Cassette.hastagging(typeof(context)), then a number of additional passes are run in order to accomodate tagged value propagation:\n\nExpr(:new) is replaced with a call to Cassette.tagged_new\nExpr(:splatnew) is replaced with a call to Cassette.tagged_splatnew\nconditional values passed to Expr(:gotoifnot) are untagged\narguments to Expr(:foreigncall) are untagged\nload/stores to external module bindings are intercepted by the tagging system\n\nThe default definition of overdub is to recursively enter the given function and continue overdubbing, but one can interrupt/redirect this recursion by overloading overdub w.r.t. a given context and/or method signature to define new contextual execution primitives. For example:\n\njulia> using Cassette\n\njulia> Cassette.@context Ctx;\n\njulia> Cassette.overdub(::Ctx, ::typeof(sin), x) = cos(x)\n\njulia> Cassette.overdub(Ctx(), x -> sin(x) + cos(x), 1) == 2 * cos(1)\ntrue\n\nSee also: recurse, prehook, posthook\n\n\n\n\n\n"
},

{
    "location": "api.html#Cassette.@overdub",
    "page": "Cassette API Documentation",
    "title": "Cassette.@overdub",
    "category": "macro",
    "text": "Cassette.@overdub(ctx, expression)\n\nA convenience macro for executing expression within the context ctx. This macro roughly expands to Cassette.recurse(ctx, () -> expression).\n\nSee also: overdub, recurse\n\n\n\n\n\n"
},

{
    "location": "api.html#Cassette.recurse",
    "page": "Cassette API Documentation",
    "title": "Cassette.recurse",
    "category": "function",
    "text": "recurse(context::Context, f, args...)\n\nExecute f(args...) overdubbed with respect to context.\n\nThis method performs exactly the same transformation as the default overdub transformation, but is not meant to be overloaded. Thus, one can call recurse to \"continue\" recursively overdubbing a function when calling overdub directly on that function might\'ve dispatched to a contextual primitive.\n\nTo illustrate why recurse might be useful, consider the following example which utilizes recurse as part of a Cassette-based memoization implementation for the classic Fibonacci function:\n\nusing Cassette: Cassette, @context, overdub, recurse\n\nfib(x) = x < 3 ? 1 : fib(x - 2) + fib(x - 1)\nfibtest(n) = fib(2 * n) + n\n\n@context MemoizeCtx\n\nfunction Cassette.overdub(ctx::MemoizeCtx, ::typeof(fib), x)\n    result = get(ctx.metadata, x, 0)\n    if result === 0\n        result = recurse(ctx, fib, x)\n        ctx.metadata[x] = result\n    end\n    return result\nend\n\nSee Cassette\'s Contextual Dispatch documentation for more details and examples.\n\n\n\n\n\n"
},

{
    "location": "api.html#Cassette.prehook",
    "page": "Cassette API Documentation",
    "title": "Cassette.prehook",
    "category": "function",
    "text": "prehook(context::Context, f, args...)\n\nOverload this Cassette method w.r.t. a given context in order to define a new contextual prehook for that context.\n\nTo understand when/how this method is called, see the documentation for overdub.\n\nInvoking prehook is a no-op by default (it immediately returns nothing).\n\nSee also: overdub, posthook, recurse, fallback\n\nExamples\n\nSimple trace logging:\n\njulia> Cassette.@context PrintCtx;\n\njulia> Cassette.prehook(::PrintCtx, f, args...) = println(f, args)\n\njulia> Cassette.overdub(PrintCtx(), /, 1, 2)\nfloat(1,)\nAbstractFloat(1,)\nFloat64(1,)\nsitofp(Float64, 1)\nfloat(2,)\nAbstractFloat(2,)\nFloat64(2,)\nsitofp(Float64, 2)\n/(1.0, 2.0)\ndiv_float(1.0, 2.0)\n0.5\n\nCounting the number of method invocations with one or more arguments of a given type:\n\njulia> mutable struct Count{T}\n           count::Int\n       end\n\njulia> Cassette.@context CountCtx;\n\njulia> Cassette.prehook(ctx::CountCtx{Count{T}}, f, arg::T, args::T...) where {T} = (ctx.metadata.count += 1)\n\n# count the number of calls of the form `f(::Float64, ::Float64...)`\njulia> ctx = CountCtx(metadata = Count{Float64}(0));\n\njulia> Cassette.overdub(ctx, /, 1, 2)\n0.5\n\njulia> ctx.metadata.count\n2\n\n\n\n\n\n"
},

{
    "location": "api.html#Cassette.posthook",
    "page": "Cassette API Documentation",
    "title": "Cassette.posthook",
    "category": "function",
    "text": "posthook(context::Context, output, f, args...)\n\nOverload this Cassette method w.r.t. a given context in order to define a new contextual posthook for that context.\n\nTo understand when/how this method is called, see the documentation for overdub.\n\nInvoking posthook is a no-op by default (it immediately returns nothing).\n\nSee also: overdub, prehook, recurse, fallback\n\nExamples\n\nSimple trace logging:\n\njulia> Cassette.@context PrintCtx;\n\njulia> Cassette.posthook(::PrintCtx, output, f, args...) = println(output, \" = \", f, args)\n\njulia> Cassette.overdub(PrintCtx(), /, 1, 2)\n1.0 = sitofp(Float64, 1)\n1.0 = Float64(1,)\n1.0 = AbstractFloat(1,)\n1.0 = float(1,)\n2.0 = sitofp(Float64, 2)\n2.0 = Float64(2,)\n2.0 = AbstractFloat(2,)\n2.0 = float(2,)\n0.5 = div_float(1.0, 2.0)\n0.5 = /(1.0, 2.0)\n0.5\n\nAccumulate the sum of all numeric scalar outputs encountered in the trace:\n\njulia> mutable struct Accum\n           x::Number\n       end\n\njulia> Cassette.@context AccumCtx;\n\njulia> Cassette.posthook(ctx::AccumCtx{Accum}, out::Number, f, args...) = (ctx.metadata.x += out)\n\njulia> ctx = AccumCtx(metadata = Accum(0));\n\njulia> Cassette.overdub(ctx, /, 1, 2)\n0.5\n\njulia> ctx.metadata.x\n13.0\n\n\n\n\n\n"
},

{
    "location": "api.html#Cassette.fallback",
    "page": "Cassette API Documentation",
    "title": "Cassette.fallback",
    "category": "function",
    "text": "fallback(context::Context, f, args...)\n\nOverload this Cassette method w.r.t. a given context in order to define a new contextual execution fallback for that context.\n\nTo understand when/how this method is called, see the documentation for overdub and canrecurse.\n\nBy default, invoking fallback(context, f, args...) will simply call f(args...) (with all arguments automatically untagged, if hastagging(typeof(context))).\n\nSee also:  canrecurse, overdub, recurse, prehook, posthook\n\n\n\n\n\n"
},

{
    "location": "api.html#Cassette.canrecurse",
    "page": "Cassette API Documentation",
    "title": "Cassette.canrecurse",
    "category": "function",
    "text": "canrecurse(context::Context, f, args...)\n\nReturn true if f(args...) has a lowered IR representation that Cassette can overdub, return false otherwise.\n\nAlternatively, but equivalently:\n\nReturn false if recurse(context, f, args...) directly translates to fallback(context, f, args...), return true otherwise.\n\nNote that unlike overdub, fallback, etc., this function is not intended to be overloaded.\n\nSee also:  overdub, fallback, recurse\n\n\n\n\n\n"
},

{
    "location": "api.html#Cassette.@pass",
    "page": "Cassette API Documentation",
    "title": "Cassette.@pass",
    "category": "macro",
    "text": "Cassette.@pass transform\n\nReturn a Cassette pass that can be provided to the Context constructor\'s pass keyword argument in order to apply transform to the lowered IR representations of all methods invoked during contextual execution.\n\ntransform must be a Julia object that is callable with the following signature:\n\ntransform(::Type{<:Context}, ::Cassette.Reflection)::Union{Expr,CodeInfo}\n\nIf isa(transform(...), Expr), then the returned Expr will be emitted immediately without any additional processing. Otherwise, if isa(transform(...), CodeInfo), then the returned CodeInfo will undergo the rest of Cassette\'s overdubbing transformation before being emitted from the overdub generator.\n\nTwo special Expr heads are available to Cassette pass authors that are not normally valid in Julia IR. Exprs with these heads can be used to interact with the downstream built-in Cassette passes that consume them.\n\n:nooverdub: Wrap an Expr with this head value around the first argument in an   Expr(:call) to tell downstream built-in Cassette passes not to overdub that call. For   example, Expr(:call, Expr(:nooverdub, GlobalRef(MyModule, :myfunc)), args...).\n:contextslot: Cassette will replace any Expr(:contextslot) with the actual SlotNumber   corresponding to the context object associated with the execution trace. For example, one   could construct an IR element that accesses the context\'s metadata field by emitting:   Expr(:call, Expr(:nooverdub, GlobalRef(Core, :getfield)), Expr(:contextslot), QuoteNode(:metadata))\n\nCassette provides a few IR-munging utility functions of interest to pass authors; for details, see insert_statements!, replace_match!, and is_ir_element.\n\nNote that the @pass macro expands to an eval call and thus should only be called at top-level. Furthermore, to avoid world-age issues, transform should not be overloaded after it has been registered with @pass.\n\nNote also that transform should be \"relatively pure.\" More specifically, Julia\'s compiler has license to apply transform multiple times, even if only compiling a single method invocation once. Thus, it is required that transform always return a generally \"equivalent\" CodeInfo for a given context, method body, and signature. If your transform implementation is not naturally \"pure\" in this sense, then it is still possible to guarantee this property by memoizing your implementation (i.e. maintaining a cache of previously computed IR results, instead of recomputing results every time).\n\nSee also: Context, overdub\n\n\n\n\n\n"
},

{
    "location": "api.html#Cassette.replace_match!",
    "page": "Cassette API Documentation",
    "title": "Cassette.replace_match!",
    "category": "function",
    "text": "replace_match!(replace, ismatch, x)\n\nReturn x with all subelements y replaced with replace(y) if ismatch(y). If !ismatch(y), but y is of type Expr, Array, or SubArray, then replace y in x with replace_match!(replace, ismatch, y).\n\nGenerally, x should be of type Expr, Array, or SubArray.\n\nNote that this function modifies x (and potentially its subelements) in-place.\n\nSee also: insert_statements!, is_ir_element\n\n\n\n\n\n"
},

{
    "location": "api.html#Cassette.insert_statements!",
    "page": "Cassette API Documentation",
    "title": "Cassette.insert_statements!",
    "category": "function",
    "text": "insert_statements!(code::Vector, codelocs::Vector, stmtcount, newstmts)\n\nFor every statement stmt at position i in code for which stmtcount(stmt, i) returns an Int, remove stmt, and in its place, insert the statements returned by newstmts(stmt, i). If stmtcount(stmt, i) returns nothing, leave stmt alone.\n\nFor every insertion, all downstream SSAValues, label indices, etc. are incremented appropriately according to number of inserted statements.\n\nProper usage of this function dictates that following properties hold true:\n\ncode is expected to be a valid value for the code field of a CodeInfo object.\ncodelocs is expected to be a valid value for the codelocs field of a CodeInfo object.\nnewstmts(stmt, i) should return a Vector of valid IR statements.\nstmtcount and newstmts must obey stmtcount(stmt, i) == length(newstmts(stmt, i)) if   isa(stmtcount(stmt, i), Int).\n\nTo gain a mental model for this function\'s behavior, consider the following scenario. Let\'s say our code object contains several statements:\n\ncode = Any[oldstmt1, oldstmt2, oldstmt3, oldstmt4, oldstmt5, oldstmt6]\ncodelocs = Int[1, 2, 3, 4, 5, 6]\n\nLet\'s also say that for our stmtcount returns 2 for stmtcount(oldstmt2, 2), returns 3 for stmtcount(oldstmt5, 5), and returns nothing for all other inputs. From this setup, we can think of code/codelocs being modified in the following manner:\n\nnewstmts2 = newstmts(oldstmt2, 2)\nnewstmts5 = newstmts(oldstmt5, 5)\ncode = Any[oldstmt1,\n           newstmts2[1], newstmts2[2],\n           oldstmt3, oldstmt4,\n           newstmts5[1], newstmts5[2], newstmts5[3],\n           oldstmt6]\ncodelocs = Int[1, 2, 2, 3, 4, 5, 5, 5, 6]\n\nSee also: replace_match!, is_ir_element\n\n\n\n\n\n"
},

{
    "location": "api.html#Cassette.is_ir_element",
    "page": "Cassette API Documentation",
    "title": "Cassette.is_ir_element",
    "category": "function",
    "text": "is_ir_element(x, y, code::Vector)\n\nReturn true if x === y or if x is an SSAValue such that is_ir_element(code[x.id], y, code) is true.\n\nSee also: replace_match!, insert_statements!\n\n\n\n\n\n"
},

{
    "location": "api.html#Cassette.OVERDUB_CONTEXT_NAME",
    "page": "Cassette API Documentation",
    "title": "Cassette.OVERDUB_CONTEXT_NAME",
    "category": "constant",
    "text": "Cassette.OVERDUB_CONTEXT_NAME\n\nThe variable name bound to overdub\'s Context argument in its @generated method definition.\n\nThis binding can be used to manually reference/destructure overdub arguments within Expr thunks emitted by user-provided passes.\n\nSee also: OVERDUB_ARGUMENTS_NAME, @pass, overdub\n\n\n\n\n\n"
},

{
    "location": "api.html#Cassette.OVERDUB_ARGUMENTS_NAME",
    "page": "Cassette API Documentation",
    "title": "Cassette.OVERDUB_ARGUMENTS_NAME",
    "category": "constant",
    "text": "Cassette.OVERDUB_ARGUMENTS_NAME\n\nThe variable name bound to overdub\'s tuple of non-Context arguments in its @generated method definition.\n\nThis binding can be used to manually reference/destructure overdub arguments within Expr thunks emitted by user-provided passes.\n\nSee also: OVERDUB_CONTEXT_NAME, @pass, overdub\n\n\n\n\n\n"
},

{
    "location": "api.html#Cassette.Reflection",
    "page": "Cassette API Documentation",
    "title": "Cassette.Reflection",
    "category": "type",
    "text": "Cassette.Reflection\n\nA struct representing the information retrieved via Cassette.reflect.\n\nA Reflection is essentially just a convenient bundle of information about a specific method invocation.\n\nFields\n\nsignature: the invocation signature (in Tuple{...} type form) for the invoked method.\nmethod: the Method object associated with the invoked method.\nstatic_params: a Vector representing the invoked method\'s static parameter list.\ncode_info: the CodeInfo object associated with the invoked method.\n\n\n\n\n\n"
},

{
    "location": "api.html#Cassette.tag",
    "page": "Cassette API Documentation",
    "title": "Cassette.tag",
    "category": "function",
    "text": "tag(value, context::Context, metadata = Cassette.NoMetaData())\n\nReturn value tagged w.r.t. context, optionally associating metadata with the returned Tagged instance.\n\nAny provided metadata must obey the type constraints determined by Cassette\'s metadatatype method.\n\nNote that hastagging(typeof(context)) must be true for a value to be tagged w.r.t. to context.\n\nSee also: untag, enabletagging, hastagging\n\n\n\n\n\n"
},

{
    "location": "api.html#Cassette.untag",
    "page": "Cassette API Documentation",
    "title": "Cassette.untag",
    "category": "function",
    "text": "untag(x, context::Context)\n\nReturn x untagged w.r.t. context if istagged(x, context), otherwise return x directly.\n\nIn other words, untag(tag(x, context), context) === x is always true.\n\nIf !istagged(x, context), then untag(x, context) === x is true.\n\nSee also: tag, istagged\n\n\n\n\n\n"
},

{
    "location": "api.html#Cassette.untagtype",
    "page": "Cassette API Documentation",
    "title": "Cassette.untagtype",
    "category": "function",
    "text": "untagtype(::Type{T}, ::Type{C<:Context})\n\nReturn typeof(untag(::T, ::C)).\n\nIn other words, untagtype(typeof(tag(x, context)), typeof(context)) === typeof(x) is always true.\n\nIf !istaggedtype(T, C), then untagtype(T, C) === T is true.\n\n\n\n\n\n"
},

{
    "location": "api.html#Cassette.metadata",
    "page": "Cassette API Documentation",
    "title": "Cassette.metadata",
    "category": "function",
    "text": "metadata(x, context::Context)\n\nReturn the metadata attached to x if hasmetadata(x, context), otherwise return Cassette.NoMetaData().\n\nIn other words, metadata(tag(x, context, m)), context) === m is always true.\n\nIf !hasmetadata(x, context), then metadata(x, context) === Cassette.NoMetaData() is true.\n\n\n\n\n\n"
},

{
    "location": "api.html#Cassette.metadatatype",
    "page": "Cassette API Documentation",
    "title": "Cassette.metadatatype",
    "category": "function",
    "text": "metadatatype(::Type{<:Context}, ::Type{T})\n\nOverload this Cassette method w.r.t. a given context to define the type of metadata that can be tagged to values of type T within that context.\n\nBy default, this method is set such that associating metadata with any tagged value is disallowed.\n\nCassette uses metadatatype to statically compute a context-specific metadata type hiearchy for all tagged values within overdubbed programs. To gain a mental model for this mechanism, consider a simple struct definition as follows:\n\nstruct Foo\n    x::Int\n    y::Complex{Int}\nend\n\nNow, Cassette can use metadatatype to determine type constraints for metadata structures associated with tagged values of type Foo. In psuedo-Julia-code, these metadata structures might look something like the following for Foo:\n\nstruct IntMeta\n    data::metadatatype(Ctx, Int)\n    meta::Cassette.NoMetaMeta\nend\n\nstruct ComplexIntMeta\n    data::metadatatype(Ctx, Complex{Int})\n    meta::NamedTuple{(:re,:im),Tuple{IntMeta,IntMeta}}\nend\n\nstruct FooMeta\n    data::metadatatype(Ctx, Foo)\n    meta::NamedTuple{(:x,:y),Tuple{IntMeta,ComplexIntMeta}\nend\n\nExamples\n\njulia> Cassette.@context Ctx;\n\n# any value of type `Number` can now be tagged with metadata of type `Number`\njulia> Cassette.metadatatype(::Type{<:Ctx}, ::Type{<:Number}) = Number\n\n# any value of type `T<:Number` can now be tagged with metadata of type `T`\njulia> Cassette.metadatatype(::Type{<:Ctx}, ::Type{T}) where {T<:Number} = T\n\n# any value of type `T<:Number` can now be tagged with metadata of type `promote_type(T, M)`\n# where `M` is the type of the trace-local metadata associated with the context\njulia> Cassette.metadatatype(::Type{<:Ctx{M}}, ::Type{T}) where {M<:Number,T<:Number} = promote_type(T, M)\n\n\n\n\n\n"
},

{
    "location": "api.html#Cassette.hasmetadata",
    "page": "Cassette API Documentation",
    "title": "Cassette.hasmetadata",
    "category": "function",
    "text": "hasmetadata(x, context::Context)\n\nReturn true if !isa(metadata(x, context), Cassette.NoMetaData), return false otherwise.\n\nIn other words, hasmetadata(tag(x, context, m), context) is always true and hasmetadata(tag(x, context), context) is always false.\n\nSee also: metadata\n\n\n\n\n\n"
},

{
    "location": "api.html#Cassette.istagged",
    "page": "Cassette API Documentation",
    "title": "Cassette.istagged",
    "category": "function",
    "text": "istagged(x, context::Context)\n\nReturn true if x is tagged w.r.t. context, return false otherwise.\n\nIn other words, istagged(tag(x, context), context) is always true.\n\nSee also: tag, istaggedtype\n\n\n\n\n\n"
},

{
    "location": "api.html#Cassette.istaggedtype",
    "page": "Cassette API Documentation",
    "title": "Cassette.istaggedtype",
    "category": "function",
    "text": "istaggedtype(::Type{T}, ::Type{C<:Context})\n\nReturn typeof(istagged(::T, ::C)).\n\nIn other words, istaggedtype(typeof(tag(x, context)), typeof(context)) is always true.\n\nSee also: tag, istagged\n\n\n\n\n\n"
},

{
    "location": "api.html#Cassette-API-Documentation-1",
    "page": "Cassette API Documentation",
    "title": "Cassette API Documentation",
    "category": "section",
    "text": "CurrentModule = CassetteCassette.Context\nCassette.similarcontext\nCassette.disablehooks\nCassette.enabletagging\nCassette.hastagging\nCassette.@contextCassette.overdub\nCassette.@overdub\nCassette.recurse\nCassette.prehook\nCassette.posthook\nCassette.fallback\nCassette.canrecurseCassette.@pass\nCassette.replace_match!\nCassette.insert_statements!\nCassette.is_ir_element\nCassette.OVERDUB_CONTEXT_NAME\nCassette.OVERDUB_ARGUMENTS_NAME\nCassette.ReflectionCassette.tag\nCassette.untag\nCassette.untagtype\nCassette.metadata\nCassette.metadatatype\nCassette.hasmetadata\nCassette.istagged\nCassette.istaggedtype"
},

{
    "location": "relatedwork.html#",
    "page": "Related Work",
    "title": "Related Work",
    "category": "page",
    "text": ""
},

{
    "location": "relatedwork.html#Related-Work-1",
    "page": "Related Work",
    "title": "Related Work",
    "category": "section",
    "text": "Cassette draws inspiration from a bunch of different work (in no particular order):Aspect-Oriented Programming: https://en.wikipedia.org/wiki/Aspect-oriented_programming\nXRay for LLVM: https://ai.google/research/pubs/pub45287\nMultimethods for C++: http://www.stroustrup.com/multimethods.pdf\nMap-Closure for Scheme: http://www.bcl.hamilton.ie/~qobi/map-closure/\nStalingrad Compiler for VLAD: https://docs.lib.purdue.edu/cgi/viewcontent.cgi?article=1368&context=ecetr\nCompiling to Categories for Haskell: http://conal.net/papers/compiling-to-categories/\nThe Checker Framework for Java: https://checkerframework.org/\nContext-Oriented Programming (this is not strictly what Cassette implements, despite the naming similarities): http://www.jot.fm/issues/issue200803/article4/\nJameson Nash\'s Brain: https://github.com/vtjnash"
},

]}
