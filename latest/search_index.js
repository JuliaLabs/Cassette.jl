var documenterSearchIndex = {"docs": [

{
    "location": "introduction.html#",
    "page": "Introduction",
    "title": "Introduction",
    "category": "page",
    "text": ""
},

{
    "location": "introduction.html#Introduction-1",
    "page": "Introduction",
    "title": "Introduction",
    "category": "section",
    "text": "Hello! Welcome to Cassette\'s documentation.For an initial overview of Cassette, please see the README.Otherwise, feel free to peruse available documentation via the sidebar.It is recommended that all Cassette users read the documentation in its entirety.It is highly recommended that all Cassette users at least read the Disclaimers section before attempting to use Cassette."
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
    "text": "Cassette can be a powerful tool for extending the Julia language, but it functions equally well as a loaded foot-gun. Here are some things one should know before using Cassette:Cassette, its API, and its documentation targets Julia package developers and/or those   interested in doing compiler research using Julia. Cassette users are expected to have   a working understanding of Julia\'s compiler, type system and metaprogramming facilities.\nPractical usage of Cassette will quite likely reveal both performance and correctness   bugs caused by either Cassette or Julia itself (especially in this early stage of   development). This is especially likely when doing nested overdubbing, or composing   multiple Cassette contexts. Please file issues on the Cassette and/or Julia issue   tracker where appropriate.\nFor now, each individual version of Cassette can technically only support a single   specific version of Julia at a time; differing by even a patch version could   (theoretically) break Cassette entirely. This is because Cassette interacts closely with   Julia internals that traditionally have license to change between patch versions. In the   future, the planned solution for this issue is for Julia itself to perform   reverse-dependency testing against Cassette\'s tests, such that breaking changes to Julia   compiler are discovered and fixed immediately.\nThe performance of Cassette\'s implementation of the contextual tagging system heavily   depends on compiler improvements planned for the Julia 1.x release cycle. In theory,   given these compiler improvements, the contextual tagging system could achieve   performance comparable to alternatives (e.g. ForwardDiff\'s dual number implementation),   but for now, the contextual tagging system is quite slow and allocation-heavy.\nCassette enables interaction with many parts of the Julia compiler, a lot of which are   undocumented or sparsely documented. It is extremely easy to accidentally implement a   pass that breaks internal compiler assumptions in some subtle way. If (and when) you   run into these scenarios, it would be helpful to open an issue on the Julia issue tracker   proposing better documentation (or even a stable API) for a specific part of the   compiler.\nDue to limitations of Julia\'s current world-age mechanism, Cassette exhibits a similar   recompilation problem to the famous   JuliaLang/julia#265 (see   jrevels/Cassette.jl#6 for details). In   order to resolve this issue, an update to Julia\'s world-age mechanism is planned for the   Julia 1.x release cycle."
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
    "text": "CurrentModule = CassetteThe central mechanism that drives Cassette usage is called the \"overdubbing\" mechanism.A naive usage of this mechanism looks like this:julia> using Cassette\n\njulia> Cassette.@context Ctx\nCassette.Context{nametype(Ctx),M,P,T,B} where B<:Union{Nothing, IdDict{Module,Dict{Symbol,BindingMeta}}} where P<:Cassette.AbstractPass where T<:Union{Nothing, Tag} where M\n\njulia> Cassette.overdub(Ctx(), /, 1, 2)\n0.5Okay - what did we actually just do here? From the output, it seems like we just computed 1/2...and indeed, we did! In reality, however, Cassette was doing a lot of work behind the scenes during this seemingly simple calculation. Let\'s drill down further into this example to see what\'s actually going on.First, we define a new Context type alias called Ctx via the @context macro. In practical usage, one normally defines one or more contexts specific to one\'s application of Cassette. Here, we just made a dummy one for illustrative purposes.Next, we \"overdubbed\" a call to 1/2 w.r.t. Ctx() using the overdub function. To get a sense of what that means, let\'s look at the lowered IR for the original call:julia> @code_lowered 1/2\nCodeInfo(\n59 1 ─ %1 = (Base.float)(x)\n   │   %2 = (Base.float)(y)\n   │   %3 = %1 / %2\n   └──      return %3\n)And now let\'s look at lowered IR for the call to overdub(Ctx(), /, 1, 2)julia> @code_lowered Cassette.overdub(Ctx(), /, 1, 2)\nCodeInfo(\n59 1 ─       #self# = (Core.getfield)(##overdub_arguments#369, 1)\n   │         x = (Core.getfield)(##overdub_arguments#369, 2)\n   │         y = (Core.getfield)(##overdub_arguments#369, 3)\n   │         (Cassette.prehook)(##overdub_context#368, Base.float, x)\n   │         ##overdub_tmp#370 = (Cassette.execute)(##overdub_context#368, Base.float, x)\n   │   %6  = ##overdub_tmp#370 isa Cassette.OverdubInstead\n   └──       goto #3 if not %6\n   2 ─       ##overdub_tmp#370 = (Cassette.overdub)(##overdub_context#368, Base.float, x)\n   3 ─       (Cassette.posthook)(##overdub_context#368, ##overdub_tmp#370, Base.float, x)\n   │   %10 = ##overdub_tmp#370\n   │         (Cassette.prehook)(##overdub_context#368, Base.float, y)\n   │         ##overdub_tmp#370 = (Cassette.execute)(##overdub_context#368, Base.float, y)\n   │   %13 = ##overdub_tmp#370 isa Cassette.OverdubInstead\n   └──       goto #5 if not %13\n   4 ─       ##overdub_tmp#370 = (Cassette.overdub)(##overdub_context#368, Base.float, y)\n   5 ─       (Cassette.posthook)(##overdub_context#368, ##overdub_tmp#370, Base.float, y)\n   │   %17 = ##overdub_tmp#370\n   │         (Cassette.prehook)(##overdub_context#368, Base.:/, %10, %17)\n   │         ##overdub_tmp#370 = (Cassette.execute)(##overdub_context#368, Base.:/, %10, %17)\n   │   %20 = ##overdub_tmp#370 isa Cassette.OverdubInstead\n   └──       goto #7 if not %20\n   6 ─       ##overdub_tmp#370 = (Cassette.overdub)(##overdub_context#368, Base.:/, %10, %17)\n   7 ─       (Cassette.posthook)(##overdub_context#368, ##overdub_tmp#370, Base.:/, %10, %17)\n   │   %24 = ##overdub_tmp#370\n   └──       return %24\n)There\'s obviously a lot more going on here than in the lowered IR for 1/2, but if you squint, you might notice that the overdubbed IR is actually the original IR with a special transformation applied to it. Specifically, the overdubbed IR is the lowered IR for the given function call with all internal method invocations of the form f(args...) replaced by statements similar to the following:begin\n    Cassette.prehook(context, f, args...)\n    tmp = Cassette.execute(context, f, args...)\n    tmp = isa(tmp, Cassette.OverdubInstead) ? overdub(context, f, args...) : tmp\n    Cassette.posthook(context, tmp, f, args...)\n    tmp\nendIt is here that we experience our first bit of overdubbing magic: for every method call in the overdubbed trace, we obtain a bunch of extra overloading points that we didn\'t have before! In the following section on contextual dispatch, we\'ll explore how prehook, posthook, execute, and more can all be overloaded to add new contextual behaviors to overdubbed programs.In the meantime, we should clarify how overdub is achieving this feat. Let\'s start by examining a \"psuedo-implementation\" of overdub:@generated function overdub(context::C, args...) where C<:Context\n    reflection = Cassette.reflect(args)\n    if isa(reflection, Cassette.Reflection)\n        Cassette.overdub_pass!(reflection, C)\n        return reflection.code_info\n    else\n        return :(Cassette.fallback(context, args...))\n    end\nendAs you can see, overdub is a @generated function, and thus returns its own method body computed from the run-time types of its inputs. To compute this method body, however, overdub is doing something quite special.First, via Cassette.reflect, overdub asks Julia\'s compiler to provide it with a bunch of information about the original method call as specified by args, including the method\'s lowered IR. The result of this query is reflection, which is a Cassette.Reflection object if the compiler found lowered IR for args and nothing otherwise (e.g. if args specifies a built-in call like getfield whose implementation is not, itself, Julia code). Then, for the former case, we execute a pass over the lowered IR (Cassette.overdub_pass!) to perform the previously presented transformation, and then return the resulting CodeInfo object directly from our generator. Otherwise, if no lowered IR is available, we simply call the context\'s fallback method (which, by default, simply calls the provided function)."
},

{
    "location": "contextualdispatch.html#",
    "page": "Contextual Dispatch",
    "title": "Contextual Dispatch",
    "category": "page",
    "text": "Documentation Coming Soon!"
},

{
    "location": "contextualtagging.html#",
    "page": "Contextual Tagging of Values",
    "title": "Contextual Tagging of Values",
    "category": "page",
    "text": "Documentation Coming Soon!"
},

{
    "location": "contextualpass.html#",
    "page": "Contextual Compiler Pass Injection",
    "title": "Contextual Compiler Pass Injection",
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
    "text": "Context{N<:Cassette.AbstractContextName,\n        M<:Any,\n        P<:Cassette.AbstractPass,\n        T<:Union{Nothing,Cassette.Tag},\n        B<:Union{Nothing,Cassette.BindingMetaDictCache}}\n\nA type representing a Cassette execution context. This type is normally interacted with through type aliases constructed via Cassette.@context:\n\njulia> Cassette.@context MyCtx\nCassette.Context{nametype(MyCtx),M,P,T,B} where B<:Union{Nothing,IdDict{Module,Dict{Symbol,BindingMeta}}}\n                                          where P<:Cassette.AbstractPass\n                                          where T<:Union{Nothing,Tag}\n                                          where M\n\nConstructors\n\nGiven a context type alias named e.g. MyCtx, an instance of the type can be constructed via:\n\nMyCtx(; metadata = nothing, pass = Cassette.NoPass())\n\nTo construct a new context instance using an existing context instance as a template, see the similarcontext function.\n\nTo enable contextual tagging for a given context instance, see the enabletagging function.\n\nFields\n\nname::N<:Cassette.AbstractContextName: a parameter used to disambiguate different   contexts for overloading purposes (e.g. distinguishes MyCtx from other Context type   aliases).\nmetadata::M<:Any: trace-local metadata as provided to the context constructor\npass::P<:Cassette.AbstractPass: the Cassette pass that will be applied to all method   bodies encountered during contextual execution (see the @pass macro for details).\ntag::T<:Union{Nothing,Tag}: the tag object that is attached to values when they are   tagged w.r.t. the context instance\nbindingscache::B<:Union{Nothing,BindingMetaDictCache}}: storage for metadata associated   with tagged module bindings\n\n\n\n\n\n"
},

{
    "location": "api.html#Cassette.similarcontext",
    "page": "Cassette API Documentation",
    "title": "Cassette.similarcontext",
    "category": "function",
    "text": "similarcontext(context::Context;\n               metadata = context.metadata,\n               pass = context.pass,\n               tag = context.tag,\n               bindingscache = context.bindingscache)\n\nReturn a copy of the given context, replacing field values in the returned instance with those provided via the keyword arguments.\n\n\n\n\n\n"
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
    "text": "Cassette.@context Ctx\n\nDefine a new Cassette context type with the name Ctx. In reality, Ctx is simply a type alias for Cassette.Context{Cassette.nametype(Ctx)}.\n\nNote that Cassette.execute is automatically overloaded w.r.t. Ctx to define several primitives by default. A full list of these default primitives can be obtained by running:\n\nmethods(Cassette.execute, (Ctx, Vararg{Any}))\n\nNote also that many of the default primitives\' signatures only match when contextual tagging is enabled.\n\nSee also: Context\n\n\n\n\n\n"
},

{
    "location": "api.html#Cassette.overdub",
    "page": "Cassette API Documentation",
    "title": "Cassette.overdub",
    "category": "function",
    "text": "overdub(context::Context, f, args...)\n\nExecute f(args...) overdubbed with respect to context.\n\nMore specifically, execute f(args...), but with every internal method invocation g(x...) replaced by statements similar to the following:\n\nbegin\n    prehook(context, g, x...)\n    tmp = execute(context, g, x...)\n    tmp = isa(tmp, Cassette.OverdubInstead) ? overdub(context, g, x...) : tmp\n    posthook(context, tmp, g, x...)\n    tmp\nend\n\nIf Cassette cannot retrieve lowered IR for the method body of f(args...) (as determined by canoverdub(context, f, args...)), then overdub(context, f, args...) will directly translate to a call to fallback(context, f, args...).\n\nAdditionally, for every method body encountered in execute trace, apply the compiler pass associated with context if one exists. Note that this user-provided pass is performed on the method IR before method invocations are transformed into the form specified above. See the @pass macro for further details.\n\nIf Cassette.hastagging(typeof(context)), then a number of additional passes are run in order to accomodate tagged value propagation:\n\nExpr(:new) is replaced with a call to Cassette.tagged_new\nconditional values passed to Expr(:gotoifnot) are untagged\narguments to Expr(:foreigncall) are untagged\nload/stores to external module bindings are intercepted by the tagging system\n\n\n\n\n\n"
},

{
    "location": "api.html#Cassette.@overdub",
    "page": "Cassette API Documentation",
    "title": "Cassette.@overdub",
    "category": "macro",
    "text": "Cassette.@overdub(ctx, expression)\n\nA convenience macro for executing expression within the context ctx. This macro roughly expands to Cassette.overdub(ctx, () -> expression).\n\nSee also: overdub\n\n\n\n\n\n"
},

{
    "location": "api.html#Cassette.prehook",
    "page": "Cassette API Documentation",
    "title": "Cassette.prehook",
    "category": "function",
    "text": "prehook(context::Context, f, args...)\n\nOverload this Cassette method w.r.t. a given context in order to define a new contextual prehook for that context.\n\nTo understand when/how this method is called, see the documentation for overdub.\n\nInvoking prehook is a no-op by default (it immediately returns nothing).\n\nSee also: overdub, posthook, execute, fallback\n\nExamples\n\nSimple trace logging:\n\njulia> Cassette.@context PrintCtx;\n\njulia> Cassette.prehook(::PrintCtx, f, args...) = println(f, args)\n\njulia> Cassette.overdub(PrintCtx(), /, 1, 2)\nfloat(1,)\nAbstractFloat(1,)\nFloat64(1,)\nsitofp(Float64, 1)\nfloat(2,)\nAbstractFloat(2,)\nFloat64(2,)\nsitofp(Float64, 2)\n/(1.0, 2.0)\ndiv_float(1.0, 2.0)\n0.5\n\nCounting the number of method invocations with one or more arguments of a given type:\n\njulia> mutable struct Count{T}\n           count::Int\n       end\n\njulia> Cassette.@context CountCtx;\n\njulia> Cassette.prehook(ctx::CountCtx{Count{T}}, f, arg::T, args::T...) where {T} = (ctx.metadata.count += 1)\n\n# count the number of calls of the form `f(::Float64, ::Float64...)`\njulia> ctx = CountCtx(metadata = Count{Float64}(0));\n\njulia> Cassette.overdub(ctx, /, 1, 2)\n0.5\n\njulia> ctx.metadata.count\n2\n\n\n\n\n\n"
},

{
    "location": "api.html#Cassette.posthook",
    "page": "Cassette API Documentation",
    "title": "Cassette.posthook",
    "category": "function",
    "text": "posthook(context::Context, output, f, args...)\n\nOverload this Cassette method w.r.t. a given context in order to define a new contextual posthook for that context.\n\nTo understand when/how this method is called, see the documentation for overdub.\n\nInvoking posthook is a no-op by default (it immediately returns nothing).\n\nSee also: overdub, prehook, execute, fallback\n\nExamples\n\nSimple trace logging:\n\njulia> Cassette.@context PrintCtx;\n\njulia> Cassette.posthook(::PrintCtx, output, f, args...) = println(output, \" = \", f, args)\n\njulia> Cassette.overdub(PrintCtx(), /, 1, 2)\n1.0 = sitofp(Float64, 1)\n1.0 = Float64(1,)\n1.0 = AbstractFloat(1,)\n1.0 = float(1,)\n2.0 = sitofp(Float64, 2)\n2.0 = Float64(2,)\n2.0 = AbstractFloat(2,)\n2.0 = float(2,)\n0.5 = div_float(1.0, 2.0)\n0.5 = /(1.0, 2.0)\n0.5\n\nAccumulate the sum of all numeric scalar outputs encountered in the trace:\n\njulia> mutable struct Accum\n           x::Number\n       end\n\njulia> Cassette.@context AccumCtx;\n\njulia> Cassette.posthook(ctx::AccumCtx{Accum}, out::Number, f, args...) = (ctx.metadata.x += out)\n\njulia> ctx = AccumCtx(metadata = Accum(0));\n\njulia> Cassette.overdub(ctx, /, 1, 2)\n0.5\n\njulia> ctx.metadata.x\n13.0\n\n\n\n\n\n"
},

{
    "location": "api.html#Cassette.execute",
    "page": "Cassette API Documentation",
    "title": "Cassette.execute",
    "category": "function",
    "text": "execute(context::Context, f, args...)\n\nOverload this Cassette method w.r.t. a given context in order to define a new contextual execution primitive for that context.\n\nTo understand when/how this method is called, see the documentation for overdub.\n\nInvoking execute immediately returns Cassette.OverdubInstead() by default.\n\nSee also: overdub, prehook, posthook, fallback\n\n\n\n\n\n"
},

{
    "location": "api.html#Cassette.fallback",
    "page": "Cassette API Documentation",
    "title": "Cassette.fallback",
    "category": "function",
    "text": "fallback(context::Context, f, args...)\n\nOverload this Cassette method w.r.t. a given context in order to define a new contextual execution fallback for that context.\n\nTo understand when/how this method is called, see the documentation for overdub and canoverdub.\n\nBy default, invoking fallback(context, f, args...) will simply call f(args...) (with all arguments automatically untagged, if hastagging(typeof(context))).\n\nSee also:  canoverdub, overdub, execute, prehook, posthook\n\n\n\n\n\n"
},

{
    "location": "api.html#Cassette.canoverdub",
    "page": "Cassette API Documentation",
    "title": "Cassette.canoverdub",
    "category": "function",
    "text": "canoverdub(context::Context, f, args...)\n\nReturn true if f(args...) has a lowered IR representation that Cassette can overdub, return false otherwise.\n\nAlternatively, but equivalently:\n\nReturn false if overdub(context, f, args...) directly translates to fallback(context, f, args...), return true otherwise.\n\nNote that unlike execute, fallback, etc., this function is not intended to be overloaded.\n\nSee also:  overdub, fallback, execute\n\n\n\n\n\n"
},

{
    "location": "api.html#Cassette.@pass",
    "page": "Cassette API Documentation",
    "title": "Cassette.@pass",
    "category": "macro",
    "text": "Cassette.@pass transform\n\nReturn a Cassette pass that can be provided to the Context constructor\'s pass keyword argument in order to apply transform to the lowered IR representations of all methods invoked during contextual execution.\n\ntransform must be a Julia object that is callable with the following signature:\n\ntransform(::Type{<:Context}, signature::Type{Tuple{...}}, method_body::CodeInfo)::CodeInfo\n\nNote that the @pass macro expands to an eval call and thus should only be called at top-level. Furthermore, to avoid world-age issues, transform should not be overloaded after it has been registered with @pass.\n\nNote also that transform should be \"relatively pure.\" More specifically, Julia\'s compiler has license to apply transform multiple times, even if only compiling a single method invocation once. Thus, it is required that transform always return a generally \"equivalent\" CodeInfo for a given context, method body, and signature.\n\nTwo special Expr heads are available to Cassette pass authors that are not normally valid in Julia IR. Exprs with these heads can be used to interact with the downstream built-in Cassette passes that consume them.\n\n:nooverdub: Wrap an Expr with this head value around the first argument in an   Expr(:call) to tell downstream built-in Cassette passes not to overdub that call. For   example, Expr(:call, Expr(:nooverdub, GlobalRef(MyModule, :myfunc)), args...).\n:contextslot: Cassette will replace any Expr(:contextslot) with the actual SlotNumber   corresponding to the context object associated with the execution trace. For example, one   could construct an IR element that accesses the context\'s metadata field by emitting:   Expr(:call, Expr(:nooverdub, GlobalRef(Core, :getfield)), Expr(:contextslot), QuoteNode(:metadata))\n\nCassette provides a few IR-munging utility functions of interest to pass authors: insert_statements!, replace_match!\n\nSee also: Context, overdub\n\n\n\n\n\n"
},

{
    "location": "api.html#Cassette.replace_match!",
    "page": "Cassette API Documentation",
    "title": "Cassette.replace_match!",
    "category": "function",
    "text": "replace_match!(replace, ismatch, x)\n\nReturn x with all subelements y replaced with replace(y) if ismatch(y). If !ismatch(y), but y is of type Expr, Array, or SubArray, then replace y in x with replace_match!(replace, ismatch, y).\n\nGenerally, x should be of type Expr, Array, or SubArray.\n\nNote that this function modifies x (and potentially its subelements) in-place.\n\nSee also: insert_statements!\n\n\n\n\n\n"
},

{
    "location": "api.html#Cassette.insert_statements!",
    "page": "Cassette API Documentation",
    "title": "Cassette.insert_statements!",
    "category": "function",
    "text": "insert_statements!(code::Vector, codelocs::Vector, stmtcount, newstmts)\n\nFor every statement stmt at position i in code for which stmtcount(stmt, i) returns an Int, remove stmt, and in its place, insert the statements returned by newstmts(stmt, i). If stmtcount(stmt, i) returns nothing, leave stmt alone.\n\nFor every insertion, all downstream SSAValues, label indices, etc. are incremented appropriately according to number of inserted statements.\n\nProper usage of this function dictates that following properties hold true:\n\ncode is expected to be a valid value for the code field of a CodeInfo object.\ncodelocs is expected to be a valid value for the codelocs field of a CodeInfo object.\nnewstmts(stmt, i) should return a Vector of valid IR statements.\nstmtcount and newstmts must obey stmtcount(stmt, i) == length(newstmts(stmt, i)) if   isa(stmtcount(stmt, i), Int).\n\nTo gain a mental model for this function\'s behavior, consider the following scenario. Let\'s say our code object contains several statements:\n\ncode = Any[oldstmt1, oldstmt2, oldstmt3, oldstmt4, oldstmt5, oldstmt6]\ncodelocs = Int[1, 2, 3, 4, 5, 6]\n\nLet\'s also say that for our stmtcount returns 2 for stmtcount(oldstmt2, 2), returns 3 for stmtcount(oldstmt5, 5), and returns nothing for all other inputs. From this setup, we can think of code/codelocs being modified in the following manner:\n\nnewstmts2 = newstmts(oldstmt2, 2)\nnewstmts5 = newstmts(oldstmt5, 5)\ncode = Any[oldstmt1,\n           newstmts2[1], newstmts2[2],\n           oldstmt3, oldstmt4,\n           newstmts5[1], newstmts5[2], newstmts5[3],\n           oldstmt6]\ncodelocs = Int[1, 2, 2, 3, 4, 5, 5, 5, 6]\n\nSee also: replace_match!\n\n\n\n\n\n"
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
    "text": "CurrentModule = CassetteCassette.Context\nCassette.similarcontext\nCassette.enabletagging\nCassette.hastagging\nCassette.@contextCassette.overdub\nCassette.@overdub\nCassette.prehook\nCassette.posthook\nCassette.execute\nCassette.fallback\nCassette.canoverdubCassette.@pass\nCassette.replace_match!\nCassette.insert_statements!Cassette.tag\nCassette.untag\nCassette.untagtype\nCassette.metadata\nCassette.metadatatype\nCassette.hasmetadata\nCassette.istagged\nCassette.istaggedtype"
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
    "text": "Cassette draws inspiration from a bunch of different work (in no particular order):Aspect-Oriented Programming: https://en.wikipedia.org/wiki/Aspect-oriented_programming\nXRay for LLVM: https://ai.google/research/pubs/pub45287\nMultimethods for C++: http://www.stroustrup.com/multimethods.pdf\nMap-Closure for Scheme: http://www.bcl.hamilton.ie/~qobi/map-closure/\nCompiling to Categories for Haskell: http://conal.net/papers/compiling-to-categories/\nThe Checker Framework for Java: https://checkerframework.org/\nContext-Oriented Programming (this is not strictly what Cassette implements, despite the naming similarities): http://www.jot.fm/issues/issue200803/article4/\nJameson Nash\'s Brain: https://github.com/vtjnash"
},

]}
