# Why Cassette?

Due to its multiple dispatch, method overloading and metaprogramming capabilities, Julia has turned out to be a great language for implementing various forms of "nonstandard execution", enabling generically-written Julia programs to be executed with new features simply by passing new types through the code. Of course, many other languages allow this kind of polymorphism, but Julia is unique in its ability to facilitate these kinds of techniques with surprising elegance and performance. These key advantages have attracted thriving, state-of-the-art Julia implementations of numerical computing tools in regimes such as mathematical optimization and parallel programming.

Over time, however, these amazing Julia tools have run up against the boundaries of purely overloading-based approaches to nonstandard execution. Cassette is an attempt to push these boundaries by giving Julia package developers the ability to fundamentally extend the language for their own purposes.

Here are some specific limitations that might be overcome by using Cassette:

- Overloading-based approaches to nonstandard execution are ultimately thwarted by dispatch and/or structural type constraints in non-generic target programs.

- Proper usage of overloading-based nonstandard execution tools require proper genericity criteria, i.e. answering "what weird subset of Julia do I really support?". These criteria are often difficult to correctly define and test against for both experienced developers and end-users.

- Not all relevant Julia language mechanisms are fully exposed/interceptable via method overloading (e.g. control flow, literals, bindings). Additionally, many different compiler passes require access to a wider scope of information than is locally available at individual method callsites (e.g. graph-based transformations).

- It is often difficult to fully resolve the kinds of ambiguities that naturally result from the composition of several multiple-dispatch-based tools. Brute-force solutions end up generating an excessive number of methods and are often either code-load-order dependent or incomplete (or even both). Julia's promotion mechanism resolves this problem when the involved types are efficiently convertible to one another, but this is not often the case in practice (e.g. subtypes of `AbstractArray`).

# No, why the *name* "Cassette"?

Cassette was originally motivated by the need for better language-level support for automatic differentiation, where the data structure used to accumulate program traces is often called a "tape" (since, of course, program traces really *were* stored on magnetic tapes back in the day).
