using Documenter, Cassette

makedocs(modules=[Cassette],
         doctest = false,
         format = :html,
         sitename = "Cassette",
         pages = ["Introduction" => "index.md",
                  "Disclaimers" => "disclaimers.md",
                  "Cassette API Documentation" => "api.md",
                  "Related Work" => "relatedwork.md"])

deploydocs(repo = "github.com/jrevels/Cassette.jl.git",
           osname = "linux",
           julia = "0.7",
           target = "build",
           deps = nothing,
           make = nothing)
