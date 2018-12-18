using Documenter, Cassette

makedocs(modules=[Cassette],
         doctest = false,
         format = :html,
         sitename = "Cassette",
         pages = ["Introduction" => "index.md",
                  "Disclaimers" => "disclaimers.md",
                  "Why Cassette?" => "whycassette.md",
                  "The Overdubbing Mechanism" => "overdub.md",
                  "Contextual Dispatch" => "contextualdispatch.md",
                  "Contextual Compiler Pass Injection" => "contextualpass.md",
                  "Contextual Tagging of Values" => "contextualtagging.md",
                  "Cassette API Documentation" => "api.md",
                  "Related Work" => "relatedwork.md"])

deploydocs(repo = "github.com/jrevels/Cassette.jl.git",
           osname = "linux",
           julia = "1.0",
           target = "build",
           deps = nothing,
           make = nothing)
