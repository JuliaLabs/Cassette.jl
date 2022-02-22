using Cassette
using Documenter

makedocs(;
    modules = [Cassette],
    authors = "Jarrett Revels",
    sitename = "Cassette",
    format = Documenter.HTML(;
        prettyurls = get(ENV, "CI", "false") == "true",
        ansicolor = true,
    ),
    pages = Any[
        "Introduction" => "index.md",
        "Disclaimers" => "disclaimers.md",
        "Why Cassette?" => "whycassette.md",
        "The Overdubbing Mechanism" => "overdub.md",
        "Contextual Dispatch" => "contextualdispatch.md",
        "Contextual Compiler Pass Injection" => "contextualpass.md",
        "Contextual Tagging of Values" => "contextualtagging.md",
        "Cassette API Documentation" => "api.md",
        "Related Work" => "relatedwork.md",
    ],
    strict = false,
)

deploydocs(; repo = "github.com/jrevels/Cassette.jl",
             push_preview = true,
             )
