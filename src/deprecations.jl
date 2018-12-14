Sorry_OverdubInstead_Is_Deprecated() = error("Usage of `OverdubInstead` should be replaced by `Cassette.recurse`; see `Cassette.recurse` for details.")

Base.@deprecate_binding(OverdubInstead, Sorry_OverdubInstead_Is_Deprecated, false,
string("; the `overdub` <--> `execute` cycle has been removed from Cassette, and",
       " usage of `OverdubInstead` has been replaced with `Cassette.recurse`;",
       " see `Cassette.recurse` for details."))

struct Sorry_Execute_Is_Deprecated end

Base.@deprecate_binding(execute, Sorry_Execute_Is_Deprecated, false,
string("; the `overdub` <--> `execute` cycle has been removed from Cassette. In",
       " most cases, `execute` calls can now be replaced with `overdub`; in",
       " general, however, users may need to do some extra refactoring to",
       " properly migrate to the new overdubbing model. Feel free to open a",
       " Cassette issue for help with migration."))

Base.@deprecate(canoverdub(args...), canrecurse(args...))
