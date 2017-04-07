################
# TrackedArray #
################

mutable struct TrackedArray{G,V,N,AV<:AbstractArray{V,N},C,AC} <: AbstractArray{TrackedReal{G,V,C},N}
    tape::Tape{G}
    value::AV
    cache::AC
end
