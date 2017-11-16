@generated function mapcall(g, f, args...)
    gargs = [:(g(args[$i])) for i in 1:nfields(args)]
    return quote
        $(Expr(:meta, :inline))
        g(f)($(gargs...))
    end
end
