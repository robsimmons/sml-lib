
structure QuadtreeTest =
struct

    structure Q = Quadtree
    val t = foldl (fn (p, t) => Q.insert t p p) Q.empty
        [(0.0, 0.0),
         (~1.0, 0.0),
         (3.0, 0.0),
         (1.0, ~1.0),
         (1.0, ~2.0)]

    val l1 = Q.lookup t (0.0, 0.0) 0.0
    val l2 = Q.lookup t (0.0, 0.0) 100.0
    val l3 = Q.lookup t (0.0, 0.0) 2.0

end