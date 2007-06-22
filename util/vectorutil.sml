
structure VectorUtil :> VECTORUTIL =
struct

    exception VectorUtil of string

    fun findi f v =
        let
            fun fi n =
                if n = Vector.length v
                then NONE
                else
                    let val a = Vector.sub(v, n)
                    in
                        if f a
                        then SOME (n, a)
                        else fi (n + 1)
                    end
        in
            fi 0
        end


    fun mappair f va vb =
        if Vector.length va <> Vector.length vb
        then raise VectorUtil "in mappair, uneven vectors"
        else 
            Vector.tabulate(Vector.length va,
                            fn i =>
                            f(Vector.sub(va, i),
                              Vector.sub(vb, i)))

    fun atov a =
        Vector.tabulate(Array.length a,
                        fn i => Array.sub(a, i))

    fun unzip (v : ('a * 'b) Vector.vector) =
      (Vector.map #1 v, Vector.map #2 v)

    fun count f v =
        let
            fun c (a, n) =
                if n = Vector.length v
                then a
                else
                    c (if f (Vector.sub(v, n))
                       then a + 1
                       else a, n + 1)
        in
            c (0, 0)
        end

    (* PERF could avoid intermediate allocation, but should (probably) 
       make sure we only call f once for each element... *)
    fun filter f v = Vector.fromList (List.filter f (Vector.foldr op:: nil v))

end