
signature VECTORUTIL =
sig

    exception VectorUtil of string

    val filter : ('a -> bool) -> 'a Vector.vector -> 'a Vector.vector

    (* find the first item that satisfies the predicate, if it
       exists. return its index along with the item. *)
    val findi : ('a -> bool) -> 'a Vector.vector -> (int * 'a) option

    (* raises VectorUtil if the vectors are not the same length *)
    val mappair : ('a * 'b -> 'res) -> 
                  'a Vector.vector -> 'b Vector.vector -> 'res Vector.vector

    (* portable across 1997 and 2002 basis *)
    val atov : 'a Array.array -> 'a Vector.vector

    val unzip : ('a * 'b) Vector.vector -> ('a Vector.vector * 'b Vector.vector)

    val count : ('a -> bool) -> 'a Vector.vector -> int

end
