
(* Set of points in the 2d plane that allows retrieval of all points
   that are within some distance of a query point. *)
signature QUADTREE =
sig

    (* Functional set of points with associated data of type 'a *)
    type 'a quadtree
	
    (* No points. *)
    val empty : 'a quadtree

    val insert : 'a quadtree -> 'a -> real * real -> 'a quadtree

    val lookup : 'a quadtree -> real * real -> real -> 'a list
    val lookuppoint : 'a quadtree -> real * real -> real -> ('a * real * real) list

end