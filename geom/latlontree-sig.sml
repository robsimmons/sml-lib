(* This is a spatial data structure (enumerates points close to the
   query point) for points on the Earth.

   It is not straightforward to use most spatial data structures
   because the surface cannot be subdivided like Euclidean space
   (eventually, it wraps around). So, this structure creates
   two Quadtrees, one for each hemisphere, and for queries that
   are near the boundary, just queries both. *)
signature LATLONTREE =
sig

    (* Functional set of points with associated data of type 'a *)
    type 'a latlontree
        
    (* No points. *)
    val empty : 'a latlontree

    val insert : 'a latlontree -> 'a -> LatLon.pos -> 'a latlontree

    (* lookup t p m
       Get all the points within m meters of p. *)
    val lookup : 'a latlontree -> LatLon.pos -> real -> 'a list
    val lookuppoint : 'a latlontree -> LatLon.pos -> real -> ('a * LatLon.pos) list

    val map : ('a -> 'b) -> 'a latlontree -> 'b latlontree

end