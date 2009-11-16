(* This is a spatial data structure (enumerates points close to the
   query point) for points on the Earth.

   It is not straightforward to use most spatial data structures
   because the surface cannot be subdivided like Euclidean space
   (eventually, it wraps around). So, this structure creates
   two Quadtrees, one for each hemisphere, and for queries that
   are near the boundary, just queries both. *)
signature LATLONTREE =
sig
    include QUADTREE where type pos = LatLon.pos
                       and type dist = real
    (* tosvg tree maxdepth west print
       For visualization purposes. Uses gnomonic projection with root of
       tree as center. If west is true, print west hemisphere. If false,
       print east. *)
    val tosvg : 'a tree -> int -> bool -> (string -> unit) -> unit
end
