signature SNAPPOINTS =
sig

  (* Functor args, for example Polygon.polygon and real *)
  type poly

  (* snap epsilon polys

     Merges nearby points that are on different polygons. Nearby means
     within the supplied epsilon. The algorithm is insensitive to the
     order of the polygons or order of the points in the polygons, but
     is not rotationally invariant. *)
  val snap : real -> ('a * poly) list -> ('a * poly) list

end

(* Also needs a compatible QUADTREE. *)
signature SNAPPOINTSARG =
sig

  type pos
  (* Sequence of positions *)
  type poly
  (* Distance between points *)
  type dist = real

  (* arbitrary ordering on points. *)
  val compare : pos * pos -> order

  val dist : pos * pos -> dist

  val points : poly -> pos list
  val poly : pos list -> poly

end

(* SnapPoly : SNAPPOINTS where type poly = Polygon.polygon
   SnapLatLon : SNAPPOINts where type poly = LatLon.pos list *)
