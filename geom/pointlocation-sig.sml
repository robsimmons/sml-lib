(* Given a set of non-overlapping simple polygons in the plane,
   produce a data structure that can test which (if any) polygon a
   point lies in. *)
signature POINTLOCATION =
sig

  exception PointLocation of string

  (* A pre-built point locator. Each region has associated with it an
     arbitrary piece of data. *)
  type 'a locator

  (* The polygons must not overlap. If they do, this function may raise
     PointLocation. Or, the constructed locator may return an arbitrarily
     chosen overlapping polygon when queries. *)
  val locator : ('a * Polygon.polygon) list -> 'a locator

  (* Give epsilon, the distance within which close vertices (on
     different polygons) will be merged. *)
  val locatorex : real -> ('a * Polygon.polygon) list -> 'a locator

  (* Give the list of polygons that have no external-facing edges. This
     can be used to find inadvertent slivers of empty space between
     regions. *)
  val interior : 'a locator -> 'a list

  (* If the point is outside all the polygons, returns NONE. Boundary
     cases are handled arbitrarily, but consistently. *)
  val location : 'a locator -> real * real -> 'a option

end