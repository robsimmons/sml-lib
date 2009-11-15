(* Given a set of non-overlapping simple polygons in the plane,
   produce a data structure that can test which (if any) polygon a
   point lies in. *)
signature POINTLOCATION =
sig

  exception PointLocation of string

  (* A pre-built point locator. Each region has associated with it an
     arbitrary piece of data. *)
  type 'a locator

  (* Merges nearby points (on different polygons). Nearby means within
     the supplied epsilon. The algorithm is insensitive to the order
     of the polygons or order of the points in the polygons, but is not
     rotationally invariant. (See snappoints.sml for discussion.) *)
  (* val snap : real -> ('a * Polygon.polygon) list -> ('a * Polygon.polygon) list *)

  (* The polygons must not overlap. If they do, this function may raise
     PointLocation. Or, the constructed locator may return an arbitrarily
     chosen overlapping polygon when queries. *)
  val locator : ('a * Polygon.polygon) list -> 'a locator

  (* Give the list of polygons that have no external-facing edges. This
     can be used to find inadvertent slivers of empty space between
     regions. *)
  val interior : 'a locator -> 'a list

  (* If the point is outside all the polygons, returns NONE. Boundary
     cases are handled arbitrarily, but consistently. *)
  val location : 'a locator -> real * real -> 'a option

  (* tosvg l f
     Write an svg document describing l by repeatedly calling f to
     output the data. This is just for diagnostic purposes. *)
  val tosvg : 'a locator -> (string -> unit) -> unit

end