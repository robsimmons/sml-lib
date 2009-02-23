(* two-dimensional polygons *)
signature POLYGON =
sig

  exception Polygon of string

  (* Two-dimensional polygons. Does not exclude self-intersecting
     (complex) or degenerate shapes. *)
  type polygon

  (* there will always be at least 3 points. *)
  val points : polygon -> (real * real) list

  (* must have three points. *)
  val frompoints : (real * real) list -> polygon

  (* Gives an axis-aligned bounding box that includes the polygon, as
     the northwest corner and southeast corner. *)
  val boundingbox : polygon -> { topx : real, topy : real, botx : real, boty : real }

  (* Returns true if the given point is inside the polygon. For a point
     on the boundary, it will return true or false arbitrarily.
     However, for two adjacent polygons sharing an edge, a point on
     that edge will be inside exactly one of the two polygons. *)
  val pointinside : polygon -> real * real -> bool

  (* TODO: area, issimple, isconvex, triangulate, ... *)

end
