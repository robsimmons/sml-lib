(* Copyright 2010 Tom Murphy VII and Erin Catto. See COPYING for details. *)

(* The convex polygon shape. The maximum number of vertices is given in
   settings.sml; since these must be convex there's not usually much reason
   to make it larger than the default. Complex shapes are typically made
   of multiple polygons linked together.
   
   More code pertaining to polygons is in collision.sml, to mimic the
   organization of Box2D.

   Corresponding to collision/shapes/b2polygonshape.h *)

signature BDDPOLYGON =
sig

  (* Don't create polygons directly; use one of the initializers, so
     that the centroid and normals are set properly. *)
  type polygon = { centroid : BDDMath.vec2,
		   vertices : BDDMath.vec2 Array.array,
		   normals : BDDMath.vec2 Array.array }

  val clone : polygon -> polygon

  (* Must be no more than BDDSettings.max_polygon_vertices. 
     Must use CCW winding order, assuming the z axis is pointing
     towards your face. Note: If y increases towards the bottom
     of the screen (like e.g. SDL) then you want CW winding.
     The polygon must be convex. *)
  val polygon : BDDMath.vec2 list -> polygon
  (* An axis-aligned box centered around 0,0. Give the half-width
     and half-height. *)
  val box : real * real -> polygon
  (* rotated_box hx hy center angle
     hx/hy are the half-width and half-height.
     center is the center of the box in local coordinates.
     angle is the rotation of the box (radians) in local coordinates. *)
  val rotated_box : real * real * BDDMath.vec2 * real -> polygon

  (* A single edge, between the two points. 
     XXX tom: specify winding order? does it matter?
     *)
  val edge : BDDMath.vec2 * BDDMath.vec2 -> polygon

  (* See shape-sig.sml *)
  val test_point : polygon * BDDMath.transform * BDDMath.vec2 -> bool
  val ray_cast : polygon * BDDMath.transform * BDDTypes.ray_cast_input ->
                 BDDTypes.ray_cast_output option
  val compute_aabb : polygon * BDDMath.transform -> BDDTypes.aabb
  (* compute_mass polygon density *)
  val compute_mass : polygon * real -> BDDTypes.mass_data

  (* Get the supporting vertex index in the given direction. *)
  val get_support : polygon * BDDMath.vec2 -> int

  (* Get the supporting vertex in the given direction. *)
  val get_support_vertex : polygon * BDDMath.vec2 -> BDDMath.vec2

  (* Get the vertex count. *)
  val get_vertex_count : polygon -> int

  (* Get a vertex by index. Used in distance calculation. *)
  val get_vertex : polygon * int -> BDDMath.vec2

end
