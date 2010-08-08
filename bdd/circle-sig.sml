(* Copyright 2010 Tom Murphy VII and Erin Catto. See COPYING for details. *)

(* The circle shape. This is just the basics; the collision stuff is
   defined in collision.sml to mimic Box2D.
   Corresponding to collision/shapes/b2circleshape.h *)

signature BDDCIRCLE =
sig

  type circle = { radius : real,
                  p : BDDMath.vec2 }

  val clone : circle -> circle

  (* See shape-sig.sml *)
  val test_point : circle * BDDMath.transform * BDDMath.vec2 -> bool
  val ray_cast : circle * BDDMath.transform * BDDTypes.ray_cast_input ->
                 BDDTypes.ray_cast_output option
  val compute_aabb : circle * BDDMath.transform -> BDDTypes.aabb
  (* compute_mass circle density *)
  val compute_mass : circle * real -> BDDTypes.mass_data

  (* Get the supporting vertex index in the given direction. *)
  val get_support : circle * BDDMath.vec2 -> int

  (* Get the supporting vertex in the given direction. *)
  val get_support_vertex : circle * BDDMath.vec2 -> BDDMath.vec2

  (* Get the vertex count. Always returns 1. *)
  val get_vertex_count : circle -> int

  (* Get a vertex by index. Used in distance calculation. *)
  val get_vertex : circle * int -> BDDMath.vec2

end
