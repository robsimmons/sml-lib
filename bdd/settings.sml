(* Copyright 2010 Tom Murphy VII and Erin Catto. See COPYING for details. *)

(* This file defines some constants used by the BoxDiaDia library. I
   don't think you should need to change them. If you do, you probably
   want to define your own BDDSettings in a file in your project directory,
   and then use it instead of this file in your build.
   Corresponding to common/b2settings.h. *)
structure BDDSettings =
struct

  val flt_max = Real.maxFinite
  val epsilon = 0.000000001 (* Real.nextAfter (0.0, 1.0) *) (* Real.minNormalPos *)
  val () = if Real.== (1.0 + epsilon, 1.0)
           then raise Fail "FLT_EPSILON is supposed to have this property"
           else ()
  val pi = 3.14159265359

  (* Collision. *)

  (* The maximum number of contact points between two convex shapes. *)
  val max_manifold_points = 2

  (* The maximum number of vertices on a convex polygon. *)
  val max_polygon_vertices = 8

  (* This is used to fatten AABBs in the dynamic tree. This allows proxies
     to move by a small amount without triggering a tree adjustment.
     This is in meters. *)
  val aabb_extension = 0.1

  (* This is used to fatten AABBs in the dynamic tree. This is used to predict
     the future position based on the current displacement.
     This is a dimensionless multiplier. *)
  val aabb_multiplier = 2.0

      
  (* A small length used as a collision and constraint tolerance. Usually it is
     chosen to be numerically significant, but visually insignificant. *)
  val linear_slop = 0.005

  (* A small angle used as a collision and constraint tolerance. Usually it is
     chosen to be numerically significant, but visually insignificant. *)
  val angular_slop = 2.0 / 180.0 * pi

  (* The radius of the polygon/edge shape skin. This should not be modified. 
     Making this smaller means polygons will have an insufficient buffer for 
     continuous collision.
     Making it larger may create artifacts for vertex collision. *)
  val polygon_radius = 2.0 * linear_slop

  (* Dynamics. *)

  (* Maximum number of contacts to be handled to solve a TOI impact. *)
  val max_toi_contacts = 32

  (* A velocity threshold for elastic collisions. Any collision with a relative linear
     velocity below this threshold will be treated as inelastic. *)
  val velocity_threshold = 1.0

  (* The maximum linear position correction used when solving constraints. 
     This helps to prevent overshoot. *)
  val max_linear_correction = 0.2

  (* The maximum angular position correction used when solving constraints. 
     This helps to prevent overshoot. *)
  val max_angular_correction = 8.0 / 180.0 * pi

  (* The maximum linear velocity of a body. This limit is very large and is used
     to prevent numerical problems. You shouldn't need to adjust this. *)
  val max_translation = 2.0
  val max_translation_squared = max_translation * max_translation

  (* The maximum angular velocity of a body. This limit is very large and is used
     to prevent numerical problems. You shouldn't need to adjust this. *)
  val max_rotation = 0.5 * pi
  val max_rotation_squared = max_rotation * max_rotation

  (* This scale factor controls how fast overlap is resolved. Ideally 
     this would be 1 so that overlap is removed in one time step. However
     using values close to 1 often lead to overshoot. *)
  val contact_baumgarte = 0.2

  (* Sleep. *)

  (* The time that a body must be still before it will go to sleep. *)
  val time_to_sleep = 0.5

  (* A body cannot sleep if its linear velocity is above this tolerance. *)
  val linear_sleep_tolerance = 0.01

  (* A body cannot sleep if its angular velocity is above this tolerance. *)
  val angular_sleep_tolerance = 2.0 / 180.0 * pi

  (* Memory Allocation *)

(* XXX probably don't need. remove -tom7
     Implement this function to use your own memory allocator.
void* b2Alloc(int32 size);

     If you implement b2Alloc, you should also implement this function.
void b2Free(void* mem);

     Version numbering scheme.
     See http://en.wikipedia.org/wiki/Software_versioning
struct b2Version
{
 int32 major;  ///< significant changes
 int32 minor;  ///< incremental changes
 int32 revision;  ///< bug fixes
};

     Current version.
extern b2Version b2_version;
*)

  (* Friction mixing law. Feel free to customize this. *)
  fun mix_friction(f1, f2) : real = Math.sqrt(f1 * f2)

  (* Restitution mixing law. Feel free to customize this. *)
  fun mix_restitution(r1, r2) : real = Real.max(r1, r2)

end
