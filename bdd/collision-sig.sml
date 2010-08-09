(* Copyright 2010 Tom Murphy VII and Erin Catto. See COPYING for details. *)

(* Collision detection.
   Corresponds to collision/b2collision.h, but the type
   declarations were moved to types.sml *)

signature BDDCOLLISION =
sig

  (* Evaluate the manifold with supplied transforms. This assumes
     modest motion from the original state. This does not change the
     point count, impulses, etc. The radii must come from the shapes
     that generated the manifold. *)
  val initialize_manifold : BDDTypes.world_manifold *
                            BDDTypes.manifold * 
                            BDDMath.transform * real * 
                            BDDMath.transform * real -> unit


  (* Compute the point states given two manifolds, based on whether they
     share mutual points. The states pertain to the transition from
     manifold1 to manifold2. So state1 is either persist or remove
     while state2 is either add or persist. *)
  val get_point_states : BDDTypes.manifold * BDDTypes.manifold -> 
                         BDDTypes.point_state array * BDDTypes.point_state array


  (* True if the bounds are sorted. *)
  val aabb_valid : BDDTypes.aabb -> bool
  (* Get the center of the AABB. *)
  val aabb_center : BDDTypes.aabb -> BDDMath.vec2
  (* Get the extents of the AABB (half-widths). *)
  val aabb_extents : BDDTypes.aabb -> BDDMath.vec2

  (* combine dest a b
     Combine the two aabbs into the destination, overwriting it. *)
  val aabb_combine : BDDTypes.aabb * BDDTypes.aabb * BDDTypes.aabb -> unit

  (* contains container test
     Does the container contain the test aabb? *)
  val aabb_contains : BDDTypes.aabb * BDDTypes.aabb -> bool

  val aabb_overlap : BDDTypes.aabb * BDDTypes.aabb -> bool

  val aabb_ray_cast : BDDTypes.aabb * BDDTypes.ray_cast_input -> 
                      BDDTypes.ray_cast_output option

  (* Compute the collision manifold between two circles. *)
  val collide_circles : BDDCircle.circle * BDDMath.transform *
                        BDDCircle.circle * BDDMath.transform -> BDDTypes.manifold

  (* clip_segment_to_line (v1, v2, normal, offset)
     Clipping for contact manifolds. Returns either no points (entire segment
     is clipped out) or two points (possibly modified). *)
  val clip_segment_to_line : BDDTypes.clip_vertex * BDDTypes.clip_vertex *
                             BDDMath.vec2 * real -> 
                             (BDDTypes.clip_vertex * BDDTypes.clip_vertex) option

(*

/// Compute the collision manifold between a polygon and a circle.
void b2CollidePolygonAndCircle(b2Manifold* manifold,
                                                           const b2PolygonShape* polygon, const b2Transform& xf1,
                                                           const b2CircleShape* circle, const b2Transform& xf2);

/// Compute the collision manifold between two polygons.
void b2CollidePolygons(b2Manifold* manifold,
                                           const b2PolygonShape* polygon1, const b2Transform& xf1,
                                           const b2PolygonShape* polygon2, const b2Transform& xf2);

/// Determine if two generic shapes overlap.
bool b2TestOverlap(const b2Shape* shapeA, const b2Shape* shapeB,
                                   const b2Transform& xfA, const b2Transform& xfB);

*)


end