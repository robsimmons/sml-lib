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

(*
/// An axis aligned bounding box.
struct b2AABB
{
        /// Verify that the bounds are sorted.
        bool IsValid() const;

        /// Get the center of the AABB.
        b2Vec2 GetCenter() const

        /// Get the extents of the AABB (half-widths).
        b2Vec2 GetExtents() const

        /// Combine two AABBs into this one.
        void Combine(const b2AABB& aabb1, const b2AABB& aabb2)
        /// Does this aabb contain the provided AABB.
        bool Contains(const b2AABB& aabb) const
        bool RayCast(b2RayCastOutput* output, const b2RayCastInput& input) const;

};
*)

(*

/// Compute the collision manifold between two circles.
void b2CollideCircles(b2Manifold* manifold,
                                          const b2CircleShape* circle1, const b2Transform& xf1,
                                          const b2CircleShape* circle2, const b2Transform& xf2);

/// Compute the collision manifold between a polygon and a circle.
void b2CollidePolygonAndCircle(b2Manifold* manifold,
                                                           const b2PolygonShape* polygon, const b2Transform& xf1,
                                                           const b2CircleShape* circle, const b2Transform& xf2);

/// Compute the collision manifold between two polygons.
void b2CollidePolygons(b2Manifold* manifold,
                                           const b2PolygonShape* polygon1, const b2Transform& xf1,
                                           const b2PolygonShape* polygon2, const b2Transform& xf2);

/// Clipping for contact manifolds.
int32 b2ClipSegmentToLine(b2ClipVertex vOut[2], const b2ClipVertex vIn[2],
                                                        const b2Vec2& normal, float32 offset);

/// Determine if two generic shapes overlap.
bool b2TestOverlap(const b2Shape* shapeA, const b2Shape* shapeB,
                                   const b2Transform& xfA, const b2Transform& xfB);

*)


end