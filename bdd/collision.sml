(* Copyright 2010 Tom Murphy VII and Erin Catto. See COPYING for details. *)

(* Collision detection.
   Corresponds to the implementation components of 
   collision/b2collision.{h,cpp}. *)

structure BDDCollision :> BDDCOLLISION =
struct
  open BDDSettings
  open BDDTypes
  open BDDMath
  open BDDOps
  infix 6 :+: :-: %-% %+% +++
  infix 7 *: *% +*: +*+ #*% @*:

  fun initialize_manifold (world_manifold : world_manifold,
                           manifold : manifold,
                           xfa : transform, radiusa : real,
                           xfb, radiusb) =
    if #point_count manifold = 0
    then ()
    else case #typ manifold of
        E_Circles => 
            let 
                val () = vec2set (#normal world_manifold, 1.0, 0.0)
                val pointa = multransformv(xfa, #local_point manifold)
                val pointb = multransformv
                    (xfb,
                     #local_point (Array.sub(#points manifold, 0)))
                val () = 
                    if distance_squared (pointa, pointb) >
                       epsilon * epsilon
                    then (vec2setfrom (#normal world_manifold, pointb :-: pointa);
                          ignore (vec2normalize (#normal world_manifold)))
                    else ()

                val ca = pointa :+: radiusa *: (#normal world_manifold)
                val cb = pointb :-: radiusb *: (#normal world_manifold)
            in
                Array.update(#points world_manifold, 0, 0.5 *: (ca :+: cb))
            end

      | E_FaceA =>
            let
                val () = vec2setfrom (#normal world_manifold,
                                      transformr xfa +*: #local_normal manifold)
                val plane_point = xfa @*: #local_point manifold
            in
                for 0 (#point_count manifold - 1)
                (fn i =>
                 let val clip_point = xfb @*: #local_point (Array.sub(#points manifold,
                                                                      i))
                     val ca = clip_point :+: 
                         (radiusa - dot2(clip_point :-:
                                         plane_point,
                                         #normal world_manifold)) *:
                         #normal world_manifold
                     val cb = clip_point :-: radiusb *: #normal world_manifold
                 in
                     Array.update(#points world_manifold, i, 0.5 *: (ca :+: cb))
                 end)
            end
      | E_FaceB =>
            let val () = vec2setfrom (#normal world_manifold,
                                      transformr xfb +*: #local_normal manifold)
                val plane_point = xfb @*: #local_point manifold
            in
                for 0 (#point_count manifold - 1)
                (fn i =>
                 let val clip_point = xfa @*: #local_point (Array.sub(#points manifold,
                                                                      i))
                     val cb = clip_point :+: 
                         (radiusb - dot2(clip_point :-:
                                         plane_point,
                                         #normal world_manifold)) *:
                         #normal world_manifold
                     val ca = clip_point :-: radiusa *: #normal world_manifold
                 in
                     Array.update(#points world_manifold, i, 0.5 *: (ca :+: cb))
                 end);
                (* Ensure normal points from A to B. *)
                vec2setfrom (#normal world_manifold, vec2neg (#normal world_manifold))
            end

  fun get_point_states (manifold1 : manifold, manifold2 : manifold) =
      let
          val state1 = Array.array(max_manifold_points, NullState)
          val state2 = Array.array(max_manifold_points, NullState)
      in
          (* Detect persists and removes. *)
          for 0 (#point_count manifold1 - 1)
          (fn i =>
           let val id : contact_id = #id (Array.sub(#points manifold1, i))
           in
               Array.update(state1, i, RemoveState);
               for 0 (#point_count manifold2 - 1)
               (fn j =>
                if #id (Array.sub(#points manifold2, j)) = id
                then Array.update(state1, i, PersistState)
                else ())
           end);

          (* Detect persists and adds. *)
          for 0 (#point_count manifold2 - 1)
          (fn i =>
           let val id : contact_id = #id (Array.sub(#points manifold2, i))
           in
               Array.update(state2, i, AddState);
               for 0 (#point_count manifold1 - 1)
               (fn j =>
                if #id (Array.sub(#points manifold1, j)) = id
                then Array.update(state2, i, PersistState)
                else ())
           end);

          (state1, state2)
      end

(*
/// An axis aligned bounding box.
struct b2AABB
{
        /// Verify that the bounds are sorted.
        bool IsValid() const;

        /// Get the center of the AABB.
        b2Vec2 GetCenter() const
        {
                return 0.5f * (lowerBound + upperBound);
        }

        /// Get the extents of the AABB (half-widths).
        b2Vec2 GetExtents() const
        {
                return 0.5f * (upperBound - lowerBound);
        }

        /// Combine two AABBs into this one.
        void Combine(const b2AABB& aabb1, const b2AABB& aabb2)
        {
                lowerBound = b2Min(aabb1.lowerBound, aabb2.lowerBound);
                upperBound = b2Max(aabb1.upperBound, aabb2.upperBound);
        }

        /// Does this aabb contain the provided AABB.
        bool Contains(const b2AABB& aabb) const
        {
                bool result = true;
                result = result && lowerBound.x <= aabb.lowerBound.x;
                result = result && lowerBound.y <= aabb.lowerBound.y;
                result = result && aabb.upperBound.x <= upperBound.x;
                result = result && aabb.upperBound.y <= upperBound.y;
                return result;
        }

        bool RayCast(b2RayCastOutput* output, const b2RayCastInput& input) const;

        b2Vec2 lowerBound;      ///< the lower vertex
        b2Vec2 upperBound;      ///< the upper vertex
};
*)

(*
inline bool b2AABB::IsValid() const
{
        b2Vec2 d = upperBound - lowerBound;
        bool valid = d.x >= 0.0f && d.y >= 0.0f;
        valid = valid && lowerBound.IsValid() && upperBound.IsValid();
        return valid;
}

inline bool b2TestOverlap(const b2AABB& a, const b2AABB& b)
{
        b2Vec2 d1, d2;
        d1 = b.lowerBound - a.upperBound;
        d2 = a.lowerBound - b.upperBound;

        if (d1.x > 0.0f || d1.y > 0.0f)
                return false;

        if (d2.x > 0.0f || d2.y > 0.0f)
                return false;

        return true;
}
*)

end