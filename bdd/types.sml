(* Copyright 2010 Tom Murphy VII and Erin Catto. See COPYING for details. *)

(* This collects a bunch of basic transparent types that need to be shared
   between various modules. *)

structure BDDTypes =
struct

  exception Unimplemented of string

  (* Mass data computed for a shape. *)
  type mass_data = 
      { (* Mass of the shape, usually in kilograms. *)
        mass : real, 
        (* Position of the shape's centroid relative to the 
           shape's origin. *)
        center : BDDMath.vec2, 
        (* The rotational intertia of the shape about the
           local origin. *)
        i : real }
      
  (* Ray-cast input data. 
     The ray extends from p1 to p1 + maxFraction * (p2 - p1). *)
  type ray_cast_input =
      { p1 : BDDMath.vec2, p2 : BDDMath.vec2,
        max_fraction : real }

  (* Ray-cast output data. 
     The ray hits at p1 + fraction * (p2 - p1), where p1 and p2
     come from b2RayCastInput. *)
  type ray_cast_output =
      { normal : BDDMath.vec2,
        fraction : real }

  (* Packed bytes, high to low:
     reference_edge
     incident_edge
     incident_vertex
     flip 
     Accessors in BDDCollision.
     *)
  type contact_id = Word32.word

  (* A manifold point is a contact point belonging to a contact
     manifold. It holds details related to the geometry and dynamics
     of the contact points.
     The local point usage depends on the manifold type:
     -e_circles: the local center of circleB
     -e_faceA: the local center of cirlceB or the clip point of polygonB
     -e_faceB: the clip point of polygonA
     This structure is stored across time steps, so we keep it small.
     Note: the impulses are used for internal caching and may not
     provide reliable contact forces, especially for high speed collisions. *)
  type manifold_point =
      {
        (* usage depends on manifold type; see above *)
        local_point : BDDMath.vec2,
        (* the non-penetration impulse *)
        normal_impulse : real,
        (* the friction impulse *)
        tangent_impulse : real,
        (* uniquely identifies a contact point
           between two shapes *)
        id : contact_id
      }

  (* A manifold for two touching convex shapes.
     Box2D supports multiple types of contact:
       - clip point versus plane with radius
       - point versus point with radius (circles)
     The local point usage depends on the manifold type:
       - e_circles: the local center of circleA
       - e_faceA: the center of faceA
       - e_faceB: the center of faceB
     Similarly the local normal usage:
       - e_circles: not used
       - e_faceA: the normal on polygonA
       - e_faceB: the normal on polygonB
     We store contacts in this way so that position correction can
     account for movement, which is critical for continuous physics.
     All contact scenarios must be expressed in one of these types.
     This structure is stored across time steps, so we keep it small. *)
  datatype manifold_type = E_Circles | E_FaceA | E_FaceB
  type manifold =
      {
        typ : manifold_type,
        (* the points of contact.
           up to BDDSettings.max_manifold_points *)
        points : manifold_point array,
        (* not used for Type::e_points (? is this an out of date comment? -tom7) *)
        local_normal : BDDMath.vec2,
        (* usage depends on manifold type *)
        local_point : BDDMath.vec2,
        (* the number of manifold points *)
        point_count : int
      }

  (* This is used to compute the current state of a contact manifold. *)
  type world_manifold =
      {
        (* world vector pointing from A to B *)
        normal : BDDMath.vec2,
        (* world contact point (point of intersection)
           Up to BDDSettings.max_manifold_points *)
        points : BDDMath.vec2 array
      }

  (* This is used for determining the state of contact points. *)
  datatype point_state =
      (* point does not exist *)
      NullState
      (* point was added in the update *)
    | AddState
      (* point persisted across the update *)
    | PersistState
      (* point was removed in the update *)
    | RemoveState

  (* Used for computing contact manifolds. *)
  (* TODO(twm): Is this internal? I think it might only
     be used inside collision.sml. *)
  type clip_vertex = { v : BDDMath.vec2,
                       id : contact_id }

  (* Axis-aligned bounding box. Treated as immutable -- don't
     change the vectors! *)
  type aabb = { lowerbound : BDDMath.vec2,
                upperbound : BDDMath.vec2 }

  (* Abstract proxy for GJK distance algorithm. *)
  type distance_proxy = 
      {
       (* Get the supporting vertex index in the given direction. *)
       support : BDDMath.vec2 -> int,
       (* Get the supporting vertex in the given direction. *)
       support_vertex : BDDMath.vec2 -> BDDMath.vec2,
       (* Number of total vertices. *)
       vertex_count : int,
       (* Get a vertex by index. *)
       vertex : int -> BDDMath.vec2,
       radius : real
      }

  (* Used to "warm start" the distance function. This is
     used by the GJK algorithm as it refines its approximation. 
     BDDDistance contains the cold start initial value. *)
  type simplex_cache = { (* Length or area *)
                         metric : real ref,
                         (* PERF these were smaller int types
                            in the original Box2D. Can't tell
                            yet whether the size/speed tradeoff
                            is wise. *)
                         count : int ref,
                         (* Exactly three elements *)
                         indexa : int array,
                         indexb : int array }

  type distance_input = { proxya : distance_proxy,
                          proxyb : distance_proxy,
                          transforma : BDDMath.transform,
                          transformb : BDDMath.transform,
                          use_radii : bool }

  type distance_output = { (* Closest point on shape A *)
                           pointa : BDDMath.vec2,
                           (* Closest point on shape B *)
                           pointb : BDDMath.vec2,
                           distance : real,
                           (* Number of GJK iterations used *)
                           iterations : int }

end