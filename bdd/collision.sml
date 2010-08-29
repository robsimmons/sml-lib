(* Copyright 2010 Tom Murphy VII and Erin Catto. See COPYING for details. *)

(* Collision detection.
   Corresponds to the implementation components of 
   collision/b2collision.{h,cpp} as well as b2collidecircle.cpp and
   b2collidepolygon.cpp *)

structure BDDCollision :> BDDCOLLISION =
struct
  open BDDSettings
  open BDDTypes
  open BDDMath
  open BDDOps
  infix 6 :+: :-: %-% %+% +++
  infix 7 *: *% +*: +*+ #*% @*:

  exception BDDCollision of string

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

  fun aabb_valid { lowerbound, upperbound } =
      let val d : vec2 = upperbound :-: lowerbound
      in vec2x d > 0.0 andalso vec2y d > 0.0
          andalso vec2is_valid lowerbound
          andalso vec2is_valid upperbound
      end

  fun aabb_center { lowerbound, upperbound } =
      0.5 *: (lowerbound :+: upperbound)

  fun aabb_extents { lowerbound, upperbound } =
      0.5 *: (upperbound :-: lowerbound)

  fun aabb_combine ({ lowerbound = l1, upperbound = u1 },
                    { lowerbound = l2, upperbound = u2 }) =
      { lowerbound = vec2min(l1, l2),
        upperbound = vec2max(u1, u2) }

  fun aabb_contains ({ lowerbound, upperbound }, { lowerbound = l,
                                                   upperbound = u }) =
      vec2x lowerbound <= vec2x l andalso
      vec2y lowerbound <= vec2y l andalso
      vec2x u <= vec2x upperbound andalso
      vec2y u <= vec2y upperbound

  fun aabb_ray_cast ({ lowerbound, upperbound },
                     { p1 : BDDMath.vec2, p2 : BDDMath.vec2,
                       max_fraction : real }) 
      : BDDTypes.ray_cast_output option =
      let
          exception No
          val tmin = ref (~max_float)
          val tmax = ref max_float

          val p = p1
          val d = p2 :-: p1
          val absd : vec2 = vec2abs d
              
          (* In original, uninitialized *)
          val normal = vec2 (0.0, 0.0)
          fun setnormalx x = vec2set (normal, x, 0.0)
          fun setnormaly y = vec2set (normal, 0.0, y)

          (* In original, a loop for i = 0 and 1 *)
          fun loop proj setnormal =
              if proj absd < epsilon
              then (* parallel *)
                  not (proj p < proj lowerbound orelse
                       proj lowerbound < proj p)
              else
                  let val inv_d = 1.0 / proj d
                      val t1 = (proj lowerbound - proj p) * inv_d
                      val t2 = (proj upperbound - proj p) * inv_d
                      (* sign of the normal vector *)
                      val (t1, t2, s) =
                          if t1 > t2
                          then (t2, t1, 1.0)
                          else (t1, t2, ~1.0)
                  in
                      (* push the min up. *)
                      if (t1 > !tmin)
                      then (setnormal s; tmin := t1)
                      else ();

                      (* pull the max down. *)
                      tmax := Real.min(!tmax, t2);

                      not (!tmin > !tmax)
                  end

      in
          if loop vec2x setnormalx andalso
             loop vec2y setnormaly andalso
             (* Does the ray start inside the box?
                Does the ray intersect beyond the max fraction? *)
             not (!tmin < 0.0 orelse max_fraction < !tmin)
          then SOME { fraction = !tmin, normal = normal }
          else NONE
      end

  fun aabb_overlap ({ lowerbound = al, upperbound = au },
                    { lowerbound = bl, upperbound = bu }) : bool =
      let val d1 = bl :-: au
          val d2 = al :-: bu
      in
          not (vec2x d1 > 0.0 orelse vec2y d1 > 0.0 orelse
               vec2x d2 > 0.0 orelse vec2y d2 > 0.0)
      end


  fun collide_circles (circlea : BDDCircle.circle,
                       xfa : transform,
                       circleb : BDDCircle.circle,
                       xfb : transform) : manifold =
      let
          val pa : vec2 = xfa @*: #p circlea
          val pb : vec2 = xfb @*: #p circleb
          val d  : vec2 = pb :-: pa

          val dist_sqr = dot2(d, d)
          val ra = #radius circlea
          val rb = #radius circleb
          val radius = ra + rb
      in
          (* PERF would it work to return NONE? The client
             code can't be accessing these fields; they're
             not even initialized in the original code. *)
          if dist_sqr > radius * radius
          then { point_count = 0,
                 typ = E_Circles,
                 points = Array.fromList nil,
                 local_normal = vec2(0.0, 0.0),
                 local_point = vec2(0.0, 0.0) }
          else
              { typ = E_Circles,
                points = Array.fromList [{ local_point = #p circleb,
                                           id = 0w0,
                                           (* uninitialized in original *)
                                           normal_impulse = 0.0,
                                           tangent_impulse = 0.0 }],
                local_point = #p circlea,
                local_normal = vec2(0.0, 0.0),
                point_count = 1 }
      end

  (* Sutherland-Hodgman clipping. 

     Port note: This code is a little tricky in the original.
     I made them more exlicit here and also clearer that this
     can only return 0 or 2 points. 
     *)
  fun clip_segment_to_line (v0, v1, normal : vec2, offset : real) 
      : (clip_vertex * clip_vertex) option =
      let
          (* Calculate distance of end points to the line. *)
          val distance0 = dot2 (normal, #v v0) - offset
          val distance1 = dot2 (normal, #v v1) - offset
      in
          (* If the two points are on different sides of the
             plane, then we have to clip one of them. *)
          if distance0 * distance1 < 0.0
          then
              (* Find intersection point of edge and plane. *)
              let
                  val interp : real = distance0 / (distance0 - distance1)
                  val clippedv = #v v0 :+: interp *: (#v v1 :-: #v v0)
              in
                  if distance0 > 0.0
                  then SOME(v1, { v = clippedv, id = #id v0 })
                  else SOME(v0, { v = clippedv, id = #id v1 })
              end
          else
            (* Otherwise, we either take both or none. *)
            if distance0 <= 0.0 andalso distance1 <= 0.0
            then SOME(v0, v1)
            else NONE
      end

  (* Determine if two generic shapes overlap. *)
  fun test_overlap (shapea : BDDShape.shape, shapeb : BDDShape.shape,
                    xfa : BDDMath.transform, xfb : BDDMath.transform) : bool =
      let
          val input = { proxya = BDDDistance.shape_proxy shapea,
                        proxyb = BDDDistance.shape_proxy shapeb,
                        transforma = xfa,
                        transformb = xfb,
                        use_radii = true }
          val cache = BDDDistance.initial_cache ()
          val { distance, ... } = BDDDistance.distance (input, cache)
      in
          distance < 10.0 * epsilon
      end


  (* Compute the collision manifold between a polygon and a circle. *)
  exception NoCollision
  fun collide_polygon_and_circle ({ centroid, vertices, normals } : BDDPolygon.polygon,
                                  xfa : BDDMath.transform,
                                  { p = cirp, radius = cirr } : BDDCircle.circle,
                                  xfb : BDDMath.transform) : BDDTypes.manifold =
      let
        (* Compute circle position in the frame of the polygon. *)
        val c : vec2 = xfb @*: cirp
        val c_local : vec2 = mul_ttransformv (xfa, c)

        (* Find the min separating edge. *)
        val normal_index = ref 0
        val separation : real ref = ref (~max_float)
        val radius : real = polygon_radius + cirr
        val vertex_count = Array.length vertices

        val () = 
            Util.for 0 (vertex_count - 1)
            (fn i =>
             let
                 val s : real = dot2(Array.sub(normals, i), 
                                     c_local :-: Array.sub(vertices, i))
             in
                 if s > radius
                 (* early out *)
                 then raise NoCollision
                 else if s > !separation
                      then (separation := s;
                            normal_index := i)
                      else ()
             end)

        (* Vertices that subtend the incident face. *)
        val vert_index1 = !normal_index
        val vert_index2 = if vert_index1 + 1 < vertex_count
                          then vert_index1 + 1
                          else 0
        val v1 = Array.sub(vertices, vert_index1)
        val v2 = Array.sub(vertices, vert_index2)
           
        val separation = !separation
      in
        (* If the center is inside the polygon ... *)
        if separation < epsilon
        then 
            { point_count = 1,
              typ = E_FaceA,
              local_normal = Array.sub(normals, !normal_index),
              local_point = 0.5 *: (v1 :+: v2),
              points = Array.fromList [{ local_point = cirp,
                                         id = 0w0,
                                         (* PERF uninitialized in Box2D *)
                                         normal_impulse = 0.0,
                                         tangent_impulse = 0.0 }] }
        else
        let
            (* Compute barycentric coordinates. *)
            val u1 : real = dot2(c_local :-: v1, v2 :-: v1)
            val u2 : real = dot2(c_local :-: v2, v1 :-: v2)
        in
            if u1 <= 0.0
            then (if distance_squared(c_local, v1) > radius * radius
                  then raise NoCollision
                  else { point_count = 1,
                         typ = E_FaceA,
                         local_normal = vec2normalized (c_local :-: v1),
                         local_point = v1,
                         points = Array.fromList [{ local_point = cirp,
                                                    id = 0w0,
                                                    (* PERF uninitialized in Box2D *)
                                                    normal_impulse = 0.0,
                                                    tangent_impulse = 0.0 }] })
            else if u2 <= 0.0
            then (if distance_squared(c_local, v2) > radius * radius
                  then raise NoCollision
                  else { point_count = 1,
                         typ = E_FaceA,
                         local_normal = vec2normalized(c_local :-: v2),
                         local_point = v2,
                         points = Array.fromList [{ local_point = cirp,
                                                    id = 0w0,
                                                    (* PERF uninitialized in Box2D *)
                                                    normal_impulse = 0.0,
                                                    tangent_impulse = 0.0 }] })
            else let
                     val face_center : vec2 = 0.5 *: (v1 :+: v2)
                     val separation : real = dot2 (c_local :-: face_center,
                                                   Array.sub(normals, vert_index1))
                 in if separation > radius
                    then raise NoCollision
                    else { point_count = 1,
                           typ = E_FaceA,
                           local_normal = Array.sub(normals, vert_index1),
                           local_point = face_center,
                           points = Array.fromList [{ local_point = cirp,
                                                      id = 0w0,
                                                      (* PERF uninitialized in Box2D *)
                                                      normal_impulse = 0.0,
                                                      tangent_impulse = 0.0 }] }
                 end
        end
      end handle NoCollision => 
          (* XXX should return NONE in the no collision case.
             These are uninitialized in Box2D, and I guess client
             code just checks point_count. *)
          { point_count = 0,
            typ = E_Circles,
            points = Array.fromList nil,
            local_normal = vec2(0.0, 0.0),
            local_point = vec2(0.0, 0.0) }      


  (* Compute the collision manifold between two polygons. *)
  fun collide_polygons (polya : BDDPolygon.polygon,
                        xfa : BDDMath.transform,
                        polyb : BDDPolygon.polygon,
                        xfb : BDDMath.transform) : BDDTypes.manifold =
    let
        (* Find edge normal of max separation on A - return if separating axis is found
           Find edge normal of max separation on B - return if separation axis is found
           Choose reference edge as min(minA, minB)
           Find incident edge
           Clip

           The normal points from 1 to 2 *)
    in
      raise BDDCollision "unimplemented"
    end


end
