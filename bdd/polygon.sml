(* Copyright 2010 Tom Murphy VII and Erin Catto. See COPYING for details. *)

(* Implementation of polygons (basics).

   Corresponding to collision/shapes/b2polygonshape.cpp *)
structure BDDPolygon :> BDDPOLYGON =
struct
  open BDDSettings
  open BDDTypes
  open BDDMath
  open BDDOps
  infix 6 :+: :-: %-% %+% +++
  infix 7 *: *% +*: +*+ #*% @*:

  exception BDDPolygon of string

  fun copy_array a = Array.tabulate (Array.length a,
                                     (fn x => Array.sub(a, x)))

  (* Port note: In Box2D this inherits from Shape, which has
     a 'radius' member variable. I don't bother to store it
     here since it is a constant (BDDSettings.polygon_radius)
     for polygons. *)
  type polygon = { centroid : vec2,
                   vertices : vec2 Array.array,
                   normals : vec2 Array.array }

  fun clone { centroid, vertices, normals } =
      { centroid = vec2copy centroid,
        vertices = copy_array vertices,
        normals = copy_array normals }

  fun get_support ({ centroid, vertices, normals } : polygon,
                   d : vec2) =
      let
          val best = ref 0
          val bestval = ref (dot2(Array.sub(vertices, 0), d))
      in
          for 1 (Array.length vertices - 1)
          (fn i =>
           let val v = dot2(Array.sub(vertices, i), d)
           in if v > !bestval
              then (bestval := v;
                    best := i)
              else ()
           end);
          !best
      end

  fun get_support_vertex (poly as { vertices, ... } : polygon, d) =
      Array.sub(vertices, get_support (poly, d))

  fun get_vertex ({ vertices, ... } : polygon, idx) =
      Array.sub(vertices, idx)

  fun get_vertex_count ({ vertices, ... } : polygon) = Array.length vertices

  (* Port note: The following are special cased in box2d. It would be
     easy to use the general polygon routine, but I suspect this
     may be preferable for numerical stability. *)
  fun box (hx, hy) : polygon =
      { vertices = Array.fromList (map vec2 [(~hx, ~hy),
                                             (hx, ~hy),
                                             (hx, hy),
                                             (~hx, hy)]),
        normals = Array.fromList (map vec2 [(0.0, ~1.0),
                                            (1.0, 0.0),
                                            (0.0, 1.0),
                                            (~1.0, 0.0)]),
        centroid = vec2 (0.0, 0.0) }

  fun rotated_box (hx, hy, center, angle) =
      let val vertices = map vec2 [(~hx, ~hy),
                                   (hx, ~hy),
                                   (hx, hy),
                                   (~hx, hy)]
          val normals = map vec2 [(0.0, ~1.0),
                                  (1.0, 0.0),
                                  (0.0, 1.0),
                                  (~1.0, 0.0)]

          val xf = transform_pos_angle (center, angle)
          val vertices = map (fn v => xf @*: v) vertices
          val normals = map (fn v => transformr xf +*: v) normals
      in
          { vertices = Array.fromList vertices,
            normals = Array.fromList normals,
            centroid = vec2copy center }
      end

  fun edge (v1, v2) : polygon =
      { centroid = 0.5 *: (v1 :+: v2),
        vertices = Array.fromList [v1, v2],
        normals = let val n = cross2vs(v2 :-: v1, 1.0)
                  in
                      ignore (vec2normalize n : real);
                      Array.fromList [n, vec2neg n]
                  end }

  fun compute_centroid vecs : vec2 =
      if Array.length vecs = 2
      then 0.5 *: (Array.sub(vecs, 0) :+: Array.sub(vecs, 1))
      else
      let
          val c = ref (vec2 (0.0, 0.0))
          val area = ref 0.0

          (* pref is the reference point for forming triangles.
             Its location doesn't change the result (except for rounding error). *)
          val pref = vec2(0.0, 0.0)

          val inv3 = 1.0 / 3.0
      in
          Array.appi
          (fn (i, a) =>
           let
               (* Triangle vertices. *)
               val p1 = pref
               val p2 = a
               val p3 = if i + 1 < Array.length vecs
                        then Array.sub(vecs, i + 1)
                        else Array.sub(vecs, 0)
               val e1 = p2 :-: p1
               val e2 = p3 :-: p1
               val d : real = cross2vv (e1, e2)
               val triangle_area : real = 0.5 * d
           in
               area := !area + triangle_area;
               (* area-weighted centroid *)
               c := !c :+: ((triangle_area * inv3) *: (p1 :+: p2 :+: p3))
           end) vecs;
          (* Centroid *)
          if !area < epsilon
          then raise BDDPolygon "area not positive"
          else (1.0 / !area) *: !c
      end

  fun polygon (vecs : vec2 list) : polygon =
    let
        val num = length vecs
        val () = if num < 2 orelse num > max_polygon_vertices
                 then raise BDDPolygon "not enough vertices, or too many"
                 else ()
        val vertices = Array.fromList (map vec2copy vecs)
        (* Compute normals. Ensure the edges have non-zero length. *)
        val normals = Array.tabulate
            (num,
             (fn i1 =>
              let val i2 = if i1 + 1 < num
                           then i1 + 1
                           else 0
                  val edge : vec2 = Array.sub(vertices, i2) :-:
                      Array.sub(vertices, i1)
                  val () = if vec2length_squared edge > epsilon * epsilon
                           then ()
                           else raise BDDPolygon "nearly zero-length edge"
                  val normal = cross2vs (edge, 1.0)
              in
                  ignore (vec2normalize normal : real);
                  normal
              end))
      (* Port note: Original has commented-out code that checks
         that the polygon is convex and CCW. This is probably
         worth replicating here. *)
    in
        { vertices = vertices,
          normals = normals,
          centroid = compute_centroid vertices }
    end

  fun test_point ( { vertices, normals, ... } : polygon, xf : transform, p : vec2) : bool =
      let
          val p_local : vec2 = mul_t22mv(transformr xf, p :-: transformposition xf)
          (* Since the polygon is convex, it just has to be contained in each half-plane. *)
          fun loop i =
              if i < Array.length vertices
              then if dot2(Array.sub(normals, i), p_local :-: Array.sub(vertices, i)) > 0.0
                   then false
                   else loop (i + 1)
              else true
      in
          loop 0
      end

  exception NoRay
  fun ray_cast ({ vertices, normals, ... } : polygon,
                xf : transform,
                { p1 : BDDMath.vec2, p2 : BDDMath.vec2,
                  max_fraction : real }) : BDDTypes.ray_cast_output option =
      let
          (* Put the ray into the polygon's frame of reference. *)
          val p1 : vec2 = mul_t22mv (transformr xf, p1 :-: transformposition xf)
          val p2 : vec2 = mul_t22mv (transformr xf, p2 :-: transformposition xf)
          val d : vec2 = p2 :-: p1
      in
          if Array.length vertices = 2
          then
            let val v1 = Array.sub (vertices, 0)
                val v2 = Array.sub (vertices, 1)
                val normal = Array.sub (normals, 0)

                (* q = p1 + t * d
                   dot(normal, q - v1) = 0
                   dot(normal, p1 - v1) + t * dot(normal, d) = 0 *)
                val numerator = dot2(normal, v1 :-: p1)
                val denominator = dot2(normal, d)

                val () = if Real.==(denominator, 0.0)
                         then raise NoRay
                         else ()

                val t = numerator / denominator
                val () = if t < 0.0 orelse 1.0 < t
                         then raise NoRay
                         else ()
                val q = p1 :+: t *: d
                (* q = v1 + s * r
                   s = dot(q - v1, r) / dot(r, r) *)
                val r = v2 :-: v1
                val rr = dot2(r, r)

                val () = if Real.== (rr, 0.0)
                         then raise NoRay
                         else ()

                val s = dot2(q :-: v1, r) / rr
                val () = if s < 0.0 orelse 1.0 < s
                         then raise NoRay
                         else ()
            in
                SOME { fraction = t,
                       normal = if numerator > 0.0
                                then vec2neg normal
                                else normal }
            end
          else
            let val lower = ref 0.0
                val upper = ref max_fraction
                val index = ref ~1
            in
              for 0 (Array.length vertices - 1)
              (fn i =>
               let
                 (* p = p1 + a * d
                    dot(normal, p - v) = 0
                    dot(normal, p1 - v) + a * dot(normal, d) = 0 *)
                 val numerator : real = dot2(Array.sub(normals, i),
                                             Array.sub(vertices, i) :-: p1)
                 val denominator : real = dot2(Array.sub(normals, i), d)
               in
                 if Real.== (denominator, 0.0)
                 then if numerator < 0.0
                      then raise NoRay
                      else ()
                 else
                     let
                         (* Note: we want this predicate without division:
                            lower < numerator / denominator, where denominator < 0
                            Since denominator < 0, we have to flip the inequality:
                            lower < numerator / denominator <==>
                              denominator * lower > numerator. *)
                     in
                         if denominator < 0.0 andalso numerator < !lower * denominator
                         then (* Increase lower.
                                 The segment enters this half-space. *)
                             let in
                                 lower := numerator / denominator;
                                 index := i
                             end
                         else if denominator > 0.0 andalso numerator < !upper * denominator
                         then
                             let in
                                  (* Decrease upper.
                                     The segment exits this half-space. *)
                                  upper := numerator / denominator
                             end
                         else ()
                     end;
                  (* The use of epsilon here causes the assert on lower to trip
                     in some cases. Apparently the use of epsilon was to make edge
                     shapes work, but now those are handled separately. *)
                  if !upper < !lower
                  then raise NoRay
                  else ()
               end);

               (* PERF assert *)
               if 0.0 <= !lower andalso !lower <= max_fraction
               then ()
               else raise BDDPolygon "assertion failed in ray_cast";

               if !index >= 0
               then SOME { fraction = !lower,
                           normal = mul22v (transformr xf,
                                            Array.sub(normals, !index)) }
               else NONE
            end
      end handle NoRay => NONE

  fun compute_aabb ({vertices, ...} : polygon, xf : transform) : aabb =
      let
        val start = xf @*: Array.sub(vertices, 0)
        val lower = ref start
        val upper = ref start

        (* Want ArrayUtil.combinel... *)
        val () = for 1 (Array.length vertices - 1)
            (fn i =>
             let val vec = xf @*: Array.sub(vertices, i)
             in lower := vec2min (!lower, vec);
                upper := vec2max (!upper, vec)
             end)
        val r = vec2(polygon_radius, polygon_radius)
      in
          { lowerbound = !lower :-: r,
            upperbound = !upper :-: r }
      end

  fun compute_mass ({vertices, ...} : polygon, density : real) : BDDTypes.mass_data =
          (* PERF assertion *)
      if Array.length vertices < 2
      then raise BDDPolygon "too few points"
      else if Array.length vertices = 2
      (* A line segment has zero mass. *)
      then { center = 0.5 *: (Array.sub(vertices, 0) :+: Array.sub(vertices, 1)),
             mass = 0.0,
             i = 0.0 }
      else
      let
          (* Polygon mass, centroid, and inertia.
             Let rho be the polygon density in mass per unit area.
             Then:
             mass = rho * int(dA)
             centroid.x = (1/mass) * rho * int(x * dA)
             centroid.y = (1/mass) * rho * int(y * dA)
             I = rho * int((x*x + y*y) * dA)

             We can compute these integrals by summing all the integrals
             for each triangle of the polygon. To evaluate the integral
             for a single triangle, we make a change of variables to
             the (u,v) coordinates of the triangle:
             x = x0 + e1x * u + e2x * v
             y = y0 + e1y * u + e2y * v
             where 0 <= u && 0 <= v && u + v <= 1.

             We integrate u from [0,1-v] and then v from [0,1].
             We also need to use the Jacobian of the transformation:
             D = cross(e1, e2)

             Simplification: triangle centroid = (1/3) * (p1 + p2 + p3)

             The rest of the derivation is handled by computer algebra. *)
          val center = ref (vec2 (0.0, 0.0))
          val area = ref 0.0
          val i = ref 0.0

          (* Port note: A lot of this function is the same as a compute_centroid. 
             I didn't try to merge them, to better mimic Box2D organization. *)
          (* ppef is the reference point for forming triangles.
             Its location doesn't change the result (except for rounding error). *)
          val pref = vec2 (0.0, 0.0)
          val inv3 = 1.0 / 3.0

          val () =
          Array.appi
          (fn (idx, a) =>
           let
               (* Triangle vertices. *)
               val p1 = pref
               val p2 = a
               val p3 = if idx + 1 < Array.length vertices
                        then Array.sub(vertices, idx + 1)
                        else Array.sub(vertices, 0)
               val e1 = p2 :-: p1
               val e2 = p3 :-: p1
               val d : real = cross2vv (e1, e2)
               val triangle_area : real = 0.5 * d

               val (px, py) = vec2xy p1
               val (ex1, ey1) = vec2xy e1
               val (ex2, ey2) = vec2xy e2

               (* PERF: Factor out px* and py* in third term? IEEE surprises? *)
               val intx2 = inv3 * (0.25 * (ex1 * ex1 +
                                           ex2 * ex1 +
                                           ex2 * ex2) +
                                   (px * ex1 + 
                                    px * ex2)) + 0.5 * px * px
               val inty2 = inv3 * (0.25 * (ey1 * ey1 +
                                           ey2 * ey1 +
                                           ey2 * ey2) +
                                   (py * ey1 + 
                                    py * ey2)) + 0.5 * py * py
           in
               area := !area + triangle_area;
               (* area-weighted centroid *)
               center := !center :+: ((triangle_area * inv3) *: (p1 :+: p2 :+: p3));
               i := !i + d * (intx2 + inty2)
           end) vertices
          
          (* Total mass *)
          val mass = density * !area
          
          (* Center of mass *)
          val center =
              if !area < epsilon
              then raise BDDPolygon "area not positive"
              else (1.0 / !area) *: !center

          (* Inertia tensor relative to the local origin. *)
          val i = density * !i
      in
          { mass = mass, center = center, i = i }
      end

end
