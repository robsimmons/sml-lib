(* Copyright 2010 Tom Murphy VII and Erin Catto. See COPYING for details. *)

(* Distance calculation using GJK algorithm.
   Corresponding to collision/b2distance.cpp *)
structure BDDDistance :> BDDDISTANCE =
struct
  open BDDSettings
  open BDDTypes
  open BDDMath val distance = ()
  open BDDOps
  infix 6 :+: :-: %-% %+% +++
  infix 7 *: *% +*: +*+ #*% @*:

  exception BDDDistance

  (* Port note: I took a different strategy here than in Box2D.
     In the case of circles, these functions become very simple
     (just one vertex), so I implemented them directly rather
     than storing a vector of vertices and doing the polygon
     case. This is probably significantly faster, for circles. *)
  fun shape_proxy (BDDShape.Circle { radius, p }) =
      { vertex_count = 1,
        vertex = (fn _ => p),
        support = (fn _ => 0),
        support_vertex = (fn _ => p),
        radius = radius }

    | shape_proxy (BDDShape.Polygon (p as { vertices, normals, centroid })) =
      (* PERF could just use intrinsic length, like elsewhere. *)
      { vertex_count = Array.length vertices,
        vertex = (fn idx => Array.sub(vertices, idx)),
        (* Port note: Box2Dia reimplements these functions here; seems
           more natural to just use the polygon, which we have anyway *)
        support = (fn v => BDDPolygon.get_support (p, v)),
        support_vertex = (fn v => BDDPolygon.get_support_vertex (p, v)),
        (* Port note: constant. *)
        radius = BDDSettings.polygon_radius }

  fun initial_cache () =
      { metric = ref 0.0,
        count = ref 0,
        indexa = Array.array(3, 0),
        indexb = Array.array(3, 0) }

  type simplex_vertex =
      {
       (* support point in proxy a, b *)
       wa : vec2,
       wb : vec2,
       (* wb - wa *)
       w : vec2,
       (* barycentric coordinate for closest point *)
       a : real,
       (* wa, wb index *)
       indexa : int,
       indexb : int
      }

  fun set_a ({ wa, wb, w, a, indexa, indexb }, aa) =
      { wa = wa, wb = wb, w = w, a = aa, 
        indexa = indexa, indexb = indexb }

  fun zero_vertex () = { wa = vec2(0.0, 0.0),
                         wb = vec2(0.0, 0.0),
                         w = vec2(0.0, 0.0),
                         a = ref 0.0,
                         indexa = 0,
                         indexb = 0 }
  datatype simplex = 
      Zero
    | One of simplex_vertex
    | Two of simplex_vertex * simplex_vertex
    | Three of simplex_vertex * simplex_vertex * simplex_vertex

  fun simplex_metric (One _) = 0.0
    | simplex_metric (Two (v1, v2)) = BDDMath.distance(#w v1, #w v2)
    | simplex_metric (Three (v1, v2, v3)) = 
      cross2vv(#w v2 :-: #w v1, #w v3 :-: #w v1)
    | simplex_metric _ = raise BDDDistance

  fun read_cache (cache : simplex_cache,
                  proxya : distance_proxy, transforma,
                  proxyb : distance_proxy, transformb) : simplex =
      let fun new() =
          let val wa = transforma @*: #vertex proxya 0
              val wb = transformb @*: #vertex proxyb 0
          in
              One { indexa = 0,
                    indexb = 0,
                    wa = wa,
                    wb = wb,
                    w = wb :-: wa,
                    (* PERF uninitialized in Box2D *)
                    a = 0.0 }
          end
      in
        if !(#count cache) = 0
        then new()
        else
            let
                fun init i =
                    let 
                        val indexa = Array.sub(#indexa cache, i)
                        val indexb = Array.sub(#indexb cache, i)
                        val wa = transforma @*: #vertex proxya indexa
                        val wb = transformb @*: #vertex proxyb indexb
                    in
                        { indexa = indexa,
                          indexb = indexb,
                          wa = wa,
                          wb = wb,
                          a = 0.0,
                          w = wb :-: wa }
                    end

                val simplex = 
                    case !(#count cache) of
                        1 => One (init 0)
                      | 2 => Two (init 0, init 1)
                      | 3 => Three (init 0, init 1, init 2)
                      | _ => raise BDDDistance

                val metric1 = !(#metric cache)
                val metric2 = simplex_metric simplex
            in
                (* If the new simplex metric is substantially different than
                   old metric then flush the simplex. *)
                if metric2 < 0.5 * metric1 orelse
                   2.0 * metric1 < metric2 orelse
                   metric2 < epsilon
                then new ()
                else simplex
            end
      end

  fun simplex_count Zero = 0
    | simplex_count (One _) = 1
    | simplex_count (Two _) = 2
    | simplex_count (Three _) = 3

  fun write_cache (simplex, cache : simplex_cache) : unit =
      let 
          fun write i (v : simplex_vertex) =
              (Array.update(#indexa cache, i, #indexa v);
               Array.update(#indexb cache, i, #indexb v))
      in
          #metric cache := simplex_metric simplex;
          #count cache := simplex_count simplex;

          case simplex of
              Zero => ()
            | One v1 => write 0 v1
            | Two (v1, v2) => (write 0 v1; write 1 v2)
            | Three (v1, v2, v3) => (write 0 v1; write 1 v2; write 2 v3)
      end

  fun simplex_search_direction simplex : vec2 =
      case simplex of
          One v => vec2neg (#w v)
        | Two (v1, v2) =>
              let val e12 : vec2 = #w v2 :-: #w v1
                  val sgn : real = cross2vv(e12, vec2neg (#w v1))
              in
                  if sgn > 0.0
                  then (* Origin is left of e12. *)
                      cross2sv(1.0, e12)
                  else (* Origin is right of e12. *)
                      cross2vs(e12, 1.0)
              end
        | _ => raise BDDDistance

  fun simplex_closest_point (simplex : simplex) : vec2 =
      case simplex of
          One v1 => #w v1
        | Two (v1, v2) => #a v1 *: #w v1 :+: #a v2 *: #w v2
        | Three _ => vec2_zero
        | _ => raise BDDDistance

  fun simplex_witness_points (simplex : simplex) : vec2 * vec2 =
      case simplex of
          One v1 => (#wa v1, #wb v1)
        | Two (v1, v2) => (#a v1 *: #wa v1 :+: #a v2 *: #wa v2,
                           #a v1 *: #wb v1 :+: #a v2 *: #wb v2)
        | Three (v1, v2, v3) => 
              let val p = #a v1 *: #wa v1 :+: #a v2 *: #wa v2 :+: #a v3 *: #wa v3
              in (p, vec2copy p)
              end
        | _ => raise BDDDistance

  (* Solve a line segment using barycentric coordinates.
     
     p = a1 * w1 + a2 * w2
     a1 + a2 = 1
     
     The vector from the origin to the closest point on the line is
     perpendicular to the line.
     e12 = w2 - w1
     dot(p, e) = 0
     a1 * dot(w1, e) + a2 * dot(w2, e) = 0
     
     2-by-2 linear system
     [1      1     ][a1] = [1]
     [w1.e12 w2.e12][a2] = [0]
     
     Define
     d12_1 =  dot(w2, e12)
     d12_2 = -dot(w1, e12)
     d12 = d12_1 + d12_2
     
     Solution
     a1 = d12_1 / d12
     a2 = d12_2 / d12 *)
  fun simplex_solve2 (v1 : simplex_vertex, v2 : simplex_vertex) : simplex =
      let val w1 = #w v1
          val w2 = #w v2
          val e12 = w2 :-: w1

          val d12_2 : real = ~(dot2(w1, e12))
      in
          if d12_2 <= 0.0
          then (* w1 region *)
              (* a2 <= 0, so we clamp it to 0 *)
              One (set_a (v1, 1.0))
          else
              let val d12_1 = dot2(w2, e12)
              in if d12_1 <= 0.0
                 then (* a1 <= 0, so we clamp it to 0 *)
                     One (set_a (v2, 1.0))
                 else (* Must be in e12 region. *)
                     let val inv_d12 = 1.0 / (d12_1 + d12_2)
                     in Two (set_a (v1, d12_1 * inv_d12),
                             set_a (v2, d12_2 * inv_d12))
                     end
              end
      end

  (* Possible regions:
     - points[2]
     - edge points[0]-points[2]
     - edge points[1]-points[2]
     - inside the triangle *)
  fun simplex_solve3 (v1 : simplex_vertex,
                      v2 : simplex_vertex,
                      v3 : simplex_vertex) : simplex =
      let
        val w1 = #w v1
        val w2 = #w v2
        val w3 = #w v3
        
        (* Edge12
           [1      1     ][a1] = [1]
           [w1.e12 w2.e12][a2] = [0]
           a3 = 0 *)
        val e12 = w2 :-: w1
        val w1e12 = dot2(w1, e12)
        val w2e12 = dot2(w2, e12)
        val d12_1 = w2e12
        val d12_2 = ~w1e12

        (* Edge13
           [1      1     ][a1] = [1]
           [w1.e13 w3.e13][a3] = [0]
           a2 = 0 *)
        val e13 = w3 :-: w1
        val w1e13 = dot2(w1, e13)
        val w3e13 = dot2(w3, e13)
        val d13_1 = w3e13
        val d13_2 = ~w1e13

        (* Edge23
           [1      1     ][a2] = [1]
           [w2.e23 w3.e23][a3] = [0]
           a1 = 0 *)
        val e23 = w3 :-: w2
        val w2e23 = dot2(w2, e23)
        val w3e23 = dot2(w3, e23)
        val d23_1 = w3e23
        val d23_2 = ~w2e23
        
        (* Triangle123 *)
        val n123 = cross2vv(e12, e13)

        val d123_1 : real = n123 * cross2vv(w2, w3)
        val d123_2 : real = n123 * cross2vv(w3, w1)
        val d123_3 : real = n123 * cross2vv(w1, w2)
      in
        if (d12_2 <= 0.0 andalso d13_2 <= 0.0)
        then (* w1 region *)
            One (set_a (v1, 1.0))
        else 
        if (d12_1 > 0.0 andalso d12_2 > 0.0 andalso d123_3 <= 0.0)
        then (* e12 *)
            let
                val inv_d12 : real = 1.0 / (d12_1 + d12_2)
            in
                Two (set_a (v1, d12_1 * inv_d12),
                     set_a (v2, d12_2 * inv_d12))
            end
        else
        if (d13_1 > 0.0 andalso d13_2 > 0.0 andalso d123_2 <= 0.0)
        then (* e13 *)
            let 
                val inv_d13 : real = 1.0 / (d13_1 + d13_2)
            in
                Two (set_a (v1, d13_1 * inv_d13),
                     set_a (v3, d13_2 * inv_d13))
            end
        else
        if (d12_1 <= 0.0 andalso d23_2 <= 0.0)
        then (* w2 region *)
            One (set_a (v2, 1.0)) 
        else
        if (d13_1 <= 0.0 andalso d23_1 <= 0.0)
        then (* w3 region *)
            One (set_a (v3, 1.0))
        else
        if (d23_1 > 0.0 andalso d23_2 > 0.0 andalso d123_1 <= 0.0)
        then (* e23 region *)
            let
                val inv_d23 = 1.0 / (d23_1 + d23_2)
            in
                Two (set_a (v3, d23_2 * inv_d23),
                     set_a (v2, d23_1 * inv_d23))
            end
        else (* Must be in triangle123 *)
            let
                val inv_d123 : real = 1.0 / (d123_1 + d123_2 + d123_3)
            in
                Three (set_a (v1, d123_1 * inv_d123),
                       set_a (v2, d123_2 * inv_d123),
                       set_a (v3, d123_3 * inv_d123))
            end
      end

  val MAX_ITERS = 20

  fun vertex_eq ({indexa, indexb, ...} : simplex_vertex, 
                 {indexa = ia, indexb = ib, ...} : simplex_vertex) =
      indexa = ia andalso indexb = ib

  fun simplex_has_point (simplex, new) =
      case simplex of
          Zero => false
        | One v1 => vertex_eq (v1, new)
        | Two (v1, v2) => vertex_eq (v1, new) orelse vertex_eq (v2, new)
        | Three (v1, v2, v3) => 
              vertex_eq (v1, new) orelse
              vertex_eq (v2, new) orelse
              vertex_eq (v3, new)

  (* Add the vertex to the end of the simplex. *)
  fun push_vertex (simplex : simplex, new : simplex_vertex) =
      case simplex of
          Zero => One new
        | One v => Two (v, new)
        | Two (v1, v2) => Three (v1, v2, new)
        | Three _ => raise BDDDistance

  fun distance ({ proxya : distance_proxy,
                  proxyb : distance_proxy,
                  transforma : BDDMath.transform,
                  transformb : BDDMath.transform,
                  use_radii : bool },
                cache : simplex_cache) : distance_output =
      let
          (* Initialize the simplex. *)
          val start_simplex = 
              read_cache(cache, proxya, transforma, proxyb, transformb)

          (* Port note: In the Box2D source code there are loop
             variables distance_sqr1 and 2. But they are dead,
             because the test labeled "ensure progress" is
             commented out. I'm trying to mimic the behavior
             of Box2D so I just removed them from this code
             for efficiency. *)
          fun loop (old_simplex : simplex, iter) =
            if iter >= MAX_ITERS
            then (old_simplex, iter)
            else
              let
                  val solved =
                      case old_simplex of
                          One _ => old_simplex
                        | Two (v1, v2) => simplex_solve2 (v1, v2)
                        | Three (v1, v2, v3) => simplex_solve3 (v1, v2, v3)
                        | _ => raise BDDDistance
              in
                  case solved of
                    (* If we have 3 points, then the origin is in 
                       the corresponding triangle. *)
                    Three _ => (solved, iter)
                  | _ =>
                  let (* Get search direction. *)
                      val d : vec2 = simplex_search_direction solved
                  in
                      (* Ensure the search direction is numerically fit. *)
                      if vec2length_squared d < epsilon * epsilon
                      then
                          (* The origin is probably contained by a line segment
                             or triangle. Thus the shapes are overlapped.

                             We can't return zero here even though
                             there may be overlap. In case the simplex
                             is a point, segment, or triangle it is
                             difficult to determine if the origin is
                             contained in the CSO or very close to it. *)
                          (solved, iter)
                      else
                      let
                          (* Compute a tentative new simplex vertex using 
                             support points. *)
                          val indexa =
                              #support proxya (mul_t22mv
                                               (transformr transforma,
                                                vec2neg d))
                          val wa = transforma @*: #vertex proxya indexa
                          val indexb =
                              #support proxyb (mul_t22mv
                                               (transformr transformb, d))
                          val wb = transformb @*: #vertex proxyb indexb
                          val new : simplex_vertex =
                              { indexa = indexa, wa = wa,
                                indexb = indexb, wb = wb,
                                w = wb :-: wa,
                                (* PERF uninitialized in Box2D *)
                                a = 0.0 }

                          (* iter counts the number of support point calls. *)
                          val iter = iter + 1
                      in
                          (* Check for duplicate support points. 
                             This is the main termination criteria. 

                             Port note: In the original this was stored in
                             a copy of the simplex. Since simplex is
                             functional in this version, I just compare
                             to old_simplex. *)
                          if simplex_has_point (old_simplex, new)
                          then (* If we found a duplicate support point we
                                  must exit to avoid cycling. *)
                              (solved, iter)
                          else loop(push_vertex (solved, new), iter)
                      end
                  end
              end
          
          val (simplex, iter) = loop(start_simplex, 0)

          val (pointa, pointb) = simplex_witness_points simplex
          val () = write_cache(simplex, cache)
          val distance = BDDMath.distance(pointa, pointb)
      in
          if use_radii
          then 
              let val ra = #radius proxya
                  val rb = #radius proxyb
              in
                  if distance > ra + rb andalso
                     distance > epsilon
                  then
                      (* Shapes are still not overlapped.
                         Move the witness points to the outer surface. *)
                      let val normal : vec2 = pointb :-: pointa
                      in 
                          ignore (vec2normalize normal : real);
                          { distance = distance - (ra + rb),
                            pointa = pointa :+: (ra *: normal),
                            pointb = pointb :-: (rb *: normal),
                            iterations = iter }
                      end
                  else
                      (* Shapes are overlapped when radii are considered.
                         Move the witness points to the middle. *)
                      let
                          val p : vec2 = 0.5 *: (pointa :+: pointb)
                      in
                          { pointa = p,
                            pointb = p,
                            distance = 0.0,
                            iterations = iter }
                      end
              end
          else { iterations = iter,
                 pointa = pointa,
                 pointb = pointb,
                 distance = distance }
      end

end

