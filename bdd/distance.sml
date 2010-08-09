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
(* XXX polygons
    | shape_proxy (BDDShape.Polygon
        case b2Shape::e_polygon:
                {
                        const b2PolygonShape* polygon = (b2PolygonShape* )shape;
                        m_vertices = polygon->m_vertices;
                        m_count = polygon->m_vertexCount;
                        m_radius = polygon->m_radius;
                }
                break;


inline int32 b2DistanceProxy::GetVertexCount() const
{
        return m_count;
}

inline const b2Vec2& b2DistanceProxy::GetVertex(int32 index) const
{
        b2Assert(0 <= index && index < m_count);
        return m_vertices[index];
}

inline int32 b2DistanceProxy::GetSupport(const b2Vec2& d) const
{
        int32 bestIndex = 0;
        float32 bestValue = b2Dot(m_vertices[0], d);
        for (int32 i = 1; i < m_count; ++i)
        {
                float32 value = b2Dot(m_vertices[i], d);
                if (value > bestValue)
                {
                        bestIndex = i;
                        bestValue = value;
                }
        }

        return bestIndex;
}

inline const b2Vec2& b2DistanceProxy::GetSupportVertex(const b2Vec2& d) const
{
        int32 bestIndex = 0;
        float32 bestValue = b2Dot(m_vertices[0], d);
        for (int32 i = 1; i < m_count; ++i)
        {
                float32 value = b2Dot(m_vertices[i], d);
                if (value > bestValue)
                {
                        bestIndex = i;
                        bestValue = value;
                }
        }

        return m_vertices[bestIndex];
}
*)

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
       a : real ref,
       (* wa, wb index *)
       indexa : int,
       indexb : int
      }

  fun zero_vertex () = { wa = vec2(0.0, 0.0),
                         wb = vec2(0.0, 0.0),
                         w = vec2(0.0, 0.0),
                         a = ref 0.0,
                         indexa = 0,
                         indexb = 0 }

  type simplex = { v1 : simplex_vertex,
                   v2 : simplex_vertex,
                   v3 : simplex_vertex,
                   count : int ref }

  fun simplex_metric { v1, v2, v3, count } =
      case !count of
          1 => 0.0
        | 2 => BDDMath.distance(#w v1, #w v2)
        | 3 => cross2vv(#w v2 :-: #w v1, #w v3 :-: #w v1)
        | _ => raise BDDDistance

  fun read_cache (cache : simplex_cache,
                  proxya : distance_proxy, transforma,
                  proxyb : distance_proxy, transformb) : simplex =
      let fun new() =
          let val wa = transforma @*: #vertex proxya 0
              val wb = transformb @*: #vertex proxyb 0
          in
          { count = ref 1,
            v1 = { indexa = 0,
                   indexb = 0,
                   wa = wa,
                   wb = wb,
                   w = wb :-: wa,
                   (* PERF uninitialized in Box2D *)
                   a = ref 0.0 },
            (* PERF uninitialized in Box2D *)
            v2 = zero_vertex (),
            v3 = zero_vertex () }
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
                          a = ref 0.0,
                          w = wb :-: wa }
                    end

                val simplex = { v1 = init 0,
                                v2 = init 1,
                                v3 = init 3,
                                count = ref (!(#count cache)) }

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

  fun write_cache (simplex as 
                   { v1 : simplex_vertex,
                     v2 : simplex_vertex,
                     v3 : simplex_vertex,
                     count : int ref }, cache : simplex_cache) : unit =
      let in
          #metric cache := simplex_metric simplex;
          #count cache := !count;
          Array.update(#indexa cache, 0, #indexa v1);
          Array.update(#indexb cache, 0, #indexb v1);

          Array.update(#indexa cache, 1, #indexa v2);
          Array.update(#indexb cache, 1, #indexb v2);

          Array.update(#indexa cache, 2, #indexa v3);
          Array.update(#indexb cache, 2, #indexb v3)
      end

  fun simplex_search_direction ({ v1, v2, v3, count } : simplex) : vec2 =
      case !count of 
          1 => vec2neg (#w v1)
        | 2 =>
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

  fun simplex_closest_point ({ v1, v2, v3, count } : simplex) : vec2 =
      case !count of
          1 => #w v1
        | 2 => #a v1 *: #w v1 :+: #a v2 *: #w v2
        | 3 => vec2_zero
        | _ => raise BDDDistance

  fun simplex_witness_points ({ v1, v2, v3, count } : simplex) : vec2 * vec2 =
      case !count of
          1 => (#wa v1, #wb v1)
        | 2 => (#a v1 *: #wa v1 :+: #a v2 *: #wa v2,
                #a v1 *: #wb v1 :+: #a v2 *: #wb v2)
        | 3 => let val p = #a v1 *: #wa v1 :+: #a v2 *: #wa v2 :+: #a v3 *: #wa v3
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
  fun simplex_solve2 ({ v1, v2, v3, count } : simplex) : unit =
      let val w1 = #w v1
          val w2 = #w v2
          val e12 = w2 :-: w1

          val d12_2 : real = -dot2(w1, e12)
      in
          if d12_2 <= 0.0
          then (* w1 region *)
              // a2 <= 0, so we clamp it to 0
                m_v1.a = 1.0f;
                m_count = 1;
                return;
          else
              let val d12_1 = dot(w2, e12)
              in if d12_1 <= 0.0
                 then (* a1 <= 0, so we clamp it to 0 *)
                     let in 
                         #a v2 := 1.0;
                         count := 1;
                         vec2setfrom (v1, m_v2;
                         return;
                     end
                 else (* Must be in e12 region. *)
                     let val inv_d12 = 1.0 / (d12_1 + d12_2)
                     in
                         #a v1 := d12_1 * inv_d12;
                         #a v2 := d12_2 * inv_d12;
                         count := 2
                     end
              end
      end

  (* fun simplex_solve2 ({ v1, v2, v3, count } : simplex) = raise BDDDistance *)

end

