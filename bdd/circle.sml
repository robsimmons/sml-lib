(* Copyright 2010 Tom Murphy VII and Erin Catto. See COPYING for details. *)

(* Basic implementation of circle.
   Corresponding to collision/shapes/b2circleshape.cpp *)

structure BDDCircle :> BDDCIRCLE =
struct
  open BDDSettings
  open BDDTypes
  open BDDMath
  open BDDOps
  infix 6 :+: :-: %-% %+% +++
  infix 7 *: *% +*: +*+ #*% @*:

  exception BDDCircle

  type circle = { radius : real, p : vec2 }

  fun clone { radius, p } = { radius = radius, p = vec2copy p }

  fun get_support ({ radius, p }, _) = 0
  fun get_support_vertex ({ radius, p }, _) = p
  fun get_vertex ({ radius, p }, 0) = p
    | get_vertex _ = raise BDDCircle

  fun test_point ({ radius, p }, transform, pp) : bool =
      let val center = transformposition transform :+:
	      transformr transform +*: p
	  val d = pp :-: center
      in dot2 (d, d) <= radius * radius
      end

  (* Collision Detection in Interactive 3D Environments by Gino van den Bergen
     From Section 3.1.2
     x = s + a * r
     norm(x) = radius *)
  fun ray_cast ({ radius, p }, transform, { p1 : BDDMath.vec2, p2 : BDDMath.vec2,
					    max_fraction : real }) :
      BDDTypes.ray_cast_output option =
      let
	  val position = transformposition transform :+: transformr transform +*: p
	  val s : vec2 = p1 :-: position
	  val b : real = dot2(s, s) - radius * radius

	  (* Solve quadratic equation. *)
	  val r : vec2 = p2 :-: p1
	  val c : real = dot2 (s, r)
	  val rr : real = dot2 (r, r)
	  val sigma = c * c - rr * b

      in
	  (* Check for negative discriminant and short segment. *)
	  if sigma < 0.0 orelse rr < epsilon
	  then NONE
	  else
	      (* Find the point of intersection of the line with the circle. *)
	      let val a = ~(c + sqrt sigma)
	      in if 0.0 <= a andalso a <= max_fraction * rr
		 then let val a = a / rr
		          val normal = s :+: a *: r
		      in 
			  ignore (vec2normalize normal : real);
			  SOME { fraction = a, normal = normal }
		      end
		 else NONE
	      end
      end

  fun compute_aabb ({ radius, p }, transform : BDDMath.transform) : BDDTypes.aabb =
      let val p : vec2 = transformposition transform :+: 
	  transformr transform +*: p
      in
	  { lowerbound = vec2(vec2x p - radius, vec2y p - radius),
	    upperbound = vec2(vec2x p + radius, vec2y p + radius) }
      end

  fun compute_mass ({ radius, p }, density : real) : mass_data =
      let val mass = density * pi * radius * radius
      in
	  { mass = mass,
	    center = p,
	    (* inertia about the local origin *)
	    i = mass * (0.5 * radius * radius + dot2(p, p)) }
      end

  fun get_vertex_count _ = 1

end
