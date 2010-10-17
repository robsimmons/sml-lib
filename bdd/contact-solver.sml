(* Copyright 2010 Tom Murphy VII and Erin Catto. See COPYING for details. *)

(* Corresponding to dynamics/contacts/b2contactsolver.cpp *)
structure BDDContactSolver :> BDDCONTACT_SOLVER =
struct

  exception BDDContactSolver of string
  open BDDMath BDDTypes BDDSettings BDDOps
  infix 6 :+: :-: %-% %+% +++
  infix 7 *: *% +*: +*+ #*% @*:
  structure D = BDDDynamics

  type constraint_point =
      { local_point : BDDMath.vec2,
        r_a : BDDMath.vec2,
        r_b : BDDMath.vec2,
        normal_impulse : real,
        tangent_impulse : real,
        normal_mass : real,
        tangent_mass : real,
        velocity_bias : real }

  type ('b, 'f, 'j) constraint =
      { points : constraint_point Array.array,
        local_normal : BDDMath.vec2,
        local_point : BDDMath.vec2,
        normal : BDDMath.vec2,
        normal_mass : BDDMath.mat22,
        k : BDDMath.mat22,
        body_a : ('b, 'f, 'j) BDDDynamics.body,
        body_b : ('b, 'f, 'j) BDDDynamics.body,
        typ : BDDTypes.manifold_type,
        radius : real,
        friction : real,
        point_count : int,
        manifold : BDDTypes.manifold }

  (* Parameterized by user data, since it uses the internal
     polymorpic types. *)
  type ('b, 'f, 'j) contact_solver =
      { constraints : ('b, 'f, 'j) constraint array
	(* XXX ... *) }
	

  fun contact_solver 
      (contacts : ('b, 'f, 'j) BDDDynamics.contact Vector.vector,
       impulse_ratio : real) : ('b, 'f, 'j) contact_solver =
    let
	(* Convert a contact into a constraint. *)
	fun onecontact (contact : ('b, 'f, 'j) BDDDynamics.contact) =
	  let
	    val fixture_a = D.C.get_fixture_a contact
	    val fixture_b = D.C.get_fixture_b contact
	    val shape_a = D.F.get_shape fixture_a
	    val shape_b = D.F.get_shape fixture_b
	    val radius_a = BDDShape.get_radius shape_a
	    val radius_b = BDDShape.get_radius shape_b
	    val body_a = D.F.get_body fixture_a
	    val body_b = D.F.get_body fixture_b
	    val manifold = D.C.get_manifold contact

	    val friction = mix_friction(D.F.get_friction fixture_a,
					D.F.get_friction fixture_b)
	    val restitution = mix_restitution(D.F.get_restitution fixture_a,
					      D.F.get_restitution fixture_b)
	    val v_a : vec2 = D.B.get_linear_velocity body_a
	    val v_b : vec2 = D.B.get_linear_velocity body_b
	    val w_a : real = D.B.get_angular_velocity body_a
	    val w_b : real = D.B.get_angular_velocity body_b

	    (* PERF assert *)
	    val () = if #point_count manifold > 0
		     then ()
		     else raise BDDContactSolver "pointcount assertion"

	    val world_manifold = 
		BDDCollision.create_world_manifold (manifold,
						    D.B.get_xf body_a, radius_a,
						    D.B.get_xf body_b, radius_b)

	    val normal = #normal world_manifold

	    fun one_point (j : int) : constraint_point =
	      let
		val cp : manifold_point = Array.sub (#points manifold, j)
		val wmpj = Array.sub(#points world_manifold, j)
		val r_a = wmpj :-: sweepc (D.B.get_sweep body_a)
		val r_b = wmpj :-: sweepc (D.B.get_sweep body_b)
		val rn_a = cross2vv(r_a, normal)
		val rn_b = cross2vv(r_b, normal)
		val rn_a = rn_a * rn_a
		val rn_b = rn_b * rn_b
		val k_normal : real = 
		    D.B.get_inv_mass body_a +
		    D.B.get_inv_mass body_b +
		    D.B.get_inv_i body_a * rn_a +
		    D.B.get_inv_i body_b * rn_b

		(* PERF assert *)
		val () = if k_normal > epsilon
			 then ()
			 else raise BDDContactSolver "knormal assertion"

		val tangent : vec2 = cross2vs(normal, 1.0)
		val rt_a = cross2vv(r_a, tangent)
		val rt_b = cross2vv(r_b, tangent)
		val rt_a = rt_a * rt_a
		val rt_b = rt_b * rt_b

		val k_tangent = 
		    D.B.get_inv_mass body_a +
		    D.B.get_inv_mass body_b +
		    D.B.get_inv_i body_a * rt_a +
		    D.B.get_inv_i body_b * rt_b

		(* PERF assert *)
		val () = if k_tangent > epsilon
			 then ()
			 else raise BDDContactSolver "ktangent assertion"

	        (* Set up a velocity bias for restitution. *)
		val v_rel : real = dot2(normal, 
					v_b :+: cross2sv(w_b, r_b) :-:
					v_a :-: cross2sv(w_a, r_a))
		val velocity_bias =
		  if v_rel < ~ velocity_threshold
		  then ~restitution * v_rel
		  else 0.0
	      in
		{ normal_impulse = impulse_ratio * #normal_impulse cp,
		  tangent_impulse = impulse_ratio * #tangent_impulse cp,
		  local_point = #local_point cp,
		  r_a = r_a,
		  r_b = r_b,
		  normal_mass = 1.0 / k_normal,
		  tangent_mass = 1.0 / k_tangent,
		  velocity_bias = velocity_bias } : constraint_point

	      end

	    val points = Array.tabulate(Array.length (#points manifold),
					one_point)

	    val (k, normal_mass, points) =
		(* If we have two points, then prepare the block solver. *)
		if Array.length points = 2
		then 
		  let
		      val ccp1 = Array.sub(points, 0)
		      val ccp2 = Array.sub(points, 1)

		      val inv_mass_a = D.B.get_inv_mass body_a
		      val inv_i_a = D.B.get_inv_i body_a
		      val inv_mass_b = D.B.get_inv_mass body_b
		      val inv_i_b = D.B.get_inv_i body_b

		      val rn1_a = cross2vv(#r_a ccp1, normal)
		      val rn1_b = cross2vv(#r_b ccp1, normal)
		      val rn2_a = cross2vv(#r_a ccp2, normal)
		      val rn2_b = cross2vv(#r_b ccp2, normal)

		      val k11 = inv_mass_a + inv_mass_b + 
			  inv_i_a * rn1_a * rn1_a +
			  inv_i_b * rn1_b * rn1_b
		      val k22 = inv_mass_a + inv_mass_b +
			  inv_i_a * rn2_a * rn2_a + 
			  inv_i_b * rn2_b * rn2_b
		      val k12 = inv_mass_a + inv_mass_b +
			  inv_i_a * rn1_a * rn2_a +
			  inv_i_b * rn1_b * rn2_b

		      (* Ensure a reasonable condition number. *)
		      val MAX_CONDITION_NUMBER = 100.0
		  in
		      if k11 * k11 < MAX_CONDITION_NUMBER * (k11 * k22 - k12 * k12)
		      then (* K is safe to invert. *)
			  let
			      val k = mat22cols (vec2(k11, k12), vec2(k12, k22))
			      val norm = mat22inverse k
			  in
			      (k, norm, points)
			  end
		      else
			  (* The constraints are redundant; just use one.
			     TODO_ERIN: use deepest? *)
			  (mat22with (0.0, 0.0, 0.0, 0.0),
			   mat22with (0.0, 0.0, 0.0, 0.0),
			   Array.tabulate(1, fn _ => Array.sub(points, 0)))

		  end
		      (* PERF uninitialized *)
		else (mat22with (0.0, 0.0, 0.0, 0.0), 
		      mat22with (0.0, 0.0, 0.0, 0.0), 
		      points)
		
	  in
	      { body_a = body_a,
		body_b = body_b,
		manifold = manifold,
		normal = #normal world_manifold,
		point_count = Array.length points,
		friction = friction,
		local_normal = #local_normal manifold,
		local_point = #local_point manifold,
		radius = radius_a + radius_b,
		typ = #typ manifold,
		points = points,
		k = k,
		normal_mass = normal_mass }
	  end
	val constraints = 
	    Array.tabulate (Vector.length contacts,
			    fn x => onecontact (Vector.sub(contacts, x)))

	fun warm_start_one (c as { body_a, body_b,
				   normal, points, ... } 
			    : ('b, 'f, 'j) constraint) : unit =
	  let
	    val tangent = cross2vs(normal, 1.0)
	    val inv_mass_a = D.B.get_inv_mass body_a
	    val inv_i_a = D.B.get_inv_i body_a
	    val inv_mass_b = D.B.get_inv_mass body_b
	    val inv_i_b = D.B.get_inv_i body_b

	    fun warm_point (ccp as { normal_impulse, 
				     tangent_impulse, 
				     r_a, r_b, ... } : constraint_point) : unit =
	      let
		val p : vec2 = normal_impulse *: normal :+: tangent_impulse *: tangent
	      in
		D.B.set_angular_velocity (body_a,
					  D.B.get_angular_velocity body_a -
					  inv_i_a * cross2vv(r_a, p));
		D.B.set_linear_velocity (body_a,
					 D.B.get_linear_velocity body_a :-:
					 inv_mass_a *: p);
		D.B.set_angular_velocity (body_b,
					  D.B.get_angular_velocity body_b -
					  inv_i_b * cross2vv(r_b, p));
		D.B.set_linear_velocity (body_b,
					 D.B.get_linear_velocity body_b :-:
					 inv_mass_b *: p)
	      end
	  in
	    Array.app warm_point points
	  end
    in
	(* Port note: Rolled the warm start function, which is always
	   called immediately after initialization, into this. *)
	Array.app warm_start_one constraints;
	{ constraints = constraints }
    end


  fun solve_velocity_constraints 
      (solver : ('b, 'f, 'j) contact_solver) : unit =
      raise BDDContactSolver "unimplemented"

  fun store_impulses (solver : ('b, 'f, 'j) contact_solver) : unit =
      raise BDDContactSolver "unimplemented"
(*
{
	for (int32 i = 0; i < m_constraintCount; ++i)
	{
		b2ContactConstraint* c = m_constraints + i;
		b2Manifold* m = c->manifold;

		for (int32 j = 0; j < c->pointCount; ++j)
		{
			m->points[j].normalImpulse = c->points[j].normalImpulse;
			m->points[j].tangentImpulse = c->points[j].tangentImpulse;
		}
	}
}
*)

  (* Port note: A class in Box2D; it's just a function that
     returns multiple values.

     Note, this is almost the same function as in toi-solver.
     (Redundancy is present in Box2D too.) *)
  fun contact_solver_manifold (cc : ('b, 'f, 'j) constraint, index : int) :
      { normal : vec2, point : vec2, separation : real } =
    case #typ cc of
        E_Circles =>
          let
              val point_a : vec2 = 
                  D.B.get_world_point (#body_a cc,
                                       #local_point cc)
              val point_b : vec2 = 
                  D.B.get_world_point (#body_b cc,
				       #local_point
                                       (Array.sub (#points cc, 0)))

              val normal =
                if distance_squared (point_a, point_b) > epsilon * epsilon
                then vec2normalized (point_b :-: point_a)
                else vec2 (1.0, 0.0)
          in
              { normal = normal,
                point = 0.5 *: (point_a :+: point_b),
                separation = dot2(point_b :-: point_a, normal) - #radius cc }
          end
    | E_FaceA =>
          let
              val normal = D.B.get_world_vector (#body_a cc,
                                                 #local_normal cc)
              val plane_point : vec2 =
                  D.B.get_world_point(#body_a cc, #local_point cc)
              val clip_point : vec2 =
                  D.B.get_world_point(#body_b cc, #local_point 
				      (Array.sub(#points cc,
						 index)))
              val separation : real =
                  dot2(clip_point :-: plane_point, normal) - #radius cc
          in
              { normal = normal,
                separation = separation,
                point = clip_point }
          end
    | E_FaceB =>
          let
              val normal = D.B.get_world_vector (#body_b cc,
                                                 #local_normal cc)
              val plane_point : vec2 =
                  D.B.get_world_point(#body_b cc, #local_point cc)
              val clip_point : vec2 =
                  D.B.get_world_point(#body_a cc, #local_point
				      (Array.sub(#points cc,
						 index)))
              val separation : real =
                  dot2(clip_point :-: plane_point, normal) - #radius cc
          in
              (* Ensure normal points from A to B. *)
              { normal = vec2neg normal,
                separation = separation,
                point = clip_point }
          end

  (* Sequential solver.
     
     Port note: This is nearly identical to the code in toi-solver, so
     if you change something here, it probably should be changed there
     too. The duplication comes from Box2D. Obviously it would be
     better to factor out this common routine. *)
  fun solve_position_constraints (solver : ('b, 'f, 'j) contact_solver,
				  baumgarte : real) : bool =
    let

      val min_separation = ref 0.0
      fun oneconstraint (c : ('b, 'f, 'j) constraint) =
        let
            val body_a = #body_a c
            val body_b = #body_b c

            val inv_mass_a = D.B.get_mass body_a * D.B.get_inv_mass body_a
            val inv_i_a = D.B.get_mass body_a * D.B.get_inv_i body_a
            val inv_mass_b = D.B.get_mass body_b * D.B.get_inv_mass body_b
            val inv_i_b = D.B.get_mass body_b * D.B.get_inv_i body_b

            (* Solve normal constraints. *)
        in
            for 0 (#point_count c - 1)
            (fn j =>
             let
                 val { normal : vec2, point : vec2, separation : real } =
                     contact_solver_manifold (c, j)

                 val r_a : vec2 = point :-: sweepc (D.B.get_sweep body_a)
                 val r_b : vec2 = point :-: sweepc (D.B.get_sweep body_b)

                 (* Track max constraint error. *)
                 val () = if separation < !min_separation
                          then min_separation := separation
                          else ()

                 (* Prevent large corrections and allow slop. *)
                 val capital_c : real = 
                     clampr (baumgarte * (separation + linear_slop),
                             ~max_linear_correction,
                             0.0)
                 (* Compute the effective mass. *)
                 val rn_a : real = cross2vv (r_a, normal)
                 val rn_b : real = cross2vv (r_b, normal)
                 val k : real = inv_mass_a + inv_mass_b + 
                     inv_i_a * rn_a * rn_a +
                     inv_i_b * rn_b * rn_b

                 (* Compute normal impulse. *)
                 val impulse : real = if k > 0.0 then ~ capital_c / k else 0.0
                 val p : vec2 = impulse *: normal

                 fun update_sweep (body, inv_mass, inv_i, r) =
                   let
                     val sweep : sweep = D.B.get_sweep body
                   in
                     sweep_set_a (sweep, sweepa sweep - 
                                  (inv_i * cross2vv (r, p)));
                     sweep_set_c (sweep, sweepc sweep :-: (inv_mass *: p));
                     D.B.synchronize_transform body
                   end
             in
                 update_sweep (body_a, inv_mass_a, inv_i_a, r_a);
                 update_sweep (body_b, inv_mass_b, inv_i_b, r_b)
             end)
        end
    in
      Array.app oneconstraint (#constraints solver);
      (* We can't expect minSpeparation >= -b2_linearSlop because we don't
         push the separation above -b2_linearSlop. *)
      !min_separation >= ~1.5 * linear_slop
    end


  fun app_contacts ({ contacts, ... } : ('b, 'f, 'j) contact_solver, 
		    f : ('b, 'f, 'j) BDDDynamics.contact * 
		        { normal_impulses : real array,
			  tangent_impulses : real array } -> unit) : unit =
      raise BDDContactSolver "unimplemented"
(* (from b2island.cpp)
	for (int32 i = 0; i < m_contactCount; ++i)
	{
		b2Contact* c = m_contacts[i];

		const b2ContactConstraint* cc = constraints + i;
		
		b2ContactImpulse impulse;
		for (int32 j = 0; j < cc->pointCount; ++j)
		{
			impulse.normalImpulses[j] = cc->points[j].normalImpulse;
			impulse.tangentImpulses[j] = cc->points[j].tangentImpulse;
		}

		m_listener->PostSolve(c, &impulse);
	}
*)

end
