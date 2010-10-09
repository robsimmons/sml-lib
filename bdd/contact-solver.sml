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
        normal_mass : real,
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
      raise BDDContactSolver "unimplemented"     
      (* (don't forget to also warm-start):


{
	// Warm start.
	for (int32 i = 0; i < m_constraintCount; ++i)
	{
		b2ContactConstraint* c = m_constraints + i;

		b2Body* bodyA = c->bodyA;
		b2Body* bodyB = c->bodyB;
		float32 invMassA = bodyA->m_invMass;
		float32 invIA = bodyA->m_invI;
		float32 invMassB = bodyB->m_invMass;
		float32 invIB = bodyB->m_invI;
		b2Vec2 normal = c->normal;
		b2Vec2 tangent = b2Cross(normal, 1.0f);

		for (int32 j = 0; j < c->pointCount; ++j)
		{
			b2ContactConstraintPoint* ccp = c->points + j;
			b2Vec2 P = ccp->normalImpulse * normal + ccp->tangentImpulse * tangent;
			bodyA->m_angularVelocity -= invIA * b2Cross(ccp->rA, P);
			bodyA->m_linearVelocity -= invMassA * P;
			bodyB->m_angularVelocity += invIB * b2Cross(ccp->rB, P);
			bodyB->m_linearVelocity += invMassB * P;
		}
	}
}

 *)


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


  fun app_contacts (cs, f) : unit =
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
