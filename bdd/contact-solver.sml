(* Copyright 2010 Tom Murphy VII and Erin Catto. See COPYING for details. *)

(* Corresponding to dynamics/contacts/b2contactsolver.cpp *)
structure BDDContactSolver :> BDDCONTACT_SOLVER =
struct
  exception BDDContactSolver of string

  type contact_constraint_point =
      { local_point : BDDMath.vec2,
        r_a : BDDMath.vec2,
        r_b : BDDMath.vec2,
        normal_impulse : real,
        tangent_impulse : real,
        normal_mass : real,
        tangent_mass : real,
        velocity_bias : real }

  type ('b, 'f, 'j) contact_constraint =
      { points : contact_constraint_point Array.array,
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
  type ('b, 'f, 'j) contact_solver = unit

  fun contact_solver 
      (contacts : ('b, 'f, 'j) BDDDynamics.contact Vector.vector,
       impulse_ratio : real) : ('b, 'f, 'j) contact_solver =
      raise BDDContactSolver "unimplemented"     
      (* (don't forget to also warm-start) *)

  fun solve_velocity_constraints 
      (solver : ('b, 'f, 'j) contact_solver) : unit =
      raise BDDContactSolver "unimplemented"

  fun store_impulses (solver : ('b, 'f, 'j) contact_solver) : unit =
      raise BDDContactSolver "unimplemented"

  fun solve_position_constraints (solver : ('b, 'f, 'j) contact_solver,
				  baumgarte : real) : bool =
      raise BDDContactSolver "unimplemented"

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
