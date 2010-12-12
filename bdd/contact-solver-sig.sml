(* Copyright 2010 Tom Murphy VII and Erin Catto. See COPYING for details. *)

(* Internal module for solving contacts, which is used by the island
   solver (also internal!). Clients should ignore this module.

   Corresponding to dynamics/contacts/b2contactsolver.h *)
signature BDDCONTACT_SOLVER =
sig

  exception BDDContactSolver of string

  (* Parameterized by user data, since it uses the internal
     polymorpic types. *)
  type ('b, 'f, 'j) contact_solver

  (* contact_solver (contacts, impulse_ratio) 
     Port note: This also includes the call to 'warmStart'. *)
  val contact_solver : ('b, 'f, 'j) BDDDynamics.contact Vector.vector *
                       real ->
                       ('b, 'f, 'j) contact_solver
     
  val solve_velocity_constraints : ('b, 'f, 'j) contact_solver -> unit
  val store_impulses : ('b, 'f, 'j) contact_solver -> unit

  (* solve_position_constraints (solver, baumgarte) *)
  val solve_position_constraints : ('b, 'f, 'j) contact_solver * real -> bool

  (* Apply the function to every contact, paired with all of its
     impulses. *)
  (* Port note: In Box2D, the Report function loops over
     all of the contact constraints of the solver. This
     function is used to implement the loop without
     exposing the internal types. *)
  val app_contacts : 
      ('b, 'f, 'j) contact_solver *
      (('b, 'f, 'j) BDDDynamics.contact * 
       { normal_impulses : real array,
         tangent_impulses : real array } -> unit) -> unit

end
