(* Copyright 2010 Tom Murphy VII and Erin Catto. See COPYING for details. *)

(* This is a pure position solver for a single movable body in contact
   with multiple non-moving bodies. It's an internal module used only
   in the implementation of World.
   Corresponding to dynamics/contacts/b2toisolver.h. *)
signature BDDTOI_SOLVER =
sig

  type ('b, 'f, 'j) solver
  val solver : ('b, 'f, 'j) BDDDynamics.contact list *
      ('b, 'f, 'j) BDDDynamics.body -> ('b, 'f, 'j) solver
    
  (* solve (solver, baumgarte)
     Perform one solver iteration. Returns true if converged. *)
  val solve : ('b, 'f, 'j) solver * real -> bool

  (* Port note: Clear is never used and this is an internal
     module, so I left it out. *)

end