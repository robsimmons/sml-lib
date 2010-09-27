(* Copyright 2010 Tom Murphy VII and Erin Catto. See COPYING for details. *)

(* Internal module for solving an island, which is a subgraph of
   the bodies that have been identified as interacting. Clients
   should ignore this module.

   Corresponding to dynamics/b2island.h. *)
signature BDDISLAND =
sig

  (* solve_island bodies contacts joints world time_step gravity allow_sleep 
     Modifies the inputs in place. *)
  (* Port note: Box2D uses stateful initialization (three different Add methods)
     and reserved capacities. All that the client code does is Clear, Add in a loop,
     and call Solve once. This can just be a function. *)
  val solve_island : ('b, 'f, 'j) BDDDynamics.body list *
                     ('b, 'f, 'j) BDDDynamics.contact list *
                     ('b, 'f, 'j) BDDDynamics.joint list *
                     ('b, 'f, 'j) BDDDynamics.world *
                     BDDDynamics.time_step *
                     BDDMath.vec2 *
                     bool ->
                     unit

end