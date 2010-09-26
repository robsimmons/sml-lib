(* Copyright 2010 Tom Murphy VII and Erin Catto. See COPYING for details. *)

(* Corresponding to dynamics/b2island.cpp. *)
structure BDDIsland :> BDDISLAND =
struct

  exception BDDIsland of string

  (* Port note: Passing world instead of contact listener, as
     the listener is flattened into that object. *)
  (* Port note: The list arguments arrive reversed relative
     to the order they are added in Box2D code. *)
  fun solve_island (bodies : ('b, 'f, 'j) BDDDynamics.body list,
                    contacts : ('b, 'f, 'j) BDDDynamics.contact list,
                    joints : ('b, 'f, 'j) BDDDynamics.joint list,
                    world : ('b, 'f, 'j) BDDDynamics.world) : unit =
      raise BDDIsland "unimplemented"

  (* XXX: In World I assume that solve_island does not modify the
     presence of bodies in its internal array; Box2D iterates over
     them at the end to remove the island flag from static bodies. *)

end