(* Copyright 2010 Tom Murphy VII and Erin Catto. See COPYING for details. *)

(* Corresponding to dynamics/b2island.cpp. *)
structure BDDIsland :> BDDISLAND =
struct

  exception BDDIsland of string

  (* Port note: Passing world instead of contact listener, as
     the listener is flattened into that object. *)
  fun solve_island (bodies : ('b, 'f, 'j) BDDDynamics.body list,
                    contacts : ('b, 'f, 'j) BDDDynamics.contact list,
                    joints : ('b, 'f, 'j) BDDDynamics.joint list,
                    world : ('b, 'f, 'j) BDDDynamics.world) : unit =
      raise BDDIsland "unimplemented"



end