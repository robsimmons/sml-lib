(* Copyright 2010 Tom Murphy VII and Erin Catto. See COPYING for details. *)

(* Distance calculation using GJK algorithm.
   Corresponding to collision/b2distance.h *)
signature BDDDISTANCE =
sig

    val initial_cache : unit -> BDDTypes.simplex_cache

    val shape_proxy : BDDShape.shape -> BDDTypes.distance_proxy

    (* Computes the closest points between the arbitrary shapes in the
       input. The simplex cache is modified in the process, making
       incremental updates faster. *)
    val distance : BDDTypes.distance_input * BDDTypes.simplex_cache ->
                   BDDTypes.distance_output
end
