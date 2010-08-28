(* Copyright 2010 Tom Murphy VII and Erin Catto. See COPYING for details. *)

(* Time of impact calculation.
   Corresponding to common/b2timeofimpact.h. *)
signature BDDTIME_OF_IMPACT =
sig

  datatype state =
      SUnknown
    | SFailed
    | SOverlapped
    | STouching
    | SSeparated

  (* Compute the upper bound on time before two shapes penetrate. Time
     is represented as a fraction between [0,tMax]. This uses a swept
     separating axis and may miss some intermediate, non-tunneling
     collision. If you change the time interval, you should call this
     function again.

     Note: use BDDDistance.distance to compute the contact point and
     normal at the time of impact. *)
  val time_of_impact : { proxya : BDDTypes.distance_proxy,
                         (* XXX has to be same type? *)
                         proxyb : BDDTypes.distance_proxy,
                         sweepa : BDDMath.sweep,
                         sweepb : BDDMath.sweep,
                         (* Defines sweep interval [0, tmax] *)
                         tmax : real } -> state * real

end