(* Copyright 2010 Tom Murphy VII and Erin Catto. See COPYING for details. *)

(* The broad-phase is used for computing pairs and performing volume queries
   and ray casts. This broad-phase does not persist pairs. Instead, this
   reports potentially new pairs. It is up to the client to consume the
   new pairs and to track subsequent overlap.
   Corresponding to collision/b2broadphase.h. *)
signature BDDBROAD_PHASE =
sig

  (* A broadphase keeps track of a bunch of objects in 2D space (given
     axis-aligned bounding boxes) and lets you move them around. It
     supports the query and ray_cast operations from DynamicTree
     as well as a function that applies a callback to each pair of
     objects that might be (newly) colliding. *)
  type 'a broadphase

end
