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

  (* An object in the broadphase collision detector. *)
  type 'a proxy

  (* Create an empty broadphase. *)
  val broadphase : unit -> 'a broadphase

  (* Add a proxy with an initial AABB to the broadphase. Pairs are not
     reported until UpdatePairs is called. *)
  val create_proxy : 'a broadphase * BDDTypes.aabb * 'a -> 'a proxy

  (* Remove a proxy from the broad phase. It is up to the client to
     remove any pairs. *)
  val remove_proxy : 'a broadphase * 'a proxy -> unit

  (* move_proxy (broadphase, proxy, aabb, displacement)
     Call MoveProxy as many times as you like, then when you are done
     call UpdatePairs to finalize the proxy pairs (for your time step). *)
  val move_proxy : 'a broadphase * 'a proxy * BDDTypes.aabb * BDDMath.vec2 -> 
                   unit

  (* Get the fat AABB for a proxy. *)
  val fat_aabb : 'a proxy -> BDDTypes.aabb

  (* Get user data from a proxy. *)
  val user_data : 'a proxy -> 'a

  (* Test overlap of fat AABBs. *)
  val test_overlap : 'a proxy * 'a proxy -> bool

  (* Get the number of proxies. *)
  val proxy_count : 'a broadphase -> int

  (* update_pairs (broadphase, add_pair)
     Update the pairs, by calling the callback. This can only add pairs. *)
  val update_pairs : 'a broadphase * ('a * 'a -> unit) -> unit

  (* query (broadphase, callback, aabb)
     Query an AABB for overlapping proxies. The callback
     is called on each proxy that overlaps the supplied AABB. If it returns
     false, then the query stops early. *)
  val query : 'a broadphase * ('a proxy -> bool) * BDDTypes.aabb -> unit

  (* ray_cast broadphase callback { p1, p2, max_fraction }

     Ray-cast against the proxies in the broad phase; same as the
     embedded dynamic tree. This relies on the callback to perform an
     exact ray-cast in the case were the proxy contains a shape. The
     callback also performs the collision filtering. This has
     performance roughly equal to k * log(n), where k is the number of
     collisions and n is the number of proxies in the tree.

     The input ray extends from p1 to p1 + maxFraction * (p2 - p1).
     The callback is called for each proxy that is hit by the ray. A return
     value of 0.0 means to stop. *)
  val ray_cast : 'a broadphase * 
                 (BDDTypes.ray_cast_input * 'a proxy -> real) * 
                 BDDTypes.ray_cast_input -> unit

  (* Compute the height of the embedded tree. *)
  val compute_height : 'a broadphase -> int

end
