(* Copyright 2010 Tom Murphy VII and Erin Catto. See COPYING for details. *)

(* A dynamic AABB tree for the broad-phase collision detection.
   Corresponding to collision/b2dynamictree.h. *)
signature BDDDYNAMIC_TREE =
sig

  exception BDDDynamicTree of string

  (* A dynamic tree arranges data in a binary tree to accelerate queries
     such as volume queries and ray casts. Leafs are proxies with an
     AABB. In the tree we expand the proxy AABB by b2_fatAABBFactor so
     that the proxy AABB is bigger than the client object. This allows
     the client object to move by small amounts without triggering a
     tree update. *)
  type 'a aabb_proxy
  type 'a dynamic_tree

  val dynamic_tree : unit -> 'a dynamic_tree
  
  (* Create a proxy. Provide a tight fitting AABB and user data. *)
  val aabb_proxy : 'a dynamic_tree * BDDTypes.aabb * 'a -> 'a aabb_proxy

  val eq_proxy : 'a aabb_proxy * 'a aabb_proxy -> bool
  val cmp_proxy : 'a aabb_proxy * 'a aabb_proxy -> order

  (* Remove the proxy from the tree. It must be a leaf in this tree. *)  
  val remove_proxy : 'a dynamic_tree * 'a aabb_proxy -> unit
  
  (* move_proxy tree proxy aabb displacement
     Move a proxy with a swepted AABB. If the proxy has moved outside of
     its fattened AABB, then the proxy is removed from the tree and
     re-inserted. Otherwise the function returns immediately. Returns
     true if the proxy was re-inserted. *)
  val move_proxy : 'a dynamic_tree * 'a aabb_proxy * 
                   BDDTypes.aabb * BDDMath.vec2 -> 
                   bool

  (* rebalance tree iterations
     Perform some iterations to re-balance the tree. *)
  val rebalance : 'a dynamic_tree * int -> unit
  
  (* Get proxy user data. *)
  val user_data : 'a aabb_proxy -> 'a

  (* Get the fat AABB for a proxy. *)
  val fat_aabb : 'a aabb_proxy -> BDDTypes.aabb
  
  (* Compute the height of the tree. *)
  val compute_height : 'a dynamic_tree -> int
  
  (* query tree callback aabb
     Query the tree for proxies that overlap the aabb. The callback
     is applied to each proxy that overlaps the supplied aabb,
     in arbitrary order. If it returns false, stops early. *)
  val query : 'a dynamic_tree * ('a aabb_proxy -> bool) * BDDTypes.aabb -> unit

  (* ray_cast tree callback { p1, p2, max_fraction }

     Ray-cast against the proxies in the tree. This relies on the
     callback to perform an exact ray-cast in the case were the proxy
     contains a shape. The callback also performs the collision
     filtering. This has performance roughly equal to k * log(n),
     where k is the number of collisions and n is the number of
     proxies in the tree.

     The input ray extends from p1 to p1 + maxFraction * (p2 - p1).
     The callback is called for each proxy that is hit by the ray. A
     return value of 0.0 means to stop. *)
  val ray_cast : 'a dynamic_tree * 
                 (BDDTypes.ray_cast_input * 'a aabb_proxy -> real) *
                 BDDTypes.ray_cast_input -> unit

  (* Just for debugging. *)
  val debugprint : ('a -> string) -> 'a dynamic_tree -> unit

end