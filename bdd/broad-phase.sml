(* Copyright 2010 Tom Murphy VII and Erin Catto. See COPYING for details. *)

(* Implementation of broad phase.
   Corresponding to collision/b2broadphase.cpp and templates in the header. *)
structure BDDBroadPhase :> BDDBROAD_PHASE =
struct
  open BDDSettings
  open BDDTypes
  open BDDMath
  open BDDOps
  infix 6 :+: :-: %-% %+% +++
  infix 7 *: *% +*: +*+ #*% @*:

  exception BDDBroadPhase of string

  structure DT = BDDDynamicTree

  type 'a proxy = 'a DT.aabb_proxy

  (* Port note: Box2D representation includes a vector of points,
     but this is just C++ callback confusion. It's just a locally
     allocated ref in a closure for this code (see query_callback
     and update_pairs). *)
  datatype 'a broadphase =
      BP of { tree : 'a DT.dynamic_tree,
	      count : int ref,
	      (* Enqueued moves for the next update_pairs. *)
	      move_buffer : 'a proxy list ref }

  (* Port note: ? *)

(*

void b2BroadPhase::BufferMove(int32 proxyId)
{
	if (m_moveCount == m_moveCapacity)
	{
		int32* oldBuffer = m_moveBuffer;
		m_moveCapacity *= 2;
		m_moveBuffer = (int32* )b2Alloc(m_moveCapacity * sizeof(int32));
		memcpy(m_moveBuffer, oldBuffer, m_moveCount * sizeof(int32));
		b2Free(oldBuffer);
	}

	m_moveBuffer[m_moveCount] = proxyId;
	++m_moveCount;
}

void b2BroadPhase::UnBufferMove(int32 proxyId)
{
	for (int32 i = 0; i < m_moveCount; ++i)
	{
		if (m_moveBuffer[i] == proxyId)
		{
			m_moveBuffer[i] = e_nullProxy;
			return;
		}
	}
}

*)

  (* nb. doesn't remove existing moves.
     Port note: Stored in the opposite order from Box2D, which
     uses a growing array. *)
  fun buffer_move (BP { move_buffer, ... }, p : 'a proxy) : unit =
      move_buffer := p :: !move_buffer

  (* PERF Original just blanks elements (null_proxy) that match.
     Might be faster if we switch to using an array here. *)
  fun unbuffer_move (BP { move_buffer, ... }, p : 'a proxy) : unit =
      move_buffer := List.filter (fn q => not (DT.eq_proxy (p, q))) (!move_buffer)

  fun broadphase () = BP { tree = DT.dynamic_tree (), count = ref 0,
			   move_buffer = ref nil }

  fun create_proxy (bp as BP { tree, count, ... }, aabb : aabb, a : 'a) : 'a proxy =
      let 
	  val p = DT.aabb_proxy (tree, aabb, a)
      in
	  count := !count + 1;
	  buffer_move (bp, p);
	  p
      end

  fun remove_proxy (bp as BP { tree, count, ... } : 'a broadphase, p : 'a proxy) : unit =
      let in
	  unbuffer_move (bp, p);
	  count := !count - 1;
	  DT.remove_proxy (tree, p)
      end

  fun move_proxy (bp as BP { tree, ... } : 'a broadphase, 
		  p : 'a proxy, aabb, displacement) : unit =
      let 
	  val should_buffer = DT.move_proxy(tree, p, aabb, displacement)
      in
	  if should_buffer
	  then buffer_move (bp, p)
	  else ()
      end

  (* Compare pairs lexicographically. *)
  fun cmp_pairs ((a, b), (aa, bb)) =
      case DT.cmp_proxy (a, aa) of
	  EQUAL => DT.cmp_proxy (b, bb)
	| order => order

  val fat_aabb = DT.fat_aabb
  val user_data = DT.user_data
  fun compute_height (BP { tree, ... }) = DT.compute_height tree


  fun test_overlap (p, q) : bool =
      BDDCollision.aabb_overlap (fat_aabb p, fat_aabb q)

  fun proxy_count (BP { count, ... }) = !count

  fun query (BP { tree, ... }, callback, aabb) : unit =
      DT.query (tree, callback, aabb)

  fun ray_cast (BP { tree, ...}, callback, input) : unit =
      DT.ray_cast (tree, callback, input)

  (* Copied from sml-lib to reduce code dependencies and encourage
     inlining.
     
     Sorts a list from least to greatest, and arbitrarily discards
     duplicates.

     PERF: could be optimized to do less consing (split). *)
  fun sort_unique cmp l =
      let
	  fun split l =
	      let fun s a1 a2 nil = (a1, a2)
		    | s a1 a2 (h :: t) = s a2 (h :: a1) t
	      in s nil nil l
	      end

	  fun merge a nil = a
	    | merge nil b = b
	    | merge (aa as (a :: ta)) (bb as (b :: tb)) =
	      case cmp (a, b) of
		  EQUAL => (a :: merge ta tb)
		| LESS => (a :: merge ta bb)
		| GREATER => (b :: merge aa tb)

	  fun ms nil = nil
	    | ms [s] = [s]
	    | ms [a, b] = merge [a] [b]
	    | ms ll = 
	      let val (a, b) = split ll
	      in merge (ms a) (ms b)
	      end
      in ms l
      end


  fun update_pairs (bp as BP { tree, move_buffer, ... } : 'a broadphase, 
		    add : 'a * 'a -> unit) : unit =
      let
	  
	  (* PERF: Maybe should use a growarray for this.
	     Port note: This is a member variable of broadphase in Box2D, but
	     cleared right here. *)
	  val pairs = ref nil : ('a proxy * 'a proxy) list ref 

	  (* Perform tree queries for all moving proxies. *)
	  val () = List.app
	      (fn query_proxy =>
	       let
		   (* We have to query the tree with the fat AABB so that
		      we don't fail to create a pair that may touch later. *)
		   val fat_aabb = fat_aabb query_proxy

		   fun query_callback (p : 'a proxy) : bool =
		       let in
			   case DT.cmp_proxy (query_proxy, p) of
			       (* A proxy cannot form a pair with itself. *)
			       EQUAL => ()
			     (* Otherwise, put in canonical order (less, greater) *)
			     | LESS => pairs := (query_proxy, p) :: !pairs
			     | GREATER => pairs := (p, query_proxy) :: !pairs;
			   true
		       end
	       in
		   (* Query tree, create pairs and add them to the pair buffer. *)
		   DT.query (tree, query_callback, fat_aabb)
	       end) (rev (!move_buffer))

	  (* Reset move buffer *)
	  val () = move_buffer := nil

	  (* Sort the pair buffer and remove duplicates. *)
	  val pairs = sort_unique cmp_pairs (!pairs)
      in
	  (* Send the pairs back to the client. *)
	  app (fn (p, q) => add (user_data p, user_data q)) pairs;

	  (* Try to keep the tree balanced. *)
	  DT.rebalance (tree, 4)
      end

end
