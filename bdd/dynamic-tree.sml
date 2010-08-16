(* Copyright 2010 Tom Murphy VII and Erin Catto. See COPYING for details. *)

(* A dynamic AABB tree for the broad-phase collision detection.
   Corresponding to collision/b2dynamictree.cpp. *)
structure BDDDynamicTree :> BDDDYNAMIC_TREE =
struct
  open BDDSettings
  open BDDTypes
  open BDDMath
  open BDDOps
  infix 6 :+: :-: %-% %+% +++
  infix 7 *: *% +*: +*+ #*% @*:

  exception BDDDynamicTree of string

  (* Port note: Corresponds to DynamicTreeNode in b2dynamictree.h.
     I decided to use more idiomatic SML instead of the hand-written
     allocator in Box2D, since it's not clear it'd be faster than
     a good generational gc. *)
  datatype 'a tree_node =
      Node of { (* Fattened aabb *)
                aabb : aabb,
                (* Set for leaves; NONE for internal nodes. *)
                data : 'a option,
                (* Port note: Box2D has a possibility for
                   a 'next' pointer here, but it's just
                   so that the structure can be stored 
                   in freelists for its custom allocator. *)
                parent : 'a tree_node ref,
                left : 'a tree_node ref,
                right : 'a tree_node ref }
    | Empty
  type 'a aabb_proxy = 'a tree_node ref

  (* Port note: The representation is that leaf nodes are the
     "real" nodes (and have user data) whereas internal nodes just
     union up leaves to arrange them hierarchically, and are expendable.
     It is probably worth having both Interior and Leaf arms rather 
     than Node and Empty. *)
  type 'a dynamic_tree =
      { node_count : int,
        root : 'a aabb_proxy } ref

  (* Port note: We need to update fields of each object to mimic the
     imperative style of Box2D. We treat the object itself as a ref,
     rather than the updatable fields (the latter would be more
     idiomatic in SML). This allows us to quickly compare the objects
     for equality, but means that we need setter functions for the
     fields that we modify. *)
  fun set_node_count (r as ref { node_count = _, root }, nc) =
      r := { node_count = nc, root = root }

  fun set_root (r as ref { node_count, root = _ }, root) =
      r := { node_count = node_count, root = root }

  fun set_parent (r as ref (Node { aabb, data, parent = _, left, right }),
                  parent) =
      r := Node { aabb = aabb, data = data, parent = parent, 
                  left = left, right = right }
    | set_parent _ = raise BDDDynamicTree "expected node; got empty"

  fun set_left (r as ref (Node { aabb, data, parent, left = _, right }),
                left) =
      r := Node { aabb = aabb, data = data, parent = parent, 
                  left = left, right = right }
    | set_left _ = raise BDDDynamicTree "expected node; got empty"

  fun set_right (r as ref (Node { aabb, data, parent, left, right = _ }),
                 right) =
      r := Node { aabb = aabb, data = data, parent = parent, 
                  left = left, right = right }
    | set_right _ = raise BDDDynamicTree "expected node; got empty"

  fun !! (ref (Node x)) = x
    | !! _ = raise BDDDynamicTree "expected node; got empty"

  fun user_data n = 
      case #data (!! n) of
          NONE => raise BDDDynamicTree "data only present for leaves"
        | SOME a => a
  fun fat_aabb n = #aabb (!! n)

  (* There is one empty node. - won't work - value restriction.
     could make one as part of every tree... *)
  (* val empty = ref Empty *)
  (* Assumes there is one empty node *)
  fun is_leaf n = (* #left (!!n) = empty *)
      case #left (!!n) of
          ref Empty => true
        | _ => false

  fun compute_height (ref { root, ... } : 'a dynamic_tree) =
      let fun ch Empty = 0
            | ch (Node { left, right, ... }) =
          1 + Int.max(ch (!left), ch (!right))
      in ch (!root)
      end

  fun dynamic_tree () = ref { node_count = 0, root = ref Empty }

  (* Derive the AABB for an interior node, based on its children
     (which must have accurate AABBs. Set it and return it. *)
  fun set_derived_aabb (r as ref (Node { aabb = _, data, parent, 
                                         left, right })) =
      let val new_aabb =
          BDDCollision.aabb_combine (#aabb (!!left), #aabb (!!right))
      in r := Node { aabb = new_aabb, data = data, parent = parent, 
                     left = left, right = right };
          new_aabb
      end
    | set_derived_aabb _ = raise BDDDynamicTree "expected node; got empty"

  (* Climb the tree starting at the given node, expanding the derived
     AABBs if necessary.
     Port note: In the original, usually inlined as a do..while loop.
     PERF: Easy to merge this and the above to remove some repeated
     tests (though they are probably optimized out). *)
  fun adjust_aabbs ancestor =
      case !ancestor of
          Empty => ()
        | Node { parent, aabb = old_aabb, ... } => 
           let val new_aabb = set_derived_aabb ancestor
           in if BDDCollision.aabb_contains (old_aabb, new_aabb)
              then ()
              else adjust_aabbs parent
           end

  fun insert_leaf (tree as ref { root, ... } : 'a dynamic_tree,
                   leaf as ref (Node { aabb, data, parent, left, right })) =
      (case !root of
           Empty => 
               let in
                   (* PERF should always be the case already? *)
                   set_parent (leaf, ref Empty);
                   set_root (tree, leaf)
               end
         | _ =>
            (* Find the best sibling for this leaf. *)
            let 
                val center : vec2 = BDDCollision.aabb_center aabb
                fun find sibling =
                  if is_leaf sibling
                  then sibling
                  else
                    let
                      val left = #left (!!sibling)
                      val right = #right (!!sibling)
                      val ldelta : vec2 = 
                        vec2abs (BDDCollision.aabb_center (#aabb (!!left)) :-: center)
                      val rdelta : vec2 =
                        vec2abs (BDDCollision.aabb_center (#aabb (!!right)) :-: center)
                      val lnorm = vec2x ldelta + vec2y ldelta
                      val rnorm = vec2x rdelta + vec2y rdelta
                    in
                      if lnorm < rnorm
                      then find left
                      else find right
                    end
                val sibling = find root
                val parent = #parent (!!sibling)
                val new = ref (Node { parent = parent,
                                      data = NONE,
                                      aabb = BDDCollision.aabb_combine 
                                          (#aabb (!!leaf),
                                           #aabb (!!sibling)),
                                      (* Port note: Same in both branches. *)
                                      left = sibling,
                                      right = leaf })
            in
                set_parent (sibling, new);
                set_parent (leaf, new);

                case !parent of
                  Empty => set_root (tree, new)
                | _ => 
                  let 
                      
                  in
                      if #left (!!parent) = sibling
                      then set_left (parent, new)
                      else set_right (parent, new);

                      (* Port note: This expansion routine is not exactly 
                         the same as the one in the code, but I believe
                         it has equivalent effect. *)
                      adjust_aabbs parent
                  end
            end)
    | insert_leaf _ = raise BDDDynamicTree "can't insert empty"

  fun rebalance iters =
      raise Unimplemented "rebalance"
(*
void b2DynamicTree::Rebalance(int32 iterations)
{
        if (m_root == b2_nullNode)
        {
                return;
        }

        for (int32 i = 0; i < iterations; ++i)
        {
                int32 node = m_root;

                uint32 bit = 0;
                while (m_nodes[node].IsLeaf() == false)
                {
                        int32* children = &m_nodes[node].child1;
                        node = children[(m_path >> bit) & 1];
                        bit = (bit + 1) & (8* sizeof(uint32) - 1);
                }
                ++m_path;

                RemoveLeaf(node);
                InsertLeaf(node);
        }
}
*)

  fun aabb_proxy (tree : 'a dynamic_tree, aabb : aabb, a : 'a) : 'a aabb_proxy =
      let
          (* Fatten the aabb. *)
          val r : vec2 = vec2(aabb_extension, aabb_extension)
          val fat : aabb = { lowerbound = #lowerbound aabb :-: r,
                             upperbound = #upperbound aabb :-: r }
          (* XXX: Probably don't need to pass all this junk to
             insert_node. *)
          val node = ref (Node { aabb = fat, data = SOME a, parent = ref Empty,
                                 left = ref Empty, right = ref Empty })
          fun rebalance_loop try_count =
              if try_count >= 10 orelse compute_height tree <= 64
              then node
              else (rebalance (#node_count (!tree) div 16);
                    rebalance_loop (try_count + 1))
      in
          set_node_count (tree, #node_count (!tree) + 1);
          insert_leaf (tree, node);
          (* Rebalance if necessary. *)
          rebalance_loop 0
      end

  (* Assumes the proxy is a leaf. *)
  fun remove_leaf (tree (* : 'a dynamic_tree *), 
                   proxy (* : 'a aabb_proxy *)) =
    let val { parent, ... } = !!proxy
    in
      (* If it's the root, we just make the tree empty. 
         Port note: Throughout this code, Box2D uses equality
         on proxy IDs (integers); I ref equality. *)
      case !parent of
          Empty => set_root (tree, ref Empty)
        | Node { left, right, parent = grandparent, ... } =>
            let
                (* Get the other child of our parent. *)
                val sibling = if left = proxy 
                              then right
                              else left
            in
              case !grandparent of
                  (* Note: discards parent. *)
                  Empty => (set_parent (sibling, ref Empty);
                            set_root (tree, sibling))
                | Node { left = g_left, ... } => 
                      let 
                      in
                          (* Destroy parent and connect grandparent
                             to sibling. *)
                          if g_left = sibling
                          then set_left (grandparent, sibling)
                          else set_right (grandparent, sibling);
                          set_parent (sibling, grandparent);
                          (* Adjust ancestor bounds. *)
                          adjust_aabbs grandparent
                      end
            end
    end

  fun remove_proxy (tree : 'a dynamic_tree, proxy : 'a aabb_proxy) =
      if is_leaf proxy
      then remove_leaf (tree, proxy)
      else raise BDDDynamicTree "can only remove leaves"

  fun move_proxy (tree : 'a dynamic_tree,
                  proxy as ref (Node { aabb = proxy_aabb, data, ... }) : 'a aabb_proxy,
                  aabb : aabb,
                  displacement : vec2) : bool =
      if BDDCollision.aabb_contains (proxy_aabb, aabb)
      then false
      else
        let
            val () = if is_leaf proxy
                     then ()
                     else raise BDDDynamicTree "move_proxy on non-leaf"
            val () = remove_leaf (tree, proxy)

            (* Predict AABB displacement. *)
            val d : vec2 = aabb_multiplier *: displacement

            val (blx, bux) = if vec2x d < 0.0
                             then (vec2x d, 0.0)
                             else (0.0, vec2x d)
            val (bly, buy) = if vec2y d < 0.0
                             then (vec2y d, 0.0)
                             else (0.0, vec2y d)

            (* Extend AABB *)
            val b : aabb = { lowerbound = 
                               #lowerbound aabb :-: displacement :+: vec2(blx, bly),
                             upperbound = 
                               #upperbound aabb :+: displacement :+: vec2(bux, buy) }
        in
            proxy := Node { aabb = b, data = data, parent = ref Empty,
                            left = ref Empty, right = ref Empty };
            insert_leaf (tree, proxy);
            true
        end
    | move_proxy _ = raise BDDDynamicTree "move_proxy on Empty"

  fun query (tree: 'a dynamic_tree,
             f : 'a aabb_proxy -> bool,
             aabb : aabb) : unit =
      raise Unimplemented "query"

(*

template <typename T>
inline void b2DynamicTree::Query(T* callback, const b2AABB& aabb) const
{
        const int32 k_stackSize = 128;
        int32 stack[k_stackSize];

        int32 count = 0;
        stack[count++] = m_root;

        while (count > 0)
        {
                int32 nodeId = stack[--count];
                if (nodeId == b2_nullNode)
                {
                        continue;
                }

                const b2DynamicTreeNode* node = m_nodes + nodeId;

                if (b2TestOverlap(node->aabb, aabb))
                {
                        if (node->IsLeaf())
                        {
                                bool proceed = callback->QueryCallback(nodeId);
                                if (proceed == false)
                                {
                                        return;
                                }
                        }
                        else
                        {
                                if (count < k_stackSize)
                                {
                                        stack[count++] = node->child1;
                                }

                                if (count < k_stackSize)
                                {
                                        stack[count++] = node->child2;
                                }
                        }
                }
        }
}
*)


  fun ray_cast (tree : 'a dynamic_tree,
                callback : BDDTypes.ray_cast_input * 'a aabb_proxy -> real,
                { p1 : BDDMath.vec2, p2 : BDDMath.vec2,
                  max_fraction : real }) : unit =
      raise Unimplemented "ray_cast"

(*
template <typename T>
inline void b2DynamicTree::RayCast(T* callback, const b2RayCastInput& input) const
{
        b2Vec2 p1 = input.p1;
        b2Vec2 p2 = input.p2;
        b2Vec2 r = p2 - p1;
        b2Assert(r.LengthSquared() > 0.0f);
        r.Normalize();

        // v is perpendicular to the segment.
        b2Vec2 v = b2Cross(1.0f, r);
        b2Vec2 abs_v = b2Abs(v);

        // Separating axis for segment (Gino, p80).
        // |dot(v, p1 - c)| > dot(|v|, h)

        float32 maxFraction = input.maxFraction;

        // Build a bounding box for the segment.
        b2AABB segmentAABB;
        {
                b2Vec2 t = p1 + maxFraction * (p2 - p1);
                segmentAABB.lowerBound = b2Min(p1, t);
                segmentAABB.upperBound = b2Max(p1, t);
        }

        const int32 k_stackSize = 128;
        int32 stack[k_stackSize];

        int32 count = 0;
        stack[count++] = m_root;

        while (count > 0)
        {
                int32 nodeId = stack[--count];
                if (nodeId == b2_nullNode)
                {
                        continue;
                }

                const b2DynamicTreeNode* node = m_nodes + nodeId;

                if (b2TestOverlap(node->aabb, segmentAABB) == false)
                {
                        continue;
                }

                // Separating axis for segment (Gino, p80).
                // |dot(v, p1 - c)| > dot(|v|, h)
                b2Vec2 c = node->aabb.GetCenter();
                b2Vec2 h = node->aabb.GetExtents();
                float32 separation = b2Abs(b2Dot(v, p1 - c)) - b2Dot(abs_v, h);
                if (separation > 0.0f)
                {
                        continue;
                }

                if (node->IsLeaf())
                {
                        b2RayCastInput subInput;
                        subInput.p1 = input.p1;
                        subInput.p2 = input.p2;
                        subInput.maxFraction = maxFraction;

                        float32 value = callback->RayCastCallback(subInput, nodeId);

                        if (value == 0.0f)
                        {
                                // The client has terminated the ray cast.
                                return;
                        }

                        if (value > 0.0f)
                        {
                                // Update segment bounding box.
                                maxFraction = value;
                                b2Vec2 t = p1 + maxFraction * (p2 - p1);
                                segmentAABB.lowerBound = b2Min(p1, t);
                                segmentAABB.upperBound = b2Max(p1, t);
                        }
                }
                else
                {
                        if (count < k_stackSize)
                        {
                                stack[count++] = node->child1;
                        }

                        if (count < k_stackSize)
                        {
                                stack[count++] = node->child2;
                        }
                }
        }
}
*)

end
