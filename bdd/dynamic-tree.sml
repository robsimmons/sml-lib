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
     I decided to use idiomatic SML instead of the hand-written
     allocator in Box2D, since it's not clear it'd be faster than
     a good generational gc. *)
  datatype 'a tree_node =
      Node of { (* Fattened aabb *)
                aabb : aabb,
                data : 'a,
                (* Port note: Box2D has a possibility for
                   a 'next' pointer here, but it's just
                   so that the structure can be stored 
                   in freelists for its custom allocator. *)
                parent : 'a tree_node ref,
                left : 'a tree_node ref,
                right : 'a tree_node ref }
    | Empty
  type 'a aabb_proxy = 'a tree_node ref

  (* Port note: The representation appears to be that leaf nodes are the
     "real" data (and have user data) whereas internal nodes just
     union up leaves to arrange them hierarchically, and are expendable.
     If this is true, it may be worth having both Interior and Leaf
     arms rather than Node and Empty. *)
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

  fun user_data n = #data (!! n)
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

  fun insert_leaf (tree as ref { root, ... } : 'a dynamic_tree,
                   node as ref (Node { aabb, data, parent, left, right })) =
      raise Unimplemented "insert_leaf"

  fun rebalance iters =
      raise Unimplemented "rebalance"

  fun aabb_proxy (tree (* : 'a dynamic_tree *), aabb : aabb, a (* : 'a*) ) 
      (* : 'a aabb_proxy *) =
      let
          (* Fatten the aabb. *)
          val r : vec2 = vec2(aabb_extension, aabb_extension)
          val fat : aabb = { lowerbound = #lowerbound aabb :-: r,
                             upperbound = #upperbound aabb :-: r }
          (* XXX: Probably don't need to pass all this junk to
             insert_node. *)
          val node = ref (Node { aabb = fat, data = a, parent = ref Empty,
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

  (* Derive the AABB for an interior node, based on its children
     (which must have accurate AABBs. Set it and return it. *)
  fun set_derived_aabb (r as ref (Node { aabb = _, data, parent, 
                                         left, right }))
      let val new_aabb = 
          BDDCollision.aabb_combine (#aabb (!!left), #aabb (!!right))
      in r := Node { aabb = new_aabb, data = data, parent = parent, 
                     left = left, right = right }
      end
    | set_derived_aabb _ = raise BDDDynamicTree "expected node; got empty"

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
                                fun adjust ancestor =
                                  case !ancestor of
                                      Empty => ()
                                    | Node { parent, aabb = old_aabb, ... } => 
                                      let 
                                        val new_aabb = 
                                            set_derived_aabb grandparent
                                      in
                                        if BDDCollision.aabb_contains (old_aabb, new_aabb)
                                        then ()
                                        else adjust parent
                                      end
                            in
                                (* Destroy parent and connect grandparent
                                   to sibling. *)
                                if g_left = sibling
                                then set_left (grandparent, sibling)
                                else set_right (grandparent, sibling);
                                set_parent (sibling, grandparent);
                                (* Adjust ancestor bounds. *)
                                adjust grandparent
                            end
                end
      end

  fun remove_proxy (tree : 'a dynamic_tree, proxy : 'a aabb_proxy) =
      if is_leaf proxy
      then remove_leaf (tree, proxy)
      else raise BDDDynamicTree "can only remove leaves"

  fun move_proxy (tree : 'a dynamic_tree,
                  proxy : 'a aabb_proxy,
                  aabb : aabb,
                  displacement : vec2) : bool =
      raise Unimplemented "move_proxy"

  fun query (tree: 'a dynamic_tree,
             f : 'a aabb_proxy -> bool,
             aabb : aabb) : unit =
      raise Unimplemented "query"

  fun ray_cast (tree : 'a dynamic_tree,
                callback : BDDTypes.ray_cast_input * 'a aabb_proxy -> real,
                { p1 : BDDMath.vec2, p2 : BDDMath.vec2,
                  max_fraction : real }) : unit =
      raise Unimplemented "ray_cast"

(*

bool b2DynamicTree::MoveProxy(int32 proxyId, const b2AABB& aabb, const b2Vec2& displacement)
{
        b2Assert(0 <= proxyId && proxyId < m_nodeCapacity);

        b2Assert(m_nodes[proxyId].IsLeaf());

        if (m_nodes[proxyId].aabb.Contains(aabb))
        {
                return false;
        }

        RemoveLeaf(proxyId);

        // Extend AABB.
        b2AABB b = aabb;
        b2Vec2 r(b2_aabbExtension, b2_aabbExtension);
        b.lowerBound = b.lowerBound - r;
        b.upperBound = b.upperBound + r;

        // Predict AABB displacement.
        b2Vec2 d = b2_aabbMultiplier * displacement;

        if (d.x < 0.0f)
        {
                b.lowerBound.x += d.x;
        }
        else
        {
                b.upperBound.x += d.x;
        }

        if (d.y < 0.0f)
        {
                b.lowerBound.y += d.y;
        }
        else
        {
                b.upperBound.y += d.y;
        }

        m_nodes[proxyId].aabb = b;

        InsertLeaf(proxyId);
        return true;
}

void b2DynamicTree::InsertLeaf(int32 leaf)
{
        ++m_insertionCount;

        if (m_root == b2_nullNode)
        {
                m_root = leaf;
                m_nodes[m_root].parent = b2_nullNode;
                return;
        }

        // Find the best sibling for this node.
        b2Vec2 center = m_nodes[leaf].aabb.GetCenter();
        int32 sibling = m_root;
        if (m_nodes[sibling].IsLeaf() == false)
        {
                do 
                {
                        int32 child1 = m_nodes[sibling].child1;
                        int32 child2 = m_nodes[sibling].child2;

                        b2Vec2 delta1 = b2Abs(m_nodes[child1].aabb.GetCenter() - center);
                        b2Vec2 delta2 = b2Abs(m_nodes[child2].aabb.GetCenter() - center);

                        float32 norm1 = delta1.x + delta1.y;
                        float32 norm2 = delta2.x + delta2.y;

                        if (norm1 < norm2)
                        {
                                sibling = child1;
                        }
                        else
                        {
                                sibling = child2;
                        }

                }
                while(m_nodes[sibling].IsLeaf() == false);
        }

        // Create a parent for the siblings.
        int32 node1 = m_nodes[sibling].parent;
        int32 node2 = AllocateNode();
        m_nodes[node2].parent = node1;
        m_nodes[node2].userData = NULL;
        m_nodes[node2].aabb.Combine(m_nodes[leaf].aabb, m_nodes[sibling].aabb);

        if (node1 != b2_nullNode)
        {
                if (m_nodes[m_nodes[sibling].parent].child1 == sibling)
                {
                        m_nodes[node1].child1 = node2;
                }
                else
                {
                        m_nodes[node1].child2 = node2;
                }

                m_nodes[node2].child1 = sibling;
                m_nodes[node2].child2 = leaf;
                m_nodes[sibling].parent = node2;
                m_nodes[leaf].parent = node2;

                do 
                {
                        if (m_nodes[node1].aabb.Contains(m_nodes[node2].aabb))
                        {
                                break;
                        }

                        m_nodes[node1].aabb.Combine(m_nodes[m_nodes[node1].child1].aabb, m_nodes[m_nodes[node1].child2].aabb);
                        node2 = node1;
                        node1 = m_nodes[node1].parent;
                }
                while(node1 != b2_nullNode);
        }
        else
        {
                m_nodes[node2].child1 = sibling;
                m_nodes[node2].child2 = leaf;
                m_nodes[sibling].parent = node2;
                m_nodes[leaf].parent = node2;
                m_root = node2;
        }
}

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
