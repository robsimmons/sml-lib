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

  (* Port note: Corresponds to DynamicTreeNode in b2dynamictree.h.
     I decided to use idiomatic SML instead of the hand-written
     allocator in Box2D, since it's not clear it'd be faster than
     a good generational gc. *)
  datatype 'a aabb_proxy =
      Node of { (* Fattened aabb *)
                aabb : aabb,
                data : 'a,
                (* Port note: Box2D has a possibility for
                   a 'next' pointer here, but it's just
                   so that the structure can be stored 
                   in freelists for its custom allocator. *)
                parent : 'a aabb_proxy option ref,
                left : 'a aabb_proxy option ref,
                right : 'a aabb_proxy option ref }

  type 'a dynamic_tree =
      { node_count : int ref,
        root : 'a aabb_proxy option ref }

  fun user_data (Node { data, ... }) = data
  fun fat_aabb (Node { aabb, ... }) = aabb

  fun compute_height ({ root, ... } : dynamic_tree) =
      let fun ch NONE = 0
            | ch (SOME (Node { left, right })) =
          1 + Int.max(ch (!left), ch (!right))
      in ch (!root)
      end

  fun dynamic_tree () = { node_count = ref 0, root = ref NONE }

  fun 'a aabb_proxy (tree : dynamic_tree, aabb : aabb, a : 'a) : 'a aabb_proxy =
      let
          val r : vec2 = vec2(aabb_extension, aabb_extension)
      in
          HERE
      end
      
(*

// Create a proxy in the tree as a leaf node. We return the index
// of the node instead of a pointer so that we can grow
// the node pool.
int32 b2DynamicTree::CreateProxy(const b2AABB& aabb, void* userData)
{
        int32 proxyId = AllocateNode();

        // Fatten the aabb.
        b2Vec2 r(b2_aabbExtension, b2_aabbExtension);
        m_nodes[proxyId].aabb.lowerBound = aabb.lowerBound - r;
        m_nodes[proxyId].aabb.upperBound = aabb.upperBound + r;
        m_nodes[proxyId].userData = userData;

        InsertLeaf(proxyId);

        // Rebalance if necessary.
        int32 iterationCount = m_nodeCount >> 4;
        int32 tryCount = 0;
        int32 height = ComputeHeight();
        while (height > 64 && tryCount < 10)
        {
                Rebalance(iterationCount);
                height = ComputeHeight();
                ++tryCount;
        }

        return proxyId;
}

void b2DynamicTree::DestroyProxy(int32 proxyId)
{
        b2Assert(0 <= proxyId && proxyId < m_nodeCapacity);
        b2Assert(m_nodes[proxyId].IsLeaf());

        RemoveLeaf(proxyId);
        FreeNode(proxyId);
}

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

void b2DynamicTree::RemoveLeaf(int32 leaf)
{
        if (leaf == m_root)
        {
                m_root = b2_nullNode;
                return;
        }

        int32 node2 = m_nodes[leaf].parent;
        int32 node1 = m_nodes[node2].parent;
        int32 sibling;
        if (m_nodes[node2].child1 == leaf)
        {
                sibling = m_nodes[node2].child2;
        }
        else
        {
                sibling = m_nodes[node2].child1;
        }

        if (node1 != b2_nullNode)
        {
                // Destroy node2 and connect node1 to sibling.
                if (m_nodes[node1].child1 == node2)
                {
                        m_nodes[node1].child1 = sibling;
                }
                else
                {
                        m_nodes[node1].child2 = sibling;
                }
                m_nodes[sibling].parent = node1;
                FreeNode(node2);

                // Adjust ancestor bounds.
                while (node1 != b2_nullNode)
                {
                        b2AABB oldAABB = m_nodes[node1].aabb;
                        m_nodes[node1].aabb.Combine(m_nodes[m_nodes[node1].child1].aabb, m_nodes[m_nodes[node1].child2].aabb);

                        if (oldAABB.Contains(m_nodes[node1].aabb))
                        {
                                break;
                        }

                        node1 = m_nodes[node1].parent;
                }
        }
        else
        {
                m_root = sibling;
                m_nodes[sibling].parent = b2_nullNode;
                FreeNode(node2);
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
