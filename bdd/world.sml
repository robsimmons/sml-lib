(* Copyright 2010 Tom Murphy VII and Erin Catto. See COPYING for details. *)

(* Main dynamics library. Ties together all the mutually-referential types
   like bodies, fixtures, contacts and joints.
   
   Corresponding to dynamics/b2world.cpp. *)
functor BDDWorld(Arg : BDDWORLD_ARG) :>
  BDDWORLD where type fixture_data = Arg.fixture_data 
             and type body_data = Arg.body_data =
struct
  open Arg
  open BDDSettings
  open BDDTypes
  open BDDMath
  open BDDOps
  infix 6 :+: :-: %-% %+% +++
  infix 7 *: *% +*: +*+ #*% @*:

  exception BDDWorld of string

  structure D = BDDDynamics
  structure Body = BDDBody(Arg)
  structure Fixture = BDDFixture(Arg)
  structure Contact = BDDContact(Arg)
  structure DT = BDDDynamicsTypes(Arg)
  type filter = D.filter
  open DT

  (* XXX implement! *)
  structure Joint = struct
    type joint_type = int
    fun get_next _ = raise Match (* XXX *)
  end


  fun !! (SOME x) = x
    | !! NONE = raise BDDWorld "Expected non-NONE value, like Box2D does"

  (* Port note: ContactManager is only used in World, so its data
     is flattened into that object. *)
  structure ContactManager :>
  sig
      val collide : world -> unit
      val find_new_contacts : world -> unit
      val destroy : world * contact -> unit
  end =
  struct

    fun destroy (world : world, c : contact) : unit =
      let
        val fixture_a = D.C.get_fixture_a c
        val fixture_b = D.C.get_fixture_b c
        val body_a = D.F.get_body fixture_a
        val body_b = D.F.get_body fixture_b

        val () = if D.C.is_touching c
                 then D.W.get_end_contact world c
                 else ()

        (* remove from world DLL. *)
        val prev = D.C.get_prev c
        val next = D.C.get_next c
        val () = case prev of
            NONE => ()
          | SOME prev => D.C.set_next (prev, next)
        val () = case next of
            NONE => ()
          | SOME next => D.C.set_prev (next, prev)

        val () = if SOME c = D.W.get_contact_list world
                 then D.W.set_contact_list (world, next)
                 else ()

        (* Remove from body A *)
        val nodea = D.C.get_node_a c
        val prev = D.E.get_prev nodea
        val next = D.E.get_next nodea
        val () = case prev of
            NONE => ()
          | SOME prev => D.E.set_next (prev, next)
        val () = case next of
            NONE => ()
          | SOME next => D.E.set_prev (next, prev)
        (* Port note: The original code uses pointers to the interior of the
           contact (&c->m_nodeA == ?); I've made these their own cells. I 
           think this is right, but if something is going wrong here, this 
           is a good thing to take a look at. *)
        val () = if SOME nodea = D.B.get_contact_list body_a
                 then D.B.set_contact_list (body_a, next)
                 else ()

        (* Remove from body B *)
        val nodeb = D.C.get_node_b c
        val prev = D.E.get_prev nodeb
        val next = D.E.get_next nodeb
        val () = case prev of
            NONE => ()
          | SOME prev => D.E.set_next (prev, next)
        val () = case next of
            NONE => ()
          | SOME next => D.E.set_prev (next, prev)
        val () = if SOME nodeb = D.B.get_contact_list body_b
                 then D.B.set_contact_list (body_b, next)
                 else ()
      in
          D.W.set_contact_count (world, D.W.get_contact_count world - 1)
      end

    (* Callback used in find_new_contacts. *)
    exception Return
    fun add_pair (world : world) (fixture_a : fixture, fixture_b : fixture) : unit =
      let
        val body_a = D.F.get_body fixture_a
        val body_b = D.F.get_body fixture_b
        (* No self-contact *)
        val () = if body_a = body_b then raise Return
                 else ()

        (* Just raises Return if a contact already exists. *)
        fun one_edge e =
            if SOME body_a = D.E.get_other e
            then
                let val fa = D.C.get_fixture_a (!! (D.E.get_contact e))
                    val fb = D.C.get_fixture_b (!! (D.E.get_contact e))
                in
                    if (fa = fixture_a andalso fb = fixture_b) orelse
                       (fa = fixture_b andalso fb = fixture_a)
                    (* Contact already exists. *)
                    then raise Return
                    else ()
                end
            else ()
        val () = oapp D.E.get_next one_edge (D.B.get_contact_list body_b)

        (* Does a joint override collision? Is at least one body dynamic? *)
        val () = if D.B.should_collide (body_b, body_a)
                 then ()
                 else raise Return

        (* Check user filtering. *)
        val () = if D.W.get_should_collide world (fixture_a, fixture_b)
                 then ()
                 else raise Return

        (* Call the factory. *)
        val c = D.C.new (fixture_a, fixture_b)

        (* Contact creation may swap fixtures.
           XXX (it doesn't, currently.)
           *)
        val fixture_a = D.C.get_fixture_a c
        val fixture_b = D.C.get_fixture_b c
        val body_a = D.F.get_body fixture_a
        val body_b = D.F.get_body fixture_b

        (* Add to world DLL. *)
        val () = D.C.set_next (c, D.W.get_contact_list world)
        val () = case D.W.get_contact_list world of
            NONE => ()
          | SOME prev => D.C.set_prev (prev, SOME c)
        val () = D.W.set_contact_list (world, SOME c)

        (* Connect to island graph. *)
        val node_a = D.C.get_node_a c
        val () = D.E.set_contact (node_a, SOME c)
        val () = D.E.set_other (node_a, SOME body_b)

        val () = D.E.set_next (node_a, D.B.get_contact_list body_a)
        val () = case D.B.get_contact_list body_a of
            NONE => ()
          | SOME prev => D.E.set_prev (prev, SOME node_a)
        val () = D.B.set_contact_list (body_a, SOME node_a)

        val node_b = D.C.get_node_b c
        val () = D.E.set_contact (node_b, SOME c)
        val () = D.E.set_other (node_b, SOME body_a)

        val () = D.E.set_next (node_b, D.B.get_contact_list body_b)
        val () = case D.B.get_contact_list body_b of
            NONE => ()
          | SOME prev => D.E.set_prev (prev, SOME node_b)
        val () = D.B.set_contact_list (body_b, SOME node_b)
      in
        D.W.set_contact_count (world, D.W.get_contact_count world + 1)
      end handle Return => ()

    fun find_new_contacts world =
      BDDBroadPhase.update_pairs (D.W.get_broad_phase world, add_pair world)

  (* This is the top level collision call for the time step. Here
     all the narrow phase collision is processed for the world
     contact list. *)
    fun collide world =
      let
        (* Update awake contacts. *)
        fun loop NONE = ()
          | loop (SOME c) =
          let
            val fixture_a = D.C.get_fixture_a c
            val fixture_b = D.C.get_fixture_b c
            val body_a = D.F.get_body fixture_a
            val body_b = D.F.get_body fixture_b

            (* Port note: Two paths to this in the original. *)
            fun common_case () =
              let val proxy_a = D.F.get_proxy fixture_a
                  val proxy_b = D.F.get_proxy fixture_b
              in
                  if not (BDDBroadPhase.test_overlap (proxy_a, proxy_b))
                  (* Clear contacts that cease to overlap in the broad phase. *)
                  then let val next = D.C.get_next c
                       in destroy (world, c);
                          loop next
                       end
                  (* It persists. *)
                  else 
                      let in
                          D.C.update (c, world);
                          loop (D.C.get_next c)
                      end
              end
          in
            if not (Body.get_awake body_a orelse Body.get_awake body_b)
            then loop (D.C.get_next c)
            else (* Is this contact flagged for filtering? *)
                if D.C.get_flag (c, D.C.FLAG_FILTER)
                then (* Should these bodies collide? 
                        Port note: Both conditionals folded into one. *) 
                    (if not (D.B.should_collide (body_b, body_a)) orelse
                        not (D.W.get_should_collide world (fixture_a, fixture_b))
                     then let val next = D.C.get_next c
                          in destroy (world, c);
                             loop next
                          end
                     else (* Clear the filtering flag. *)
                         let in
                             D.C.clear_flag (c, D.C.FLAG_FILTER);
                             common_case ()
                         end)
                else common_case ()
          end
      in
        loop (D.W.get_contact_list world)
      end

  end (* ContactManager *)


  structure World =
  struct

    open D.W
    type contact_impulse = D.contact_impulse
    datatype raycast_action = datatype D.raycast_action

    (* Should probably expose this *)
    fun default_collision_filter (fixture_a : fixture, fixture_b : fixture) : bool =
      let
          val filter_a = Fixture.get_filter fixture_a
          val filter_b = Fixture.get_filter fixture_b
      in
          if Fixture.filter_group_index filter_a =
             Fixture.filter_group_index filter_b andalso
             Fixture.filter_group_index filter_a <> 0
          then Fixture.filter_group_index filter_a > 0
          else Word16.andb (Fixture.filter_mask_bits filter_a, 
                            Fixture.filter_category_bits filter_b) <> 0w0 andalso
               Word16.andb (Fixture.filter_mask_bits filter_b, 
                            Fixture.filter_category_bits filter_a) <> 0w0
      end

    fun world (gravity, do_sleep) : world =
        ref (D.W { flags = FLAG_CLEAR_FORCES,
                   body_list = NONE,
                   joint_list = NONE,
                   body_count = 0,
                   joint_count = 0,
                   gravity = gravity,
                   allow_sleep = do_sleep,
                    
                   ground_body = NONE,
                   goodbye_joint_hook = ignore,
                   goodbye_fixture_hook = ignore,
                    
                   inv_dt0 = 0.0,
                   
                   warm_starting = true,
                   continuous_physics = true,
                   
                   broad_phase = BDDBroadPhase.broadphase (),
                   contact_list = NONE,
                   contact_count = 0,
                   should_collide = default_collision_filter,
                   begin_contact = ignore,
                   end_contact = ignore,
                   pre_solve = ignore,
                   post_solve = ignore })


    fun get_proxy_count world =
        BDDBroadPhase.proxy_count (get_broad_phase world)

    val set_should_collide_filter = set_should_collide


    fun create_body (world : world,
                     def as { typ : Body.body_type,
                              position : BDDMath.vec2,
                              angle : real,
                              linear_velocity : BDDMath.vec2,
                              angular_velocity : real,
                              linear_damping : real,
                              angular_damping : real,
                              allow_sleep : bool,
                              awake : bool,
                              fixed_rotation : bool,
                              bullet : bool,
                              active : bool,
                              data : body_data,
                              inertia_scale : real }) : body =
      if is_locked world
      then raise BDDWorld "Can't call create_body from callbacks."
      else
      let
          val body = D.B.new (def, world, get_body_list world)
          (* Add to doubly linked list as the new head. *)
          val () = case get_body_list world of
              NONE => ()
            | SOME b => D.B.set_prev (b, SOME body)
          val () = set_body_list (world, SOME body)
          val () = set_body_count (world, get_body_count world + 1)
      in
          body
      end
            
    (* Joints are unimplemented for now. *)
    fun create_joint (world : world,
                      { typ : Joint.joint_type,
                        user_data : joint_data,
                        body_a : body,
                        body_b : body,
                        collide_connected : bool }) : joint =
        raise BDDWorld "unimplemented"
(*
b2Joint* b2World::CreateJoint(const b2JointDef* def)
{
        b2Assert(IsLocked() == false);
        if (IsLocked())
        {
                return NULL;
        }

        b2Joint* j = b2Joint::Create(def, &m_blockAllocator);

        // Connect to the world list.
        j->m_prev = NULL;
        j->m_next = m_jointList;
        if (m_jointList)
        {
                m_jointList->m_prev = j;
        }
        m_jointList = j;
        ++m_jointCount;

        // Connect to the bodies' doubly linked lists.
        j->m_edgeA.joint = j;
        j->m_edgeA.other = j->m_bodyB;
        j->m_edgeA.prev = NULL;
        j->m_edgeA.next = j->m_bodyA->m_jointList;
        if (j->m_bodyA->m_jointList) j->m_bodyA->m_jointList->prev = &j->m_edgeA;
        j->m_bodyA->m_jointList = &j->m_edgeA;

        j->m_edgeB.joint = j;
        j->m_edgeB.other = j->m_bodyA;
        j->m_edgeB.prev = NULL;
        j->m_edgeB.next = j->m_bodyB->m_jointList;
        if (j->m_bodyB->m_jointList) j->m_bodyB->m_jointList->prev = &j->m_edgeB;
        j->m_bodyB->m_jointList = &j->m_edgeB;

        b2Body* bodyA = def->bodyA;
        b2Body* bodyB = def->bodyB;

        // If the joint prevents collisions, then flag any contacts for filtering.
        if (def->collideConnected == false)
        {
                b2ContactEdge* edge = bodyB->GetContactList();
                while (edge)
                {
                        if (edge->other == bodyA)
                        {
                                // Flag the contact for filtering at the next time step (where either
                                // body is awake).
                                edge->contact->FlagForFiltering();
                        }

                        edge = edge->next;
                }
        }

        // Note: creating a joint doesn't wake the bodies.

        return j;
}
*)

    fun destroy_joint (joint : joint) : unit =
        raise BDDWorld "unimplemented"
(*
void b2World::DestroyJoint(b2Joint* j)
{
        b2Assert(IsLocked() == false);
        if (IsLocked())
        {
                return;
        }

        bool collideConnected = j->m_collideConnected;

        // Remove from the doubly linked list.
        if (j->m_prev)
        {
                j->m_prev->m_next = j->m_next;
        }

        if (j->m_next)
        {
                j->m_next->m_prev = j->m_prev;
        }

        if (j == m_jointList)
        {
                m_jointList = j->m_next;
        }

        // Disconnect from island graph.
        b2Body* bodyA = j->m_bodyA;
        b2Body* bodyB = j->m_bodyB;

        // Wake up connected bodies.
        bodyA->SetAwake(true);
        bodyB->SetAwake(true);

        // Remove from body 1.
        if (j->m_edgeA.prev)
        {
                j->m_edgeA.prev->next = j->m_edgeA.next;
        }

        if (j->m_edgeA.next)
        {
                j->m_edgeA.next->prev = j->m_edgeA.prev;
        }

        if (&j->m_edgeA == bodyA->m_jointList)
        {
                bodyA->m_jointList = j->m_edgeA.next;
        }

        j->m_edgeA.prev = NULL;
        j->m_edgeA.next = NULL;

        // Remove from body 2
        if (j->m_edgeB.prev)
        {
                j->m_edgeB.prev->next = j->m_edgeB.next;
        }

        if (j->m_edgeB.next)
        {
                j->m_edgeB.next->prev = j->m_edgeB.prev;
        }

        if (&j->m_edgeB == bodyB->m_jointList)
        {
                bodyB->m_jointList = j->m_edgeB.next;
        }

        j->m_edgeB.prev = NULL;
        j->m_edgeB.next = NULL;

        b2Joint::Destroy(j, &m_blockAllocator);

        b2Assert(m_jointCount > 0);
        --m_jointCount;

        // If the joint prevents collisions, then flag any contacts for filtering.
        if (collideConnected == false)
        {
                b2ContactEdge* edge = bodyB->GetContactList();
                while (edge)
                {
                        if (edge->other == bodyA)
                        {
                                // Flag the contact for filtering at the next time step (where either
                                // body is awake).
                                edge->contact->FlagForFiltering();
                        }

                        edge = edge->next;
                }
        }
}
*)

    fun destroy_body (body : body) : unit =
      let val world = D.B.get_world body
      in if is_locked world
         then raise BDDWorld "Can't call destroy_body from callbacks."
         else
         let
             (* Delete the attached joints. *)
             fun one_joint j =
                 (get_goodbye_joint_hook world j;
                  destroy_joint j)
             val () = oapp Joint.get_next one_joint (D.B.get_joint_list body)
             val () = D.B.set_joint_list (body, NONE)

             (* Delete the attached contacts. *)
             fun one_contactedge ce = 
                 ContactManager.destroy (world, 
                                         case D.E.get_contact ce of
                                             NONE => raise BDDWorld "contact edge had no contact?"
                                           | SOME c => c)
             val () = oapp D.E.get_next one_contactedge (D.B.get_contact_list body)
             val () = D.B.set_contact_list (body, NONE)

             (* Delete the attached fixtures. This destroys broad-phase proxies. *)
             fun one_fixture f =
                 (get_goodbye_fixture_hook world f;
                  D.F.destroy_proxy (f, get_broad_phase world);
                  Body.destroy_fixture (body, f))
             val () = oapp D.F.get_next one_fixture (D.B.get_fixture_list body)
             val () = D.B.set_fixture_list (body, NONE)
             val () = D.B.set_fixture_count (body, 0)

             (* Remove from world body list *)
             val prev = D.B.get_prev body
             val next = D.B.get_next body
             val () = case prev of
                 NONE => ()
               | SOME prev => D.B.set_next (prev, next)
             val () = case next of
                 NONE => ()
               | SOME next => D.B.set_prev (next, prev)

             val () = if get_body_list world = SOME body
                      then set_body_list (world, next)
                      else ()

             val () = set_body_count (world, get_body_count world - 1)
         in
             ()
         end
    end

    fun clear_forces (world : world) : unit =
        oapp Body.get_next (fn b =>
                            let in
                                D.B.set_force (b, vec2 (0.0, 0.0));
                                D.B.set_torque (b, 0.0)
                            end) (get_body_list world)

    fun query_aabb (world : world, callback : fixture -> bool, aabb : BDDTypes.aabb) : unit =
        BDDBroadPhase.query (get_broad_phase world,
                             (fn proxy =>
                              let val fixture = BDDBroadPhase.user_data proxy
                              in callback fixture
                              end), 
                             aabb)

    fun ray_cast (world : world,
                  callback : { fixture : fixture, point : BDDMath.vec2,
                               normal : BDDMath.vec2, fraction : real } -> raycast_action,
                  point1 : BDDMath.vec2,
                  point2 : BDDMath.vec2) : unit =
        let val bp = get_broad_phase world
            fun cb (input as { p1 : BDDMath.vec2, p2 : BDDMath.vec2, max_fraction : real }, proxy) =
                let val fixture = BDDBroadPhase.user_data proxy
                    val hit = Fixture.ray_cast (fixture, input)
                in
                    case hit of
                        NONE => max_fraction
                      | SOME { normal, fraction } =>
                            let val point : vec2 = (1.0 - fraction) *: p1 :+: fraction *: p2
                            (* TODO: Might want to propagate this datatype deeper; it's better. *)
                            in case callback { fixture = fixture, point = point, 
                                               normal = normal, fraction = fraction } of
                                IgnoreAndContinue => ~1.0
                              | Terminate => 0.0
                              | Clip r => r
                              | Don'tClip => 1.0
                            end
                end
        in
            BDDBroadPhase.ray_cast (bp, cb, { max_fraction = 1.0, p1 = point1, p2 = point2 })
        end

    fun solve (world : world, step : D.time_step) =
        raise BDDWorld "unimplemented"
(*
// Find islands, integrate and solve constraints, solve position constraints
void b2World::Solve(const b2TimeStep& step)
{
        // Size the island for the worst case.
        b2Island island(m_bodyCount,
                                        m_contactManager.m_contactCount,
                                        m_jointCount,
                                        &m_stackAllocator,
                                        m_contactManager.m_contactListener);

        // Clear all the island flags.
        for (b2Body* b = m_bodyList; b; b = b->m_next)
        {
                b->m_flags &= ~b2Body::e_islandFlag;
        }
        for (b2Contact* c = m_contactManager.m_contactList; c; c = c->m_next)
        {
                c->m_flags &= ~b2Contact::e_islandFlag;
        }
        for (b2Joint* j = m_jointList; j; j = j->m_next)
        {
                j->m_islandFlag = false;
        }

        // Build and simulate all awake islands.
        int32 stackSize = m_bodyCount;
        b2Body** stack = (b2Body** )m_stackAllocator.Allocate(stackSize * sizeof(b2Body* ));
        for (b2Body* seed = m_bodyList; seed; seed = seed->m_next)
        {
                if (seed->m_flags & b2Body::e_islandFlag)
                {
                        continue;
                }

                if (seed->IsAwake() == false || seed->IsActive() == false)
                {
                        continue;
                }

                // The seed can be dynamic or kinematic.
                if (seed->GetType() == b2_staticBody)
                {
                        continue;
                }

                // Reset island and stack.
                island.Clear();
                int32 stackCount = 0;
                stack[stackCount++] = seed;
                seed->m_flags |= b2Body::e_islandFlag;

                // Perform a depth first search (DFS) on the constraint graph.
                while (stackCount > 0)
                {
                        // Grab the next body off the stack and add it to the island.
                        b2Body* b = stack[--stackCount];
                        b2Assert(b->IsActive() == true);
                        island.Add(b);

                        // Make sure the body is awake.
                        b->SetAwake(true);

                        // To keep islands as small as possible, we don't
                        // propagate islands across static bodies.
                        if (b->GetType() == b2_staticBody)
                        {
                                continue;
                        }

                        // Search all contacts connected to this body.
                        for (b2ContactEdge* ce = b->m_contactList; ce; ce = ce->next)
                        {
                                b2Contact* contact = ce->contact;

                                // Has this contact already been added to an island?
                                if (contact->m_flags & b2Contact::e_islandFlag)
                                {
                                        continue;
                                }

                                // Is this contact solid and touching?
                                if (contact->IsEnabled() == false ||
                                        contact->IsTouching() == false)
                                {
                                        continue;
                                }

                                // Skip sensors.
                                bool sensorA = contact->m_fixtureA->m_isSensor;
                                bool sensorB = contact->m_fixtureB->m_isSensor;
                                if (sensorA || sensorB)
                                {
                                        continue;
                                }

                                island.Add(contact);
                                contact->m_flags |= b2Contact::e_islandFlag;

                                b2Body* other = ce->other;

                                // Was the other body already added to this island?
                                if (other->m_flags & b2Body::e_islandFlag)
                                {
                                        continue;
                                }

                                b2Assert(stackCount < stackSize);
                                stack[stackCount++] = other;
                                other->m_flags |= b2Body::e_islandFlag;
                        }

                        // Search all joints connect to this body.
                        for (b2JointEdge* je = b->m_jointList; je; je = je->next)
                        {
                                if (je->joint->m_islandFlag == true)
                                {
                                        continue;
                                }

                                b2Body* other = je->other;

                                // Don't simulate joints connected to inactive bodies.
                                if (other->IsActive() == false)
                                {
                                        continue;
                                }

                                island.Add(je->joint);
                                je->joint->m_islandFlag = true;

                                if (other->m_flags & b2Body::e_islandFlag)
                                {
                                        continue;
                                }

                                b2Assert(stackCount < stackSize);
                                stack[stackCount++] = other;
                                other->m_flags |= b2Body::e_islandFlag;
                        }
                }

                island.Solve(step, m_gravity, m_allowSleep);

                // Post solve cleanup.
                for (int32 i = 0; i < island.m_bodyCount; ++i)
                {
                        // Allow static bodies to participate in other islands.
                        b2Body* b = island.m_bodies[i];
                        if (b->GetType() == b2_staticBody)
                        {
                                b->m_flags &= ~b2Body::e_islandFlag;
                        }
                }
        }

        m_stackAllocator.Free(stack);

        // Synchronize fixtures, check for out of range bodies.
        for (b2Body* b = m_bodyList; b; b = b->GetNext())
        {
                // If a body was not in an island then it did not move.
                if ((b->m_flags & b2Body::e_islandFlag) == 0)
                {
                        continue;
                }

                if (b->GetType() == b2_staticBody)
                {
                        continue;
                }

                // Update fixtures (for broad-phase).
                b->SynchronizeFixtures();
        }

        // Look for new contacts.
        m_contactManager.FindNewContacts();
}
*)
    (* Advance a dynamic body to its first time of contact
       and adjust the position to ensure clearance. *)
    fun solve_toi_body (world : world, body : body) : unit =
      let
        (* Find the minimum contact. *)
        val toi_contact : contact option ref = ref NONE
        val toi : real ref = ref 1.0
        val toi_other : body option ref = ref NONE
        val found : bool ref = ref false
        val count : int ref = ref 0

        val bullet = Body.get_bullet body

        (* Iterate until all contacts agree on the minimum TOI. We have
          to iterate because the TOI algorithm may skip some intermediate
          collisions when objects rotate through each other. *)
        fun loop iter =
          let
              fun one_edge ce =
                  (* n.b. weird behavior if contact 
                     was not initialized *)
                if D.E.get_contact ce = !toi_contact  
                then ()
                else
                  let val other = !! (D.E.get_other ce)
                      val typ = D.B.get_typ other
                      (* Port note: Technically the null check only happens if we get
                         into the bullet case below *)
                      val contact = !! (D.E.get_contact ce) 
                  in
                      (* Only bullets perform TOI with dynamic bodies. *)
                      if (if bullet
                          then (* Bullets only perform TOI with bodies that have their TOI resolved. *)
                               not (D.B.get_flag (other, D.B.FLAG_TOI)) orelse
                               (* No repeated hits on non-static bodies *)
                               (typ <> D.Static andalso D.C.get_flag (contact, D.C.FLAG_BULLET_HIT))
                          else typ = D.Dynamic)
                      then ()
                      else (* check for a disabled contact *)
                      if not (D.C.get_flag (contact, D.C.FLAG_ENABLED))
                      then ()
                      (* prevent infinite looping *)
                      else 
                      if D.C.get_toi_count contact > 10
                      then ()
                      else
                      let val fixture_a = D.C.get_fixture_a contact
                          val fixture_b = D.C.get_fixture_b contact
                      in
                          (* Cull sensors. *)
                          if Fixture.is_sensor fixture_a orelse Fixture.is_sensor fixture_b
                          then ()
                          else let val body_a = D.F.get_body fixture_a
                                   val body_b = D.F.get_body fixture_b

                                   (* Compute the time of impact in interval [0, minTOI] *)
                                   val toi_input = 
                                       { proxya = BDDDistance.shape_proxy (D.F.get_shape fixture_a),
                                         proxyb = BDDDistance.shape_proxy (D.F.get_shape fixture_b),
                                         sweepa = D.B.get_sweep body_a,
                                         sweepb = D.B.get_sweep body_b,
                                         tmax = !toi }
                               in
                                   case BDDTimeOfImpact.time_of_impact toi_input of
                                     (BDDTimeOfImpact.STouching, t) =>
                                         if t < !toi
                                         then let in
                                                toi_contact := SOME contact;
                                                toi := t;
                                                toi_other := SOME other;
                                                found := true
                                              end
                                         else ()
                                   | _ => ();

                                   count := !count + 1
                               end
                      end
                  end
           in
               oapp D.E.get_next one_edge (D.B.get_contact_list body);
               if !found andalso !count > 1 andalso iter < 49
               then loop (iter + 1)
               else ()
           end
        val () = loop 0
      in
        case (!toi_contact, !toi_other) of
            (NONE, NONE) => D.B.advance (body, 1.0)
          | (SOME _, NONE) => raise BDDWorld "impossible"
          | (NONE, SOME _) => raise BDDWorld "impossible"
          | (SOME toi_contact, SOME toi_other) => 
        let
          val backup : sweep = sweepcopy (D.B.get_sweep body)
          val () = D.B.advance (body, !toi)
          val () = D.C.update (toi_contact, world)
          val () = if not (D.C.get_flag (toi_contact, D.C.FLAG_ENABLED))
                   (* Contact disabled. Backup and recurse. *)
                   then let in
                           D.B.set_sweep (body, backup);
                           solve_toi_body (world, body)
                        end
                   else ()
          val () = D.C.set_toi_count (toi_contact, D.C.get_toi_count toi_contact + 1)
          (* Update all the valid contacts on this body and build a contact island. 
             Port note: This was a fixed array in Box2D. *)
          val contacts = ref nil
          val ncontacts = ref 0
          fun one_edge ce =
            if !ncontacts >= BDDSettings.max_toi_contacts
            then ()
            else let val other = !! (D.E.get_other ce)
                     val typ = D.B.get_typ other
                     val contact = !! (D.E.get_contact ce)
                     val fixture_a = D.C.get_fixture_a contact
                     val fixture_b = D.C.get_fixture_b contact
                 in
                     (* Only perform correction with static bodies, so the
                        body won't get pushed out of the world. *)
                     if typ = D.Dynamic
                     then ()
                     else
                     (* Check for a disabled contact. *)
                     if not (D.C.get_flag (contact, D.C.FLAG_ENABLED))
                     then ()
                     else
                     (* Cull sensors. *)
                     if Fixture.is_sensor fixture_a orelse 
                        Fixture.is_sensor fixture_b
                     then ()
                     else
                     let in
                         (* The contact likely has some new contact points. The listener
                            gives the client a chance to disable the contact. *)
                         if contact <> toi_contact
                         then D.C.update (contact, world)
                         else ();
                         
                         (* Did the user disable the contact? *)
                         if D.C.get_flag (contact, D.C.FLAG_ENABLED)
                         then ()
                         else
                         if not (Contact.is_touching contact)
                         then ()
                         else 
                             let in
                                 contacts := contact :: !contacts;
                                 count := !count + 1
                             end
                     end
                 end
          val () = oapp D.E.get_next one_edge (D.B.get_contact_list body)

          (* Reduce the TOI body's overlap with the contact island. *)
          val TOI_BAUMGARTE = 0.75
          val solver = BDDTOISolver.solver (!contacts, body)
          fun loop 20 = ()
            | loop iter =
              if BDDTOISolver.solve (solver, TOI_BAUMGARTE)
              then ()
              else loop (iter + 1)
        in
          if D.B.get_typ toi_other <> D.Static
          then D.C.set_flag (toi_contact, D.C.FLAG_BULLET_HIT)
          else ()
        end
      end

    (* Sequentially solve TOIs for each body. We bring each
       body to the time of contact and perform some position correction.
       Time is not conserved. *)
    fun solve_toi (world : world) : unit =
      let
        (* Prepare all contacts. *)
          fun onecontact c =
            let in
                (* Enable the contact *)
                D.C.set_flag (c, D.C.FLAG_ENABLED);
                (* Set the number of TOI events for this contact to zero. *)
                D.C.set_toi_count (c, 0)
            end
          val () = oapp D.C.get_next onecontact (get_contact_list world)

          (* Initialize the TOI flag. *)
          fun onebody_toi b =
            (* Kinematic and static bodies will not be affected by the TOI event.
               If a body was not in an island then it did not move. *)
            if not (D.B.get_flag (b, D.B.FLAG_ISLAND)) orelse
               (case D.B.get_typ b of
                    D.Kinematic => true
                  | D.Static => true
                  | D.Dynamic => false) 
            then D.B.set_flag (b, D.B.FLAG_TOI)
            else D.B.clear_flag (b, D.B.FLAG_TOI)
          val () = oapp D.B.get_next onebody_toi (get_body_list world)

          (* Collide non-bullets. *)
          fun onebody_nonbullet b =
            if D.B.get_flag (b, D.B.FLAG_TOI)
            then ()
            else if Body.get_bullet b
                 then ()
                 else (solve_toi_body (world, b);
                       D.B.set_flag (b, D.B.FLAG_TOI))
          val () = oapp D.B.get_next onebody_nonbullet (get_body_list world)

          (* Collide bullets. *)
          fun onebody_bullet b =
            if D.B.get_flag (b, D.B.FLAG_TOI)
            then ()
            else if not (Body.get_bullet b)
                 then ()
                 else (solve_toi_body (world, b);
                       D.B.set_flag (b, D.B.FLAG_TOI))
          val () = oapp D.B.get_next onebody_bullet (get_body_list world)
      in
          ()
      end

    fun step (world : world, dt : real, 
              velocity_iterations : int, position_iterations : int) : unit =
      let
          (* If new fixtures were added, we need to find the new contacts. *)
          val () = if get_flag (world, FLAG_NEW_FIXTURE)
                   then (ContactManager.find_new_contacts world;
                         clear_flag (world, FLAG_NEW_FIXTURE))
                   else ()
          val () = set_flag (world, FLAG_LOCKED)

          val inv_dt = if dt > 0.0
                       then 1.0 / dt
                       else 0.0
          val step = { dt = dt,
                       velocity_iterations = velocity_iterations,
                       position_iterations = position_iterations,
                       inv_dt = inv_dt,
                       dt_ratio = get_inv_dt0 world * dt,
                       warm_starting = get_warm_starting world }
          (* Update contacts. This is where some contacts are destroyed. *)
          val () = ContactManager.collide world

          (* Integrate velocities, solve velocity constraints, and integrate positions. *)
          val () = if dt > 0.0
                   then solve (world, step)
                   else ()

          (* Handle TOI events. *)
          val () = if get_continuous_physics world andalso dt > 0.0
                   then solve_toi world
                   else ()

          val () = if dt > 0.0
                   then set_inv_dt0 (world, inv_dt)
                   else ()

          val () = if get_flag (world, FLAG_CLEAR_FORCES)
                   then clear_forces world
                   else ()
      in
        clear_flag (world, FLAG_LOCKED)
      end

  end (* World *)

end
