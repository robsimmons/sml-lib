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

    (* Port note: ContactManager is flattened into World.

       // Broad-phase callback.
       void AddPair(void* proxyUserDataA, void* proxyUserDataB);
       
       void FindNewContacts();
            
       void Collide();
       *)

    fun contact_destroy (world : world, c : contact) : unit =
        raise BDDWorld "unimplemented (contactmanager.cpp)"

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
             fun one_contactedge ce = contact_destroy (world, 
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

(*
// Advance a dynamic body to its first time of contact
// and adjust the position to ensure clearance.
void b2World::SolveTOI(b2Body* body)
{
        // Find the minimum contact.
        b2Contact* toiContact = NULL;
        float32 toi = 1.0f;
        b2Body* toiOther = NULL;
        bool found;
        int32 count;
        int32 iter = 0;

        bool bullet = body->IsBullet();

        // Iterate until all contacts agree on the minimum TOI. We have
        // to iterate because the TOI algorithm may skip some intermediate
        // collisions when objects rotate through each other.
        do
        {
                count = 0;
                found = false;
                for (b2ContactEdge* ce = body->m_contactList; ce; ce = ce->next)
                {
                        if (ce->contact == toiContact)
                        {
                                continue;
                        }

                        b2Body* other = ce->other;
                        b2BodyType type = other->GetType();

                        // Only bullets perform TOI with dynamic bodies.
                        if (bullet == true)
                        {
                                // Bullets only perform TOI with bodies that have their TOI resolved.
                                if ((other->m_flags & b2Body::e_toiFlag) == 0)
                                {
                                        continue;
                                }

                                // No repeated hits on non-static bodies
                                if (type != b2_staticBody && (ce->contact->m_flags & b2Contact::e_bulletHitFlag) != 0)
                                {
                                                continue;
                                }
                        }
                        else if (type == b2_dynamicBody)
                        {
                                continue;
                        }

                        // Check for a disabled contact.
                        b2Contact* contact = ce->contact;
                        if (contact->IsEnabled() == false)
                        {
                                continue;
                        }

                        // Prevent infinite looping.
                        if (contact->m_toiCount > 10)
                        {
                                continue;
                        }

                        b2Fixture* fixtureA = contact->m_fixtureA;
                        b2Fixture* fixtureB = contact->m_fixtureB;

                        // Cull sensors.
                        if (fixtureA->IsSensor() || fixtureB->IsSensor())
                        {
                                continue;
                        }

                        b2Body* bodyA = fixtureA->m_body;
                        b2Body* bodyB = fixtureB->m_body;

                        // Compute the time of impact in interval [0, minTOI]
                        b2TOIInput input;
                        input.proxyA.Set(fixtureA->GetShape());
                        input.proxyB.Set(fixtureB->GetShape());
                        input.sweepA = bodyA->m_sweep;
                        input.sweepB = bodyB->m_sweep;
                        input.tMax = toi;

                        b2TOIOutput output;
                        b2TimeOfImpact(&output, &input);

                        if (output.state == b2TOIOutput::e_touching && output.t < toi)
                        {
                                toiContact = contact;
                                toi = output.t;
                                toiOther = other;
                                found = true;
                        }

                        ++count;
                }

                ++iter;
        } while (found && count > 1 && iter < 50);

        if (toiContact == NULL)
        {
                body->Advance(1.0f);
                return;
        }

        b2Sweep backup = body->m_sweep;
        body->Advance(toi);
        toiContact->Update(m_contactManager.m_contactListener);
        if (toiContact->IsEnabled() == false)
        {
                // Contact disabled. Backup and recurse.
                body->m_sweep = backup;
                SolveTOI(body);
        }

        ++toiContact->m_toiCount;

        // Update all the valid contacts on this body and build a contact island.
        b2Contact* contacts[b2_maxTOIContacts];
        count = 0;
        for (b2ContactEdge* ce = body->m_contactList; ce && count < b2_maxTOIContacts; ce = ce->next)
        {
                b2Body* other = ce->other;
                b2BodyType type = other->GetType();

                // Only perform correction with static bodies, so the
                // body won't get pushed out of the world.
                if (type == b2_dynamicBody)
                {
                        continue;
                }

                // Check for a disabled contact.
                b2Contact* contact = ce->contact;
                if (contact->IsEnabled() == false)
                {
                        continue;
                }

                b2Fixture* fixtureA = contact->m_fixtureA;
                b2Fixture* fixtureB = contact->m_fixtureB;

                // Cull sensors.
                if (fixtureA->IsSensor() || fixtureB->IsSensor())
                {
                        continue;
                }

                // The contact likely has some new contact points. The listener
                // gives the user a chance to disable the contact.
                if (contact != toiContact)
                {
                        contact->Update(m_contactManager.m_contactListener);
                }

                // Did the user disable the contact?
                if (contact->IsEnabled() == false)
                {
                        // Skip this contact.
                        continue;
                }

                if (contact->IsTouching() == false)
                {
                        continue;
                }

                contacts[count] = contact;
                ++count;
        }

        // Reduce the TOI body's overlap with the contact island.
        b2TOISolver solver(&m_stackAllocator);
        solver.Initialize(contacts, count, body);

        const float32 k_toiBaumgarte = 0.75f;
        bool solved = false;
        for (int32 i = 0; i < 20; ++i)
        {
                bool contactsOkay = solver.Solve(k_toiBaumgarte);
                if (contactsOkay)
                {
                        solved = true;
                        break;
                }
        }

        if (toiOther->GetType() != b2_staticBody)
        {
                        toiContact->m_flags |= b2Contact::e_bulletHitFlag;
        }
}
*)

(*
// Sequentially solve TOIs for each body. We bring each
// body to the time of contact and perform some position correction.
// Time is not conserved.
void b2World::SolveTOI()
{
        // Prepare all contacts.
        for (b2Contact* c = m_contactManager.m_contactList; c; c = c->m_next)
        {
                // Enable the contact
                c->m_flags |= b2Contact::e_enabledFlag;

                // Set the number of TOI events for this contact to zero.
                c->m_toiCount = 0;
        }

        // Initialize the TOI flag.
        for (b2Body* body = m_bodyList; body; body = body->m_next)
        {
                // Kinematic, and static bodies will not be affected by the TOI event.
                // If a body was not in an island then it did not move.
                if ((body->m_flags & b2Body::e_islandFlag) == 0 || body->GetType() == b2_kinematicBody || body->GetType() == b2_staticBody)
                {
                        body->m_flags |= b2Body::e_toiFlag;
                }
                else
                {
                        body->m_flags &= ~b2Body::e_toiFlag;
                }
        }

        // Collide non-bullets.
        for (b2Body* body = m_bodyList; body; body = body->m_next)
        {
                if (body->m_flags & b2Body::e_toiFlag)
                {
                        continue;
                }

                if (body->IsBullet() == true)
                {
                        continue;
                }

                SolveTOI(body);

                body->m_flags |= b2Body::e_toiFlag;
        }

        // Collide bullets.
        for (b2Body* body = m_bodyList; body; body = body->m_next)
        {
                if (body->m_flags & b2Body::e_toiFlag)
                {
                        continue;
                }

                if (body->IsBullet() == false)
                {
                        continue;
                }

                SolveTOI(body);

                body->m_flags |= b2Body::e_toiFlag;
        }
}
*)
    fun step (world : world, dt : real, 
              velocity_iterations : int, position_iterations : int) : unit =
        raise BDDWorld "unimplemented" (* b2world.cpp *)
(*
        // If new fixtures were added, we need to find the new contacts.
        if (m_flags & e_newFixture)
        {
                m_contactManager.FindNewContacts();
                m_flags &= ~e_newFixture;
        }

        m_flags |= e_locked;

        b2TimeStep step;
        step.dt = dt;
        step.velocityIterations = velocityIterations;
        step.positionIterations = positionIterations;
        if (dt > 0.0f)
        {
                step.inv_dt = 1.0f / dt;
        }
        else
        {
                step.inv_dt = 0.0f;
        }

        step.dtRatio = m_inv_dt0 * dt;

        step.warmStarting = m_warmStarting;

        // Update contacts. This is where some contacts are destroyed.
        m_contactManager.Collide();

        // Integrate velocities, solve velocity constraints, and integrate positions.
        if (step.dt > 0.0f)
        {
                Solve(step);
        }

        // Handle TOI events.
        if (m_continuousPhysics && step.dt > 0.0f)
        {
                SolveTOI();
        }

        if (step.dt > 0.0f)
        {
                m_inv_dt0 = step.inv_dt;
        }

        if (m_flags & e_clearForces)
        {
                ClearForces();
        }

        m_flags &= ~e_locked;
*)

  end

end
