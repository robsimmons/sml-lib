(* Copyright 2010 Tom Murphy VII and Erin Catto. See COPYING for details. *)

(* Implementation of rigid bodies.

   Corresponding to parts of dynamics/b2body.cpp. *)
functor BDDBody(Arg : 
                sig
                  type fixture_data
                  type body_data
                  type joint_data
                end) : BDDBODY =
struct
  open Arg
  open BDDSettings
  open BDDTypes
  open BDDMath
  open BDDOps
  infix 6 :+: :-: %-% %+% +++
  infix 7 *: *% +*: +*+ #*% @*:

  exception BDDBody of string
  
  fun !! (SOME r) = r
    | !! NONE = raise BDDBody
      ("Expected non-NONE reference; corresponds to an unchecked NULL " ^
       "dereference in Box2D. This is probably because an element " ^
       "(e.g. fixture, joint) was used after being detached, " ^
       "or before being initialized.")

  structure D = BDDDynamics
  datatype bodycell = datatype D.bodycell
  datatype body_type = datatype D.body_type
  structure DT = BDDDynamicsTypes(Arg)
  open DT
  type filter = D.filter

  open D.B
  val get_type = get_typ
  val set_type = set_typ

  fun set_awake (b, f) = 
      let in
          set_sleep_time (b, 0.0);
          if f 
          then set_flag (b, FLAG_AWAKE)
          else 
              let in
                  clear_flag (b, FLAG_AWAKE);
                  set_linear_velocity (b, vec2 (0.0, 0.0));
                  set_force (b, vec2 (0.0, 0.0));
                  set_torque (b, 0.0)
              end
      end

  fun get_awake b = get_flag (b, FLAG_AWAKE)

  fun set_linear_velocity (b : body, v : BDDMath.vec2) : unit =
      case get_type b of
          Static => ()
        | _ => let in
                   if dot2 (v, v) > 0.0
                   then set_awake (b, true)
                   else ();
                   set_linear_velocity (b, v)
               end

  fun set_angular_velocity (b : body, w : real) : unit =
      case get_type b of
          Static => ()
        | _ => let in
                   if w * w > 0.0
                   then set_awake (b, true)
                   else ();
                   set_angular_velocity (b, w)
               end

  val get_transform = get_xf
  fun get_position b = transformposition (get_transform b)

  fun get_angle b = sweepa (get_sweep b)
  fun get_world_center b = sweepc (get_sweep b)
  fun get_local_center b = sweeplocalcenter (get_sweep b)

  fun get_inertia (ref (B { i, mass, sweep, ... })) =
      let val lc = sweeplocalcenter sweep
      in i * mass * dot2(lc, lc)
      end

  fun get_mass_data (ref (B { i, mass, sweep, ... })) =
      let val lc = sweeplocalcenter sweep
      in { mass = mass,
           i = i * mass * dot2(lc, lc),
           center = lc }
      end

  fun set_bullet (b, f) = if f then set_flag (b, FLAG_BULLET)
                          else clear_flag (b, FLAG_BULLET)
  fun get_bullet b = get_flag (b, FLAG_BULLET)

  fun get_active b = get_flag (b, FLAG_ACTIVE)
  fun set_active (b, flag) =
      if flag = get_flag (b, FLAG_ACTIVE)
      then ()
      else 
          if flag
          then
              let 
                  val bp = D.W.get_broad_phase (get_world b)
                  val xf = D.B.get_xf b
              in
                  set_flag (b, FLAG_ACTIVE);
                  (* Create all proxies *)
                  oapp D.F.get_next 
                  (fn fixture =>
                   D.F.create_proxy(fixture, bp, xf)) 
                  (D.B.get_fixture_list b)
                  (* Contacts are created the next time step. *)
              end
          else
              let 
                  val w = get_world b
                  val bp = D.W.get_broad_phase w
              in
                  clear_flag (b, FLAG_ACTIVE);
                  (* Destroy all proxies. *)
                  oapp D.F.get_next
                  (fn fixture =>
                   D.F.destroy_proxy (fixture, bp))
                  (D.B.get_fixture_list b);
                  (* Destroy the attached contacts. *)
                  oapp D.E.get_next
                  (fn ce0 => D.W.CM.destroy (w, !!(D.E.get_contact ce0)))
                  (D.B.get_contact_list b);
                  (* Clear them. *)
                  D.B.set_contact_list (b, NONE)
              end


  fun reset_mass_data b =
      let
          (* Compute mass data from shapes. Each shape has its own density. *)
          val () = D.B.set_mass (b, 0.0)
          val () = D.B.set_inv_mass (b, 0.0)
          val () = D.B.set_i (b, 0.0)
          val () = D.B.set_inv_i (b, 0.0)
          val () = sweep_set_localcenter (D.B.get_sweep b, vec2 (0.0, 0.0))
      in
          (* Static and kinematic bodies have zero mass. *)
          if D.B.get_typ b = D.Static orelse
             D.B.get_typ b = D.Kinematic
          then
             let in
                 sweep_set_c0 (D.B.get_sweep b, 
                               transformposition (D.B.get_xf b));
                 sweep_set_c (D.B.get_sweep b, 
                              transformposition (D.B.get_xf b))
             end
          else
             let
                 val () = if D.B.get_typ b = D.Dynamic
                          then ()
                          else raise BDDBody "assertion failed"

                 (* Accumulate mass over all fixtures. *)
                 val center = ref (vec2 (0.0, 0.0))
                 val () =
                     oapp D.F.get_next
                     (fn f =>
                      if Real.== (D.F.get_density f, 0.0)
                      then ()
                      else
                          let val mass_data = D.F.get_mass_data f
                          in
                              D.B.set_mass(b, D.B.get_mass b +
                                           #mass mass_data);
                              center := !center :+:
                              (#mass mass_data *: #center mass_data);
                              D.B.set_i(b, D.B.get_i b + #i mass_data)
                          end)
                     (D.B.get_fixture_list b)
                 (* Compute center of mass. *)
                 val () =
                 if D.B.get_mass b > 0.0
                 then
                     let in
                         D.B.set_inv_mass (b, 1.0 / D.B.get_mass b);
                         center := D.B.get_inv_mass b *: !center
                     end
                 else
                     (* Force all dynamic bodies to have positive mass. *)
                     let in
                         D.B.set_mass (b, 1.0);
                         D.B.set_inv_mass (b, 1.0)
                     end

                 val () =
                 if D.B.get_i b > 0.0 andalso
                    not (D.B.get_flag(b, FLAG_FIXED_ROTATION))
                 then
                     (* Center the intertia about the center of mass. *)
                     let in
                         D.B.set_i (b, D.B.get_i b -
                                    D.B.get_mass b * dot2(!center, !center));
                         (* PERF *)
                         (if D.B.get_i b > 0.0
                          then ()
                          else raise BDDBody "assertion");
                         D.B.set_inv_i (b, 1.0 / D.B.get_i b)
                     end
                 else
                     let in
                         D.B.set_i (b, 0.0);
                         D.B.set_inv_i (b, 0.0)
                     end

                 (* Move center of mass *)
                 val old_center : vec2 = sweepc (D.B.get_sweep b)
                 val c = D.B.get_xf b @*: (!center)
             in
                 sweep_set_localcenter (D.B.get_sweep b, !center);
                 sweep_set_c0 (D.B.get_sweep b, c);
                 sweep_set_c (D.B.get_sweep b, c);
                 (* Update center of mass velocity. *)
                 D.B.set_linear_velocity 
                 (b,
                  D.B.get_linear_velocity b :+: 
                  cross2sv(D.B.get_angular_velocity b,
                           c :-: old_center))
             end
      end

  fun set_fixed_rotation (b, f) =
      let in
          if f then set_flag (b, FLAG_FIXED_ROTATION)
          else clear_flag (b, FLAG_FIXED_ROTATION);
          reset_mass_data b
      end

  fun get_fixed_rotation b = get_flag (b, FLAG_FIXED_ROTATION)

  fun set_sleeping_allowed (b, f) =
      if f then set_flag (b, FLAG_AUTO_SLEEP)
      else (clear_flag (b, FLAG_AUTO_SLEEP);
            set_awake (b, true))

  fun get_sleeping_allowed b = get_flag (b, FLAG_AUTO_SLEEP)

  fun apply_force (b, force : vec2, point : vec2) : unit =
      case get_type b of
          Dynamic => 
              (if get_awake b
               then ()
               else set_awake (b, true);
               set_force (b, get_force b :+: force);
               set_torque (b, cross2vv(point :-: sweepc (get_sweep b), force)))
        | _ => ()

  fun apply_torque (b, torque : real) : unit =
      case get_type b of
          Dynamic => 
              (if get_awake b
               then ()
               else set_awake (b, true);
               set_torque (b, get_torque b + torque))
        | _ => ()

  fun apply_linear_impulse (b, impulse : vec2, point : vec2) : unit =
      case get_type b of
          Dynamic => 
              (if get_awake b
               then ()
               else set_awake (b, true);
               set_linear_velocity (b, get_linear_velocity b :+:
                                    get_inv_mass b *: impulse);
               set_angular_velocity (b, get_angular_velocity b +
                                     get_inv_i b *
                                     cross2vv(point :-: sweepc (get_sweep b),
                                              impulse)))
        | _ => ()

  fun apply_angular_impulse (b, impulse : real) : unit =
      case get_type b of
          Dynamic => 
              (if get_awake b
               then ()
               else set_awake (b, true);
               set_angular_velocity (b, get_angular_velocity b +
                                     get_inv_i b * impulse))
        | _ => ()


  val get_joints = get_joint_list
  val get_fixtures = get_fixture_list

  fun create_fixture (body : body,
                      { shape : BDDShape.shape,
                        data : body_data,
                        friction : real,
                        restitution : real,
                        density : real,
                        is_sensor : bool,
                        filter : D.filter }) : fixture =
      raise BDDBody "unimplemented"
(*
b2Fixture* b2Body::CreateFixture(const b2FixtureDef* def)
{
        b2Assert(m_world->IsLocked() == false);
        if (m_world->IsLocked() == true)
        {
                return NULL;
        }

        b2BlockAllocator* allocator = &m_world->m_blockAllocator;

        void* memory = allocator->Allocate(sizeof(b2Fixture));
        b2Fixture* fixture = new (memory) b2Fixture;
        fixture->Create(allocator, this, def);

        if (m_flags & e_activeFlag)
        {
                b2BroadPhase* broadPhase = &m_world->m_contactManager.m_broadPhase;
                fixture->CreateProxy(broadPhase, m_xf);
        }

        fixture->m_next = m_fixtureList;
        m_fixtureList = fixture;
        ++m_fixtureCount;

        fixture->m_body = this;

        // Adjust mass properties if needed.
        if (fixture->m_density > 0.0f)
        {
                ResetMassData();
        }

        // Let the world know we have a new fixture. This will cause new contacts
        // to be created at the beginning of the next time step.
        m_world->m_flags |= b2World::e_newFixture;

        return fixture;
}
*)

    fun create_fixture_default (body : body, shape : BDDShape.shape,
                                data : body_data, density : real) : fixture =
        create_fixture (body,
                        { shape = shape,
                          data = data,
                          friction = 0.2,
                          restitution = 0.0,
                          density = density,
                          is_sensor = false,
                          filter = D.default_filter })

    fun destroy_fixture (body : body, fixture : fixture) : unit =
        raise BDDBody "unimplemented"
(*
        b2Assert(m_world->IsLocked() == false);
        if (m_world->IsLocked() == true)
        {
                return;
        }

        b2Assert(fixture->m_body == this);

        // Remove the fixture from this body's singly linked list.
        b2Assert(m_fixtureCount > 0);
        b2Fixture** node = &m_fixtureList;
        bool found = false;
        while ( *node != NULL)
        {
                if ( *node == fixture)
                {
                        *node = fixture->m_next;
                        found = true;
                        break;
                }

                node = &( *node)->m_next;
        }

        // You tried to remove a shape that is not attached to this body.
        b2Assert(found);

        // Destroy any contacts associated with the fixture.
        b2ContactEdge* edge = m_contactList;
        while (edge)
        {
                b2Contact* c = edge->contact;
                edge = edge->next;

                b2Fixture* fixtureA = c->GetFixtureA();
                b2Fixture* fixtureB = c->GetFixtureB();

                if (fixture == fixtureA || fixture == fixtureB)
                {
                        // This destroys the contact and removes it from
                        // this body's contact list.
                        m_world->m_contactManager.Destroy(c);
                }
        }

        b2BlockAllocator* allocator = &m_world->m_blockAllocator;

        if (m_flags & e_activeFlag)
        {
                b2Assert(fixture->m_proxyId != b2BroadPhase::e_nullProxy);
                b2BroadPhase* broadPhase = &m_world->m_contactManager.m_broadPhase;
                fixture->DestroyProxy(broadPhase);
        }
        else
        {
                b2Assert(fixture->m_proxyId == b2BroadPhase::e_nullProxy);
        }

        fixture->Destroy(allocator);
        fixture->m_body = NULL;
        fixture->m_next = NULL;
        fixture->~b2Fixture();
        allocator->Free(fixture, sizeof(b2Fixture));

        --m_fixtureCount;

        // Reset the mass data.
        ResetMassData();
*)

(* PSSt. lots of this stuff will have to move to dynamics, probably.
   check to make sure it's not already there! *)

    fun set_mass_data (b : body, mass_data : mass_data) : unit =
        let val world = D.B.get_world b
            val () = if D.W.get_flag (world, D.W.FLAG_LOCKED)
                     then raise BDDBody "can't set mass data while locked"
                     else ()
        in
            case D.B.get_typ b of
                D.Dynamic =>
                let
                    (* Port note: Assignment to inv_mass here is dead *)
                    val () = D.B.set_i (b, 0.0)
                    val () = D.B.set_inv_i (b, 0.0)
                        
                    val mass = if #mass mass_data <= 0.0
                               then 1.0
                               else #mass mass_data
                    val () = D.B.set_mass (b, mass)
                    val () = D.B.set_inv_mass (b, 1.0 / mass)

                    val () =
                    if #i mass_data > 0.0 andalso
                       not (D.B.get_flag (b, D.B.FLAG_FIXED_ROTATION))
                    then
                        let in
                            D.B.set_i (b, #i mass_data -
                                       D.B.get_mass b *
                                       dot2(#center mass_data,
                                            #center mass_data));
                            (* PERF *)
                            if D.B.get_i b > 0.0
                            then ()
                            else raise BDDBody "assertion failed";
                            D.B.set_inv_i (b, 1.0 / D.B.get_i b)
                        end
                    else ()

                    (* Move center of mass *)
                    val center = #center mass_data
                    val old_center : vec2 = sweepc (D.B.get_sweep b)
                    val c = D.B.get_xf b @*: center
                in
                    sweep_set_localcenter (D.B.get_sweep b, center);
                    sweep_set_c0 (D.B.get_sweep b, c);
                    sweep_set_c (D.B.get_sweep b, c);
                    (* Update center of mass velocity. *)
                    D.B.set_linear_velocity 
                    (b,
                     D.B.get_linear_velocity b :+: 
                     cross2sv(D.B.get_angular_velocity b,
                              c :-: old_center))
                end
              | _ => ()
      end


(* Advance, synchronizetransform are in dynamics *)

    fun set_transform (b, position : vec2, angle : real) : unit =
        raise BDDBody "unimplemented"
(*
        b2Assert(m_world->IsLocked() == false);
        if (m_world->IsLocked() == true)
        {
                return;
        }

        m_xf.R.Set(angle);
        m_xf.position = position;

        m_sweep.c0 = m_sweep.c = b2Mul(m_xf, m_sweep.localCenter);
        m_sweep.a0 = m_sweep.a = angle;

        b2BroadPhase* broadPhase = &m_world->m_contactManager.m_broadPhase;
        for (b2Fixture* f = m_fixtureList; f; f = f->m_next)
        {
                f->Synchronize(broadPhase, m_xf, m_xf);
        }

        m_world->m_contactManager.FindNewContacts();
*)

(*
void b2Body::SetType(b2BodyType type)
{
        if (m_type == type)
        {
                return;
        }

        m_type = type;

        ResetMassData();

        if (m_type == b2_staticBody)
        {
                m_linearVelocity.SetZero();
                m_angularVelocity = 0.0f;
        }

        SetAwake(true);

        m_force.SetZero();
        m_torque = 0.0f;

        // Since the body type changed, we need to flag contacts for filtering.
        for (b2ContactEdge* ce = m_contactList; ce; ce = ce->next)
        {
                ce->contact->FlagForFiltering();
        }
}


void b2Body::ResetMassData()
{
        // Compute mass data from shapes. Each shape has its own density.
        m_mass = 0.0f;
        m_invMass = 0.0f;
        m_I = 0.0f;
        m_invI = 0.0f;
        m_sweep.localCenter.SetZero();

        // Static and kinematic bodies have zero mass.
        if (m_type == b2_staticBody || m_type == b2_kinematicBody)
        {
                m_sweep.c0 = m_sweep.c = m_xf.position;
                return;
        }

        b2Assert(m_type == b2_dynamicBody);

        // Accumulate mass over all fixtures.
        b2Vec2 center = b2Vec2_zero;
        for (b2Fixture* f = m_fixtureList; f; f = f->m_next)
        {
                if (f->m_density == 0.0f)
                {
                        continue;
                }

                b2MassData massData;
                f->GetMassData(&massData);
                m_mass += massData.mass;
                center += massData.mass * massData.center;
                m_I += massData.I;
        }

        // Compute center of mass.
        if (m_mass > 0.0f)
        {
                m_invMass = 1.0f / m_mass;
                center *= m_invMass;
        }
        else
        {
                // Force all dynamic bodies to have a positive mass.
                m_mass = 1.0f;
                m_invMass = 1.0f;
        }

        if (m_I > 0.0f && (m_flags & e_fixedRotationFlag) == 0)
        {
                // Center the inertia about the center of mass.
                m_I -= m_mass * b2Dot(center, center);
                b2Assert(m_I > 0.0f);
                m_invI = 1.0f / m_I;

        }
        else
        {
                m_I = 0.0f;
                m_invI = 0.0f;
        }

        // Move center of mass.
        b2Vec2 oldCenter = m_sweep.c;
        m_sweep.localCenter = center;
        m_sweep.c0 = m_sweep.c = b2Mul(m_xf, m_sweep.localCenter);

        // Update center of mass velocity.
        m_linearVelocity += b2Cross(m_angularVelocity, m_sweep.c - oldCenter);
}

( * ShouldCollide is in D.B * )
    and synchronizefixtures

void b2Body::SetActive(bool flag)
{
        if (flag == IsActive())
        {
                return;
        }

        if (flag)
        {
                m_flags |= e_activeFlag;

                // Create all proxies.
                b2BroadPhase* broadPhase = &m_world->m_contactManager.m_broadPhase;
                for (b2Fixture* f = m_fixtureList; f; f = f->m_next)
                {
                        f->CreateProxy(broadPhase, m_xf);
                }

                // Contacts are created the next time step.
        }
        else
        {
                m_flags &= ~e_activeFlag;

                // Destroy all proxies.
                b2BroadPhase* broadPhase = &m_world->m_contactManager.m_broadPhase;
                for (b2Fixture* f = m_fixtureList; f; f = f->m_next)
                {
                        f->DestroyProxy(broadPhase);
                }

                // Destroy the attached contacts.
                b2ContactEdge* ce = m_contactList;
                while (ce)
                {
                        b2ContactEdge* ce0 = ce;
                        ce = ce->next;
                        m_world->m_contactManager.Destroy(ce0->contact);
                }
                m_contactList = NULL;
        }
}
*)

end
