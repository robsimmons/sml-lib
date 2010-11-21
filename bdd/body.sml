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

  fun create_fixture (body : body, def) : fixture =
    let val world = D.B.get_world body
        val () = if D.W.get_flag (world, D.W.FLAG_LOCKED)
                 then raise BDDBody "can't set mass data while locked"
                 else ()

        val f = D.F.new (body, def)
        val bp = D.W.get_broad_phase world
    in
        if D.B.get_flag (body, D.B.FLAG_ACTIVE)
        then D.F.create_proxy (f, bp, D.B.get_xf body)
        else ();

        D.F.set_next (f, D.B.get_fixture_list body);
        D.B.set_fixture_list (body, SOME f);
        D.B.set_fixture_count (body, 1 + D.B.get_fixture_count body);

        (* Adjust mass properties if needed. *)
        if D.F.get_density f > 0.0
        then reset_mass_data body
        else ();

        (* Let the world know we have a new fixture. This will cause 
           new contacts to be created at the beginning of the next 
           time step. *)
        D.W.set_flags (world, D.W.FLAG_NEW_FIXTURE);

        f
    end

    fun create_fixture_default (body : body, shape : BDDShape.shape,
                                data : fixture_data, density : real) : fixture =
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

    fun set_transform (b, position : vec2, angle : real) : unit =
        let val world = D.B.get_world b
            val () = if D.W.get_flag (world, D.W.FLAG_LOCKED)
                     then raise BDDBody "can't set transform data while locked"
                     else ()

            val xf = transform_pos_angle (position, angle)
            (* nb. so that we can modify it *)
            val sweep = D.B.get_sweep b
            val c = xf @*: sweeplocalcenter sweep

            val bp = D.W.get_broad_phase world
        in
            D.B.set_xf (b, xf);
            sweep_set_c0 (sweep, c);
            sweep_set_c (sweep, c);
            sweep_set_a0 (sweep, angle);
            sweep_set_a (sweep, angle);
            oapp D.F.get_next
            (fn fixture =>
             D.F.synchronize (fixture, bp, xf, xf)
             ) (D.B.get_fixture_list b);
            D.W.CM.find_new_contacts world
        end

    fun set_type (b : body, typ : D.body_type) =
        if D.B.get_typ b = typ
        then ()
        else
            let in
                D.B.set_typ (b, typ);
                reset_mass_data b;
                (if typ = D.Static
                 then (D.B.set_linear_velocity (b, vec2 (0.0, 0.0));
                       D.B.set_angular_velocity (b, 0.0))
                 else ());
                set_awake (b, true);
                D.B.set_force (b, vec2 (0.0, 0.0));
                D.B.set_torque (b, 0.0);
                (* Since the body type changed, we need to flag 
                   contacts for filtering. *)
                oapp D.E.get_next (D.C.flag_for_filtering o !! o
                                   D.E.get_contact)
                  (D.B.get_contact_list b)
            end

(* Advance, synchronizetransform are in dynamics *)
(* ShouldCollide is in D.B 
    and synchronizefixtures *)

end
