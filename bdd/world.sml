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

    fun destroy_joint (joint : joint) : unit =
        raise BDDWorld "unimplemented"

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

    fun step (world : world, time_step : real, 
              velocity_iterations : int, position_iterations : int) : unit =
        raise BDDWorld "unimplemented" (* b2world.cpp *)

  end

end
