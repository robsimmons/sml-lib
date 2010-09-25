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

  (* 16 category bits, then 16 mask bits; group index *)
(* XXXXXX
  type filter = Word32.word * int
  val default_filter = (0wx0001FFFF, 1)
  datatype body_type =
      Static
    | Kinematic
    | Dynamic
*)

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
  
  end


  structure World =
  struct

    open D.W
    type contact_impulse = D.contact_impulse
    datatype raycast_action = datatype D.raycast_action

    fun get_proxy_count world =
        BDDBroadPhase.proxy_count (get_broad_phase world)

    val set_should_collide_filter = set_should_collide

    fun create_body (world : world,
        { typ : Body.body_type,
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
        raise BDDWorld "unimplemented"

    fun destroy_body (body : body) : unit =
        raise BDDWorld "unimplemented"

    fun create_joint (world : world,
                      { typ : Joint.joint_type,
                        user_data : joint_data,
                        body_a : body,
                        body_b : body,
                        collide_connected : bool }) : joint =
        raise BDDWorld "unimplemented"

    fun destroy_joint (joint : joint) : unit =
        raise BDDWorld "unimplemented"

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
