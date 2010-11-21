(* Copyright 2010 Tom Murphy VII and Erin Catto. See COPYING for details. *)

(* The implementation of the dynamics engine has several complex,
   mutually-referential data structures. This defines the raw storage
   for these (body, fixture, world, contact, and joint) in a transparent
   way, as well as accessors for them. These are then used to implement
   the algorithms and abstract types in BDDWorld. This way we don't need
   the entire implementation in one file. Clients should not bother with
   this file. You can't even use it to get at the internals of dynamics
   types, because in the client interface those types are abstract.

   Corresponding to parts of dynamics/contacts/b2contact.h, dynamics/b2body.h,
   dynamics/b2fixture.h, dynamics/joints/b2joint.h, etc. *)
structure BDDDynamics =
struct
  open BDDSettings
  open BDDTypes
  open BDDMath
  open BDDOps
  infix 6 :+: :-: %-% %+% +++
  infix 7 *: *% +*: +*+ #*% @*:

  exception BDDDynamics of string

  (* 16 category bits, then 16 mask bits; group index *)
  type filter = Word32.word * int
  val default_filter = (0wx0001FFFF, 1)
  datatype body_type =
      Static
    | Kinematic
    | Dynamic

  datatype joint_type =
      Revolute
    | Prismatic
    | Distance
    | Pulley
    | Mouse
    | Gear
    | Line
    | Weld
    | Friction

  datatype limit_state =
      Inactive
    | AtLower
    | AtUpper
    | Equal

  fun !! _ (SOME r) = r
    | !! s NONE = raise BDDDynamics
      ("Expected non-NONE reference; corresponds to an unchecked NULL " ^
       "dereference in Box2D. This is probably because an element " ^
       "(e.g. fixture, joint) was used after being detached, " ^
       "or before being initialized: " ^ s)

  (* Action that the raycast callback can take upon encountering a fixture.
     Port note: These were all encoded as special float values in Box2D. *)
  datatype raycast_action =
      IgnoreAndContinue
    | Terminate
      (* Clip to the fraction of the ray; must be in (0, 1). *)
    | Clip of real
    | Don'tClip

  type contact_impulse = { normal_impulses : real array,
                           tangent_impulses : real array }

  (* Corresponding to Dynamics/b2TimeStep.h *)
  type time_step = { (* time step *)
                     dt : real,
                     (* inverse time step (0 if dt == 0) *)
                     inv_dt : real,
                     (* dt * inv_dt0 *)
                     dt_ratio : real,
                     velocity_iterations : int,
                     position_iterations : int,
                     warm_starting : bool }

  (* TODO(twm): Explain what's going on with jointedge and contactedge. *)

  (* Bodies and fixtures are refs to fuctional records
     ("cells" in local terminology). *)
  datatype ('b, 'f, 'j) bodycell =
      B of { typ : body_type,
             flags : Word8.word,
             island_index : int,
             (* Body origin transform *)
             xf : transform,
             (* Sept motion for CCD *)
             sweep : sweep,
             linear_velocity : vec2,
             angular_velocity : real,
             force : vec2,
             torque : real,
             world : ('b, 'f, 'j) worldcell ref,

             (* XXX option? *)
             prev : ('b, 'f, 'j) bodycell ref option,
             next : ('b, 'f, 'j) bodycell ref option,

             fixture_list : ('b, 'f, 'j) fixturecell ref option,
             fixture_count : int,

             joint_list : ('b, 'f, 'j) jointedgecell ref option,
             (* Port note: a pointer in body.h,
                but members in contact.h. Using refs everywhere. *)
             contact_list : ('b, 'f, 'j) contactedgecell ref option,

             mass : real,
             inv_mass : real,

             (* Rotational inertia about the center of mass. *)
             i : real,
             inv_i : real,

             linear_damping : real,
             angular_damping : real,

             sleep_time : real,
             data : 'b }

  and ('b, 'f, 'j) fixturecell =
      F of { aabb : aabb,
             density : real,
             next : ('b, 'f, 'j) fixturecell ref option,
             (* Should always be SOME unless deleted. *)
             body : ('b, 'f, 'j) bodycell ref option,
             shape : BDDShape.shape,
             friction : real,
             restitution : real,
             (* Broad phase proxy, where the user data is
                this fixture. *)
             proxy : ('b, 'f, 'j) fixturecell ref BDDBroadPhase.proxy option,
             filter : filter,
             sensor : bool,
             data : 'f }

  and ('b, 'f, 'j) contactcell =
      C of { flags : Word32.word,
             (* All contacts in the world. *)
             prev : ('b, 'f, 'j) contactcell ref option,
             next : ('b, 'f, 'j) contactcell ref option,
             (* nodes for connecting bodies *)
             node_a : ('b, 'f, 'j) contactedgecell ref,
             node_b : ('b, 'f, 'j) contactedgecell ref,
             (* PERF can probably make these non-optional.
                .new always creates them *)
             fixture_a : ('b, 'f, 'j) fixturecell ref option,
             fixture_b : ('b, 'f, 'j) fixturecell ref option,
             manifold : BDDTypes.manifold,
             toi_count : int }

  (* A contact edge is used to connect bodies and contacts together
     in a contact graph where each body is a node and each contact
     is an edge. A contact edge belongs to a doubly linked list
     maintained in each attached body. Each contact has two contact
     nodes, one for each attached body. *)
  and ('b, 'f, 'j) contactedgecell =
      E of { (* provides quick access to the other body attached. 
                PERF: Do these really need to be optional? 
                See World.ContactManager.add_pair. Could pass them
                to 'new' or do initialization in that function. *)
             other : ('b, 'f, 'j) bodycell ref option,
             contact : ('b, 'f, 'j) contactcell ref option,
             (* the previous and next contact edge in the 
                body's contact list *)
             prev : ('b, 'f, 'j) contactedgecell ref option,
             next : ('b, 'f, 'j) contactedgecell ref option }

  and ('b, 'f, 'j) jointcell = 
      J of { (* Port note: These were individual boolean fields
                in Box2D. *)
             flags : Word8.word,
             typ : joint_type,
             (* the previous and next joints in the world joint list. 
                the body joint lists are stored in joint edges. *)
             prev : ('b, 'f, 'j) jointcell ref option,
             next : ('b, 'f, 'j) jointcell ref option,

             edge_a : ('b, 'f, 'j) jointedgecell ref,
             edge_b : ('b, 'f, 'j) jointedgecell ref,
             
             body_a : ('b, 'f, 'j) bodycell ref,
             body_b : ('b, 'f, 'j) bodycell ref,

             data : 'j,
             
             (* Cache here per time step to reduce cache misses.
                PERF: This is probably not a good idea in the SML port. *)
             local_center_a : vec2,
             local_center_b : vec2,
             
             inv_mass_a : real,
             inv_i_a : real,
             inv_mass_b : real,
             inv_i_b : real }

  (* A joint edge is used to connect bodies and joints together
     in a joint graph where each body is a node and each joint
     is an edge. A joint edge belongs to a doubly linked list
     maintained in each attached body. Each joint has two joint
     nodes, one for each attached body. *)
  and ('b, 'f, 'j) jointedgecell = 
      G of { (* The other body of the joint. *)
             other : ('b, 'f, 'j) bodycell ref,
             (* The joint. *)
             joint : ('b, 'f, 'j) jointcell ref,
             (* The previous and next joint edges in the body's
                joint list. *)
             prev : ('b, 'f, 'j) jointedgecell ref option,
             next : ('b, 'f, 'j) jointedgecell ref option }

  and ('b, 'f, 'j) worldcell =
      W of { flags : Word32.word,

             body_list : ('b, 'f, 'j) bodycell ref option,
             joint_list : ('b, 'f, 'j) jointcell ref option,
             body_count : int,
             joint_count : int,
             gravity : BDDMath.vec2,
             (* Why not a flag? *)
             allow_sleep : bool,
             ground_body : ('b, 'f, 'j) bodycell ref option,
             goodbye_joint_hook : ('b, 'f, 'j) jointcell ref -> unit,
             goodbye_fixture_hook : ('b, 'f, 'j) fixturecell ref -> unit,
             (* Port note: Skipped debug drawing. *)

             (* This is used to compute the time step ratio to
                support a variable time step. *)
             inv_dt0 : real,
             (* For debugging the solver. Why not flags? *)
             warm_starting : bool,
             continuous_physics : bool,

             (* Port Note: Folded the "contact manager" object into the world
                object. *)
             (* The broad phase uses the userdata to point back to the 
                fixture cell. *)
             broad_phase : 
               ('b, 'f, 'j) fixturecell ref BDDBroadPhase.broadphase,
             contact_list : ('b, 'f, 'j) contactcell ref option,
             contact_count : int,
             should_collide : (('b, 'f, 'j) fixturecell ref * 
                               ('b, 'f, 'j) fixturecell ref -> bool),
             begin_contact : ('b, 'f, 'j) contactcell ref -> unit,
             end_contact : ('b, 'f, 'j) contactcell ref -> unit,
             pre_solve : ('b, 'f, 'j) contactcell ref * BDDTypes.manifold -> 
                         unit,
             post_solve : ('b, 'f, 'j) contactcell ref * contact_impulse -> 
                          unit }


  type ('b, 'f, 'j) fixture = ('b, 'f, 'j) fixturecell ref
  type ('b, 'f, 'j) body = ('b, 'f, 'j) bodycell ref
  type ('b, 'f, 'j) contact = ('b, 'f, 'j) contactcell ref
  type ('b, 'f, 'j) contactedge = ('b, 'f, 'j) contactedgecell ref
  type ('b, 'f, 'j) world = ('b, 'f, 'j) worldcell ref
  type ('b, 'f, 'j) joint = ('b, 'f, 'j) jointcell ref
  type ('b, 'f, 'j) jointedge = ('b, 'f, 'j) jointedgecell ref

  (* Internal, fixtures *)
  structure F =
  struct

    fun get_aabb (ref (F{ aabb, ... })) = aabb
    fun get_density (ref (F{ density, ... })) = density
    fun get_next (ref (F{ next, ... })) = next
    fun get_body (ref (F{ body, ... })) = !! "fbody" body
    fun get_shape (ref (F{ shape, ... })) = shape
    fun get_friction (ref (F{ friction, ... })) = friction
    fun get_restitution (ref (F{ restitution, ... })) = restitution
    fun get_filter (ref (F{ filter, ... })) = filter
    fun get_sensor (ref (F{ sensor, ... })) = sensor
    fun get_data (ref (F{ data, ... })) = data
    fun get_proxy (ref (F{ proxy, ... })) = proxy

    (* This is annoying, but the least error prone way to simulate what's
       happening in the C++ code. *)
    fun set_aabb (r as ref (F { aabb = _, density, next, body, shape, friction,
                                restitution, proxy, filter, sensor, data }),
                  aabb) =
        r := F { aabb = aabb, density = density, next = next, body = body,
                 shape = shape, friction = friction, restitution = restitution,
                 proxy = proxy, filter = filter, sensor = sensor, data = data }

    fun set_density (r as ref (F { aabb, density = _, next, body, shape, friction,
                                   restitution, proxy, filter, sensor, data }),
                  density) =
        (* XXX Box2D has check on range *)
        r := F { aabb = aabb, density = density, next = next, body = body,
                 shape = shape, friction = friction, restitution = restitution,
                 proxy = proxy, filter = filter, sensor = sensor, data = data }

    fun set_next (r as ref (F { aabb, density, next = _, body, shape, friction,
                                restitution, proxy, filter, sensor, data }),
                  next) =
        r := F { aabb = aabb, density = density, next = next, body = body,
                 shape = shape, friction = friction, restitution = restitution,
                 proxy = proxy, filter = filter, sensor = sensor, data = data }

    fun set_body (r as ref (F { aabb, density, next, body = _, shape, friction,
                                restitution, proxy, filter, sensor, data }),
                  body) =
        r := F { aabb = aabb, density = density, next = next, body = body,
                 shape = shape, friction = friction, restitution = restitution,
                 proxy = proxy, filter = filter, sensor = sensor, data = data }

    fun set_shape (r as ref (F { aabb, density, next, body, shape = _, friction,
                                 restitution, proxy, filter, sensor, data }),
                  shape) =
        r := F { aabb = aabb, density = density, next = next, body = body,
                 shape = shape, friction = friction, restitution = restitution,
                 proxy = proxy, filter = filter, sensor = sensor, data = data }

    fun set_friction (r as ref (F { aabb, density, next, body, shape, friction = _,
                                restitution, proxy, filter, sensor, data }),
                  friction) =
        r := F { aabb = aabb, density = density, next = next, body = body,
                 shape = shape, friction = friction, restitution = restitution,
                 proxy = proxy, filter = filter, sensor = sensor, data = data }

    fun set_restitution (r as ref (F { aabb, density, next, body, shape, friction,
                                       restitution = _, proxy, filter, sensor,
                                       data }), restitution) =
        r := F { aabb = aabb, density = density, next = next, body = body,
                 shape = shape, friction = friction, restitution = restitution,
                 proxy = proxy, filter = filter, sensor = sensor, data = data }

    fun set_proxy (r as ref (F { aabb, density, next, body, shape, friction,
                                 restitution, proxy = _, filter, sensor, data }),
                  proxy) =
        r := F { aabb = aabb, density = density, next = next, body = body,
                 shape = shape, friction = friction, restitution = restitution,
                 proxy = proxy, filter = filter, sensor = sensor, data = data }

    (* n.b. don't call directly; call through Fixture.set_filter. *)
    fun set_filter (r as ref (F { aabb, density, next, body, shape, friction,
                                restitution, proxy, filter = _, sensor, data }),
                  filter) =
        r := F { aabb = aabb, density = density, next = next, body = body,
                 shape = shape, friction = friction, restitution = restitution,
                 proxy = proxy, filter = filter, sensor = sensor, data = data }

    fun set_sensor (r as ref (F { aabb, density, next, body, shape, friction,
                                  restitution, proxy, filter, sensor = _, data }),
                  sensor) =
        r := F { aabb = aabb, density = density, next = next, body = body,
                 shape = shape, friction = friction, restitution = restitution,
                 proxy = proxy, filter = filter, sensor = sensor, data = data }

    fun set_data (r as ref (F { aabb, density, next, body, shape, friction,
                                restitution, proxy, filter, sensor, data = _ }),
                  data) =
        r := F { aabb = aabb, density = density, next = next, body = body,
                 shape = shape, friction = friction, restitution = restitution,
                 proxy = proxy, filter = filter, sensor = sensor, data = data }

    (* Used by body *)
    fun get_mass_data f =
      BDDShape.compute_mass (get_shape f, get_density f)

    (* Port note: Corresponding to b2Fixture::Create and new *)
    fun new (body, { data, friction, restitution, filter, is_sensor,
                     shape, density }) =
        ref (F { body = SOME body,
                 data = data,
                 friction = friction,
                 restitution = restitution,
                 filter = filter,
                 sensor = is_sensor,
                 shape = BDDShape.clone shape,
                 density = density,
                 next = NONE,
                 proxy = NONE,
                 (* PERF uninitialized in box2d *)
                 aabb = { lowerbound = vec2 (0.0, 0.0),
                          upperbound = vec2 (0.0, 0.0) }
                 })

    (* Create a proxy for the fixture in the broad phase, and store it in
       fixture. *)
    fun create_proxy 
        (fixture : ('b, 'f, 'j) fixture, 
         broadphase : ('b, 'f, 'j) fixture BDDBroadPhase.broadphase, 
         xf : BDDMath.transform) : unit =
        case get_proxy fixture of
            NONE =>
                (* Create proxy in the broad-phase. *)
                let 
                    val aabb = BDDShape.compute_aabb (get_shape fixture, xf)
                in
                    set_aabb (fixture, aabb);
                    set_proxy (fixture,
                               SOME (BDDBroadPhase.create_proxy (broadphase, 
                                                                 aabb, 
                                                                 fixture)))
                end
          | SOME _ =>
                raise BDDDynamics "fixture already had a proxy in create_proxy"

    fun destroy_proxy 
        (fixture : ('b, 'f, 'j) fixture, 
         broadphase : ('b, 'f, 'j) fixture BDDBroadPhase.broadphase) =
        (* Proxy can be missing if the body is not active. *)
        case get_proxy fixture of
            NONE => ()
          | SOME p => 
                let in
                    BDDBroadPhase.remove_proxy (broadphase, p);
                    set_proxy (fixture, NONE)
                end

    fun synchronize (fixture : ('b, 'f, 'j) fixture,
                     broadphase : ('b, 'f, 'j) fixture BDDBroadPhase.broadphase,
                     transform1 : transform,
                     transform2 : transform) =
        case get_proxy fixture of
            NONE => ()
          | SOME proxy =>
          (* Compute an AABB that covers the swept shape (may miss some
             rotation effect). *)
          let val shape = get_shape fixture
              val aabb1 = BDDShape.compute_aabb (shape, transform1)
              val aabb2 = BDDShape.compute_aabb (shape, transform2)
              val aabb = BDDCollision.aabb_combine (aabb1, aabb2)

              val displacement : vec2 = 
                  transformposition transform2 :-:
                  transformposition transform1
          in
              set_aabb (fixture, aabb);
              BDDBroadPhase.move_proxy (broadphase, proxy, aabb, displacement)
          end

  end

  (* Internal, joint edges *)
  structure G =
  struct
    fun get_other (ref (G{ other, ... })) = other
    fun get_joint (ref (G{ joint, ... })) = joint
    fun get_prev (ref (G{ prev, ... })) = prev
    fun get_next (ref (G{ next, ... })) = next
    fun set_other (r as ref (G{ other = _, joint, prev, next }), other) = 
        r := G { other = other, joint = joint, prev = prev, next = next}
    fun set_joint (r as ref (G{ other, joint = _, prev, next }), joint) = 
        r := G { other = other, joint = joint, prev = prev, next = next}
    fun set_prev (r as ref (G{ other, joint, prev = _, next }), prev) = 
        r := G { other = other, joint = joint, prev = prev, next = next}
    fun set_next (r as ref (G{ other, joint, prev, next = _ }), next) = 
        r := G { other = other, joint = joint, prev = prev, next = next}
  end

  (* Internal, joints *)
  structure J =
  struct
    val FLAG_ISLAND = 0wx1 : Word8.word
    val FLAG_COLLIDE_CONNECTED = 0wx2 : Word8.word

    fun get_flags (ref (J{ flags, ... })) = flags
    fun get_typ (ref (J{ typ, ... })) = typ
    fun get_prev (ref (J{ prev, ... })) = prev
    fun get_next (ref (J{ next, ... })) = next
    fun get_edge_a (ref (J{ edge_a, ... })) = edge_a
    fun get_edge_b (ref (J{ edge_b, ... })) = edge_b
    fun get_body_a (ref (J{ body_a, ... })) = body_a
    fun get_body_b (ref (J{ body_b, ... })) = body_b
    fun get_data (ref (J{ data, ... })) = data
    fun get_local_center_a (ref (J{ local_center_a, ... })) = local_center_a
    fun get_local_center_b (ref (J{ local_center_b, ... })) = local_center_b
    fun get_inv_mass_a (ref (J{ inv_mass_a, ... })) = inv_mass_a
    fun get_inv_i_a (ref (J{ inv_i_a, ... })) = inv_i_a
    fun get_inv_mass_b (ref (J{ inv_mass_b, ... })) = inv_mass_b
    fun get_inv_i_b (ref (J{ inv_i_b, ... })) = inv_i_b

    fun set_flags (r as ref (J{ flags = _, typ, prev, next, edge_a, edge_b, body_a, body_b, data, local_center_a, local_center_b, inv_mass_a, inv_i_a, inv_mass_b, inv_i_b }), flags) = r := J { flags = flags, typ = typ, prev = prev, next = next, edge_a = edge_a, edge_b = edge_b, body_a = body_a, body_b = body_b, data = data, local_center_a = local_center_a, local_center_b = local_center_b, inv_mass_a = inv_mass_a, inv_i_a = inv_i_a, inv_mass_b = inv_mass_b, inv_i_b = inv_i_b}
    fun set_typ (r as ref (J{ flags, typ = _, prev, next, edge_a, edge_b, body_a, body_b, data, local_center_a, local_center_b, inv_mass_a, inv_i_a, inv_mass_b, inv_i_b }), typ) = r := J { flags = flags, typ = typ, prev = prev, next = next, edge_a = edge_a, edge_b = edge_b, body_a = body_a, body_b = body_b, data = data, local_center_a = local_center_a, local_center_b = local_center_b, inv_mass_a = inv_mass_a, inv_i_a = inv_i_a, inv_mass_b = inv_mass_b, inv_i_b = inv_i_b}
    fun set_prev (r as ref (J{ flags, typ, prev = _, next, edge_a, edge_b, body_a, body_b, data, local_center_a, local_center_b, inv_mass_a, inv_i_a, inv_mass_b, inv_i_b }), prev) = r := J { flags = flags, typ = typ, prev = prev, next = next, edge_a = edge_a, edge_b = edge_b, body_a = body_a, body_b = body_b, data = data, local_center_a = local_center_a, local_center_b = local_center_b, inv_mass_a = inv_mass_a, inv_i_a = inv_i_a, inv_mass_b = inv_mass_b, inv_i_b = inv_i_b}
    fun set_next (r as ref (J{ flags, typ, prev, next = _, edge_a, edge_b, body_a, body_b, data, local_center_a, local_center_b, inv_mass_a, inv_i_a, inv_mass_b, inv_i_b }), next) = r := J { flags = flags, typ = typ, prev = prev, next = next, edge_a = edge_a, edge_b = edge_b, body_a = body_a, body_b = body_b, data = data, local_center_a = local_center_a, local_center_b = local_center_b, inv_mass_a = inv_mass_a, inv_i_a = inv_i_a, inv_mass_b = inv_mass_b, inv_i_b = inv_i_b}
    fun set_edge_a (r as ref (J{ flags, typ, prev, next, edge_a = _, edge_b, body_a, body_b, data, local_center_a, local_center_b, inv_mass_a, inv_i_a, inv_mass_b, inv_i_b }), edge_a) = r := J { flags = flags, typ = typ, prev = prev, next = next, edge_a = edge_a, edge_b = edge_b, body_a = body_a, body_b = body_b, data = data, local_center_a = local_center_a, local_center_b = local_center_b, inv_mass_a = inv_mass_a, inv_i_a = inv_i_a, inv_mass_b = inv_mass_b, inv_i_b = inv_i_b}
    fun set_edge_b (r as ref (J{ flags, typ, prev, next, edge_a, edge_b = _, body_a, body_b, data, local_center_a, local_center_b, inv_mass_a, inv_i_a, inv_mass_b, inv_i_b }), edge_b) = r := J { flags = flags, typ = typ, prev = prev, next = next, edge_a = edge_a, edge_b = edge_b, body_a = body_a, body_b = body_b, data = data, local_center_a = local_center_a, local_center_b = local_center_b, inv_mass_a = inv_mass_a, inv_i_a = inv_i_a, inv_mass_b = inv_mass_b, inv_i_b = inv_i_b}
    fun set_body_a (r as ref (J{ flags, typ, prev, next, edge_a, edge_b, body_a = _, body_b, data, local_center_a, local_center_b, inv_mass_a, inv_i_a, inv_mass_b, inv_i_b }), body_a) = r := J { flags = flags, typ = typ, prev = prev, next = next, edge_a = edge_a, edge_b = edge_b, body_a = body_a, body_b = body_b, data = data, local_center_a = local_center_a, local_center_b = local_center_b, inv_mass_a = inv_mass_a, inv_i_a = inv_i_a, inv_mass_b = inv_mass_b, inv_i_b = inv_i_b}
    fun set_body_b (r as ref (J{ flags, typ, prev, next, edge_a, edge_b, body_a, body_b = _, data, local_center_a, local_center_b, inv_mass_a, inv_i_a, inv_mass_b, inv_i_b }), body_b) = r := J { flags = flags, typ = typ, prev = prev, next = next, edge_a = edge_a, edge_b = edge_b, body_a = body_a, body_b = body_b, data = data, local_center_a = local_center_a, local_center_b = local_center_b, inv_mass_a = inv_mass_a, inv_i_a = inv_i_a, inv_mass_b = inv_mass_b, inv_i_b = inv_i_b}
    fun set_data (r as ref (J{ flags, typ, prev, next, edge_a, edge_b, body_a, body_b, data = _, local_center_a, local_center_b, inv_mass_a, inv_i_a, inv_mass_b, inv_i_b }), data) = r := J { flags = flags, typ = typ, prev = prev, next = next, edge_a = edge_a, edge_b = edge_b, body_a = body_a, body_b = body_b, data = data, local_center_a = local_center_a, local_center_b = local_center_b, inv_mass_a = inv_mass_a, inv_i_a = inv_i_a, inv_mass_b = inv_mass_b, inv_i_b = inv_i_b}
    fun set_local_center_a (r as ref (J{ flags, typ, prev, next, edge_a, edge_b, body_a, body_b, data, local_center_a = _, local_center_b, inv_mass_a, inv_i_a, inv_mass_b, inv_i_b }), local_center_a) = r := J { flags = flags, typ = typ, prev = prev, next = next, edge_a = edge_a, edge_b = edge_b, body_a = body_a, body_b = body_b, data = data, local_center_a = local_center_a, local_center_b = local_center_b, inv_mass_a = inv_mass_a, inv_i_a = inv_i_a, inv_mass_b = inv_mass_b, inv_i_b = inv_i_b}
    fun set_local_center_b (r as ref (J{ flags, typ, prev, next, edge_a, edge_b, body_a, body_b, data, local_center_a, local_center_b = _, inv_mass_a, inv_i_a, inv_mass_b, inv_i_b }), local_center_b) = r := J { flags = flags, typ = typ, prev = prev, next = next, edge_a = edge_a, edge_b = edge_b, body_a = body_a, body_b = body_b, data = data, local_center_a = local_center_a, local_center_b = local_center_b, inv_mass_a = inv_mass_a, inv_i_a = inv_i_a, inv_mass_b = inv_mass_b, inv_i_b = inv_i_b}
    fun set_inv_mass_a (r as ref (J{ flags, typ, prev, next, edge_a, edge_b, body_a, body_b, data, local_center_a, local_center_b, inv_mass_a = _, inv_i_a, inv_mass_b, inv_i_b }), inv_mass_a) = r := J { flags = flags, typ = typ, prev = prev, next = next, edge_a = edge_a, edge_b = edge_b, body_a = body_a, body_b = body_b, data = data, local_center_a = local_center_a, local_center_b = local_center_b, inv_mass_a = inv_mass_a, inv_i_a = inv_i_a, inv_mass_b = inv_mass_b, inv_i_b = inv_i_b}
    fun set_inv_i_a (r as ref (J{ flags, typ, prev, next, edge_a, edge_b, body_a, body_b, data, local_center_a, local_center_b, inv_mass_a, inv_i_a = _, inv_mass_b, inv_i_b }), inv_i_a) = r := J { flags = flags, typ = typ, prev = prev, next = next, edge_a = edge_a, edge_b = edge_b, body_a = body_a, body_b = body_b, data = data, local_center_a = local_center_a, local_center_b = local_center_b, inv_mass_a = inv_mass_a, inv_i_a = inv_i_a, inv_mass_b = inv_mass_b, inv_i_b = inv_i_b}
    fun set_inv_mass_b (r as ref (J{ flags, typ, prev, next, edge_a, edge_b, body_a, body_b, data, local_center_a, local_center_b, inv_mass_a, inv_i_a, inv_mass_b = _, inv_i_b }), inv_mass_b) = r := J { flags = flags, typ = typ, prev = prev, next = next, edge_a = edge_a, edge_b = edge_b, body_a = body_a, body_b = body_b, data = data, local_center_a = local_center_a, local_center_b = local_center_b, inv_mass_a = inv_mass_a, inv_i_a = inv_i_a, inv_mass_b = inv_mass_b, inv_i_b = inv_i_b}
    fun set_inv_i_b (r as ref (J{ flags, typ, prev, next, edge_a, edge_b, body_a, body_b, data, local_center_a, local_center_b, inv_mass_a, inv_i_a, inv_mass_b, inv_i_b = _ }), inv_i_b) = r := J { flags = flags, typ = typ, prev = prev, next = next, edge_a = edge_a, edge_b = edge_b, body_a = body_a, body_b = body_b, data = data, local_center_a = local_center_a, local_center_b = local_center_b, inv_mass_a = inv_mass_a, inv_i_a = inv_i_a, inv_mass_b = inv_mass_b, inv_i_b = inv_i_b}

    fun get_flag (j, f) = Word8.andb (f, get_flags j) <> 0w0
    fun set_flag (j, f) = set_flags (j, Word8.orb(get_flags j, f))
    fun clear_flag (j, f) = set_flags (j, Word8.andb(get_flags j, Word8.notb f))

    fun get_collide_connected j = get_flag(j, FLAG_COLLIDE_CONNECTED)

    (* Used in island solver *)
    fun init_velocity_constraints (j : ('b, 'f, 'j) joint,
                                   step : time_step) : unit =
        raise BDDDynamics "unimplemented"

    (* Used in island solver *)
    fun solve_velocity_constraints (j : ('b, 'f, 'j) joint,
                                    step : time_step) : unit =
        raise BDDDynamics "unimplemented"

    (* Used in island solver *)
    fun solve_position_constraints (j : ('b, 'f, 'j) joint,
                                    baumgarte : real) : bool =
        raise BDDDynamics "unimplemented"

  end

  (* Internal, bodies *)
  structure B =
  struct
    (* Port note: Using Word8, not Word16, since it is more portable
       and probably faster. *)
    val FLAG_ISLAND = 0wx1 : Word8.word
    val FLAG_AWAKE  = 0wx2 : Word8.word
    val FLAG_AUTO_SLEEP = 0wx4 : Word8.word
    val FLAG_BULLET = 0wx8 : Word8.word
    val FLAG_FIXED_ROTATION = 0wx10 : Word8.word
    val FLAG_ACTIVE = 0wx20 : Word8.word
    val FLAG_TOI = 0wx40 : Word8.word

    fun get_typ (ref (B{ typ, ... })) = typ
    fun get_flags (ref (B{ flags, ... })) = flags
    fun get_island_index (ref (B{ island_index, ... })) = island_index
    fun get_xf (ref (B{ xf, ... })) = xf
    fun get_sweep (ref (B{ sweep, ... })) = sweep
    fun get_linear_velocity (ref (B{ linear_velocity, ... })) = linear_velocity
    fun get_angular_velocity (ref (B{ angular_velocity, ... })) = angular_velocity
    fun get_force (ref (B{ force, ... })) = force
    fun get_torque (ref (B{ torque, ... })) = torque
    fun get_prev (ref (B{ prev, ... })) = prev
    fun get_next (ref (B{ next, ... })) = next
    fun get_fixture_list (ref (B{ fixture_list, ... })) = fixture_list
    fun get_fixture_count (ref (B{ fixture_count, ... })) = fixture_count
    fun get_joint_list (ref (B{ joint_list, ... })) = joint_list
    fun get_contact_list (ref (B{ contact_list, ... })) = contact_list
    fun get_mass (ref (B{ mass, ... })) = mass
    fun get_inv_mass (ref (B{ inv_mass, ... })) = inv_mass
    fun get_i (ref (B{ i, ... })) = i
    fun get_inv_i (ref (B{ inv_i, ... })) = inv_i
    fun get_linear_damping (ref (B{ linear_damping, ... })) = linear_damping
    fun get_angular_damping (ref (B{ angular_damping, ... })) = angular_damping
    fun get_sleep_time (ref (B{ sleep_time, ... })) = sleep_time
    fun get_world (ref (B{ world, ... })) = world
    fun get_data (ref (B{ data, ... })) = data

    fun set_typ (r as ref (B { typ = _, flags, island_index, xf, sweep,
                               linear_velocity, angular_velocity, force, torque,
                               prev, next, fixture_list, fixture_count,
                               joint_list, contact_list, mass, inv_mass, i,
                               inv_i, linear_damping, angular_damping,
                               sleep_time, data, world }), typ) =
        r := B
        { world = world, typ = typ, flags = flags, island_index = island_index,
          xf = xf, sweep = sweep, linear_velocity = linear_velocity,
          angular_velocity = angular_velocity, force = force, torque = torque,
          prev = prev, next = next, fixture_list = fixture_list, data = data,
          fixture_count = fixture_count, joint_list = joint_list,
          contact_list = contact_list, mass = mass, inv_mass = inv_mass,
          i = i, inv_i = inv_i, linear_damping = linear_damping,
          angular_damping = angular_damping, sleep_time = sleep_time }

    fun set_flags (r as ref (B { typ, flags = _, island_index, xf, sweep,
                                 linear_velocity, angular_velocity, force, torque,
                                 prev, next, fixture_list, fixture_count,
                                 joint_list, contact_list, mass, inv_mass, i,
                                 inv_i, linear_damping, angular_damping,
                                 sleep_time, data, world }), flags) =
        r := B
        { world = world, typ = typ, flags = flags, island_index = island_index,
          xf = xf, sweep = sweep, linear_velocity = linear_velocity,
          angular_velocity = angular_velocity, force = force, torque = torque,
          prev = prev, next = next, fixture_list = fixture_list, data = data,
          fixture_count = fixture_count, joint_list = joint_list,
          contact_list = contact_list, mass = mass, inv_mass = inv_mass,
          i = i, inv_i = inv_i, linear_damping = linear_damping,
          angular_damping = angular_damping, sleep_time = sleep_time }

    fun set_island_index (r as ref (B { typ, flags, island_index = _, xf, sweep,
                               linear_velocity, angular_velocity, force, torque,
                               prev, next, fixture_list, fixture_count,
                               joint_list, contact_list, mass, inv_mass, i,
                               inv_i, linear_damping, angular_damping,
                               sleep_time, data, world }), island_index) =
        r := B
        { world = world, typ = typ, flags = flags, island_index = island_index,
          xf = xf, sweep = sweep, linear_velocity = linear_velocity,
          angular_velocity = angular_velocity, force = force, torque = torque,
          prev = prev, next = next, fixture_list = fixture_list, data = data,
          fixture_count = fixture_count, joint_list = joint_list,
          contact_list = contact_list, mass = mass, inv_mass = inv_mass,
          i = i, inv_i = inv_i, linear_damping = linear_damping,
          angular_damping = angular_damping, sleep_time = sleep_time }

    fun set_xf (r as ref (B { typ, flags, island_index, xf = _, sweep,
                               linear_velocity, angular_velocity, force, torque,
                               prev, next, fixture_list, fixture_count,
                               joint_list, contact_list, mass, inv_mass, i,
                               inv_i, linear_damping, angular_damping,
                               sleep_time, data, world }), xf) =
        r := B
        { world = world, typ = typ, flags = flags, island_index = island_index,
          xf = xf, sweep = sweep, linear_velocity = linear_velocity,
          angular_velocity = angular_velocity, force = force, torque = torque,
          prev = prev, next = next, fixture_list = fixture_list, data = data,
          fixture_count = fixture_count, joint_list = joint_list,
          contact_list = contact_list, mass = mass, inv_mass = inv_mass,
          i = i, inv_i = inv_i, linear_damping = linear_damping,
          angular_damping = angular_damping, sleep_time = sleep_time }

    fun set_sweep (r as ref (B { typ, flags, island_index, xf, sweep = _,
                               linear_velocity, angular_velocity, force, torque,
                               prev, next, fixture_list, fixture_count,
                               joint_list, contact_list, mass, inv_mass, i,
                               inv_i, linear_damping, angular_damping,
                               sleep_time, data, world }), sweep) =
        r := B
        { world = world, typ = typ, flags = flags, island_index = island_index,
          xf = xf, sweep = sweep, linear_velocity = linear_velocity,
          angular_velocity = angular_velocity, force = force, torque = torque,
          prev = prev, next = next, fixture_list = fixture_list, data = data,
          fixture_count = fixture_count, joint_list = joint_list,
          contact_list = contact_list, mass = mass, inv_mass = inv_mass,
          i = i, inv_i = inv_i, linear_damping = linear_damping,
          angular_damping = angular_damping, sleep_time = sleep_time }

    fun set_linear_velocity (r as ref (B { typ, flags, island_index, xf, sweep,
                               linear_velocity = _, angular_velocity, force, torque,
                               prev, next, fixture_list, fixture_count,
                               joint_list, contact_list, mass, inv_mass, i,
                               inv_i, linear_damping, angular_damping,
                               sleep_time, data, world }), linear_velocity) =
        r := B
        { world = world, typ = typ, flags = flags, island_index = island_index,
          xf = xf, sweep = sweep, linear_velocity = linear_velocity,
          angular_velocity = angular_velocity, force = force, torque = torque,
          prev = prev, next = next, fixture_list = fixture_list, data = data,
          fixture_count = fixture_count, joint_list = joint_list,
          contact_list = contact_list, mass = mass, inv_mass = inv_mass,
          i = i, inv_i = inv_i, linear_damping = linear_damping,
          angular_damping = angular_damping, sleep_time = sleep_time }

    fun set_angular_velocity (r as ref (B { typ, flags, island_index, xf, sweep,
                               linear_velocity, angular_velocity = _, force, torque,
                               prev, next, fixture_list, fixture_count,
                               joint_list, contact_list, mass, inv_mass, i,
                               inv_i, linear_damping, angular_damping,
                               sleep_time, data, world }), angular_velocity) =
        r := B
        { world = world, typ = typ, flags = flags, island_index = island_index,
          xf = xf, sweep = sweep, linear_velocity = linear_velocity,
          angular_velocity = angular_velocity, force = force, torque = torque,
          prev = prev, next = next, fixture_list = fixture_list, data = data,
          fixture_count = fixture_count, joint_list = joint_list,
          contact_list = contact_list, mass = mass, inv_mass = inv_mass,
          i = i, inv_i = inv_i, linear_damping = linear_damping,
          angular_damping = angular_damping, sleep_time = sleep_time }

    fun set_force (r as ref (B { typ, flags, island_index, xf, sweep,
                               linear_velocity, angular_velocity, force = _, torque,
                               prev, next, fixture_list, fixture_count,
                               joint_list, contact_list, mass, inv_mass, i,
                               inv_i, linear_damping, angular_damping,
                               sleep_time, data, world }), force) =
        r := B
        { world = world, typ = typ, flags = flags, island_index = island_index,
          xf = xf, sweep = sweep, linear_velocity = linear_velocity,
          angular_velocity = angular_velocity, force = force, torque = torque,
          prev = prev, next = next, fixture_list = fixture_list, data = data,
          fixture_count = fixture_count, joint_list = joint_list,
          contact_list = contact_list, mass = mass, inv_mass = inv_mass,
          i = i, inv_i = inv_i, linear_damping = linear_damping,
          angular_damping = angular_damping, sleep_time = sleep_time }

    fun set_torque (r as ref (B { typ, flags, island_index, xf, sweep,
                               linear_velocity, angular_velocity, force, torque = _,
                               prev, next, fixture_list, fixture_count,
                               joint_list, contact_list, mass, inv_mass, i,
                               inv_i, linear_damping, angular_damping,
                               sleep_time, data, world }), torque) =
        r := B
        { world = world, typ = typ, flags = flags, island_index = island_index,
          xf = xf, sweep = sweep, linear_velocity = linear_velocity,
          angular_velocity = angular_velocity, force = force, torque = torque,
          prev = prev, next = next, fixture_list = fixture_list, data = data,
          fixture_count = fixture_count, joint_list = joint_list,
          contact_list = contact_list, mass = mass, inv_mass = inv_mass,
          i = i, inv_i = inv_i, linear_damping = linear_damping,
          angular_damping = angular_damping, sleep_time = sleep_time }

    fun set_prev (r as ref (B { typ, flags, island_index, xf, sweep,
                               linear_velocity, angular_velocity, force, torque,
                               prev = _, next, fixture_list, fixture_count,
                               joint_list, contact_list, mass, inv_mass, i,
                               inv_i, linear_damping, angular_damping,
                               sleep_time, data, world }), prev) =
        r := B
        { world = world, typ = typ, flags = flags, island_index = island_index,
          xf = xf, sweep = sweep, linear_velocity = linear_velocity,
          angular_velocity = angular_velocity, force = force, torque = torque,
          prev = prev, next = next, fixture_list = fixture_list, data = data,
          fixture_count = fixture_count, joint_list = joint_list,
          contact_list = contact_list, mass = mass, inv_mass = inv_mass,
          i = i, inv_i = inv_i, linear_damping = linear_damping,
          angular_damping = angular_damping, sleep_time = sleep_time }

    fun set_next (r as ref (B { typ, flags, island_index, xf, sweep,
                               linear_velocity, angular_velocity, force, torque,
                               prev, next = _, fixture_list, fixture_count,
                               joint_list, contact_list, mass, inv_mass, i,
                               inv_i, linear_damping, angular_damping,
                               sleep_time, data, world }), next) =
        r := B
        { world = world, typ = typ, flags = flags, island_index = island_index,
          xf = xf, sweep = sweep, linear_velocity = linear_velocity,
          angular_velocity = angular_velocity, force = force, torque = torque,
          prev = prev, next = next, fixture_list = fixture_list, data = data,
          fixture_count = fixture_count, joint_list = joint_list,
          contact_list = contact_list, mass = mass, inv_mass = inv_mass,
          i = i, inv_i = inv_i, linear_damping = linear_damping,
          angular_damping = angular_damping, sleep_time = sleep_time }

    fun set_fixture_list (r as ref (B { typ, flags, island_index, xf, sweep,
                               linear_velocity, angular_velocity, force, torque,
                               prev, next, fixture_list = _, fixture_count,
                               joint_list, contact_list, mass, inv_mass, i,
                               inv_i, linear_damping, angular_damping,
                               sleep_time, data, world }), fixture_list) =
        r := B
        { world = world, typ = typ, flags = flags, island_index = island_index,
          xf = xf, sweep = sweep, linear_velocity = linear_velocity,
          angular_velocity = angular_velocity, force = force, torque = torque,
          prev = prev, next = next, fixture_list = fixture_list, data = data,
          fixture_count = fixture_count, joint_list = joint_list,
          contact_list = contact_list, mass = mass, inv_mass = inv_mass,
          i = i, inv_i = inv_i, linear_damping = linear_damping,
          angular_damping = angular_damping, sleep_time = sleep_time }

    fun set_fixture_count (r as ref (B { typ, flags, island_index, xf, sweep,
                               linear_velocity, angular_velocity, force, torque,
                               prev, next, fixture_list, fixture_count = _,
                               joint_list, contact_list, mass, inv_mass, i,
                               inv_i, linear_damping, angular_damping,
                               sleep_time, data, world }), fixture_count) =
        r := B
        { world = world, typ = typ, flags = flags, island_index = island_index,
          xf = xf, sweep = sweep, linear_velocity = linear_velocity,
          angular_velocity = angular_velocity, force = force, torque = torque,
          prev = prev, next = next, fixture_list = fixture_list, data = data,
          fixture_count = fixture_count, joint_list = joint_list,
          contact_list = contact_list, mass = mass, inv_mass = inv_mass,
          i = i, inv_i = inv_i, linear_damping = linear_damping,
          angular_damping = angular_damping, sleep_time = sleep_time }

    fun set_joint_list (r as ref (B { typ, flags, island_index, xf, sweep,
                               linear_velocity, angular_velocity, force, torque,
                               prev, next, fixture_list, fixture_count,
                               joint_list = _, contact_list, mass, inv_mass, i,
                               inv_i, linear_damping, angular_damping,
                               sleep_time, data, world }), joint_list) =
        r := B
        { world = world, typ = typ, flags = flags, island_index = island_index,
          xf = xf, sweep = sweep, linear_velocity = linear_velocity,
          angular_velocity = angular_velocity, force = force, torque = torque,
          prev = prev, next = next, fixture_list = fixture_list, data = data,
          fixture_count = fixture_count, joint_list = joint_list,
          contact_list = contact_list, mass = mass, inv_mass = inv_mass,
          i = i, inv_i = inv_i, linear_damping = linear_damping,
          angular_damping = angular_damping, sleep_time = sleep_time }

    fun set_contact_list (r as ref (B { typ, flags, island_index, xf, sweep,
                               linear_velocity, angular_velocity, force, torque,
                               prev, next, fixture_list, fixture_count,
                               joint_list, contact_list = _, mass, inv_mass, i,
                               inv_i, linear_damping, angular_damping,
                               sleep_time, data, world }), contact_list) =
        r := B
        { world = world, typ = typ, flags = flags, island_index = island_index,
          xf = xf, sweep = sweep, linear_velocity = linear_velocity,
          angular_velocity = angular_velocity, force = force, torque = torque,
          prev = prev, next = next, fixture_list = fixture_list, data = data,
          fixture_count = fixture_count, joint_list = joint_list,
          contact_list = contact_list, mass = mass, inv_mass = inv_mass,
          i = i, inv_i = inv_i, linear_damping = linear_damping,
          angular_damping = angular_damping, sleep_time = sleep_time }

    fun set_mass (r as ref (B { typ, flags, island_index, xf, sweep,
                               linear_velocity, angular_velocity, force, torque,
                               prev, next, fixture_list, fixture_count,
                               joint_list, contact_list, mass = _, inv_mass, i,
                               inv_i, linear_damping, angular_damping,
                               sleep_time, data, world }), mass) =
        r := B
        { world = world, typ = typ, flags = flags, island_index = island_index,
          xf = xf, sweep = sweep, linear_velocity = linear_velocity,
          angular_velocity = angular_velocity, force = force, torque = torque,
          prev = prev, next = next, fixture_list = fixture_list, data = data,
          fixture_count = fixture_count, joint_list = joint_list,
          contact_list = contact_list, mass = mass, inv_mass = inv_mass,
          i = i, inv_i = inv_i, linear_damping = linear_damping,
          angular_damping = angular_damping, sleep_time = sleep_time }

    fun set_inv_mass (r as ref (B { typ, flags, island_index, xf, sweep,
                               linear_velocity, angular_velocity, force, torque,
                               prev, next, fixture_list, fixture_count,
                               joint_list, contact_list, mass, inv_mass = _, i,
                               inv_i, linear_damping, angular_damping,
                               sleep_time, data, world }), inv_mass) =
        r := B
        { world = world, typ = typ, flags = flags, island_index = island_index,
          xf = xf, sweep = sweep, linear_velocity = linear_velocity,
          angular_velocity = angular_velocity, force = force, torque = torque,
          prev = prev, next = next, fixture_list = fixture_list, data = data,
          fixture_count = fixture_count, joint_list = joint_list,
          contact_list = contact_list, mass = mass, inv_mass = inv_mass,
          i = i, inv_i = inv_i, linear_damping = linear_damping,
          angular_damping = angular_damping, sleep_time = sleep_time }

    fun set_i (r as ref (B { typ, flags, island_index, xf, sweep,
                               linear_velocity, angular_velocity, force, torque,
                               prev, next, fixture_list, fixture_count,
                               joint_list, contact_list, mass, inv_mass, i = _,
                               inv_i, linear_damping, angular_damping,
                               sleep_time, data, world }), i) =
        r := B
        { world = world, typ = typ, flags = flags, island_index = island_index,
          xf = xf, sweep = sweep, linear_velocity = linear_velocity,
          angular_velocity = angular_velocity, force = force, torque = torque,
          prev = prev, next = next, fixture_list = fixture_list, data = data,
          fixture_count = fixture_count, joint_list = joint_list,
          contact_list = contact_list, mass = mass, inv_mass = inv_mass,
          i = i, inv_i = inv_i, linear_damping = linear_damping,
          angular_damping = angular_damping, sleep_time = sleep_time }

    fun set_inv_i (r as ref (B { typ, flags, island_index, xf, sweep,
                               linear_velocity, angular_velocity, force, torque,
                               prev, next, fixture_list, fixture_count,
                               joint_list, contact_list, mass, inv_mass, i,
                               inv_i = _, linear_damping, angular_damping,
                               sleep_time, data, world }), inv_i) =
        r := B
        { world = world, typ = typ, flags = flags, island_index = island_index,
          xf = xf, sweep = sweep, linear_velocity = linear_velocity,
          angular_velocity = angular_velocity, force = force, torque = torque,
          prev = prev, next = next, fixture_list = fixture_list, data = data,
          fixture_count = fixture_count, joint_list = joint_list,
          contact_list = contact_list, mass = mass, inv_mass = inv_mass,
          i = i, inv_i = inv_i, linear_damping = linear_damping,
          angular_damping = angular_damping, sleep_time = sleep_time }

    fun set_linear_damping (r as ref (B { typ, flags, island_index, xf, sweep,
                               linear_velocity, angular_velocity, force, torque,
                               prev, next, fixture_list, fixture_count,
                               joint_list, contact_list, mass, inv_mass, i,
                               inv_i, linear_damping = _, angular_damping,
                               sleep_time, data, world }), linear_damping) =
        r := B
        { world = world, typ = typ, flags = flags, island_index = island_index,
          xf = xf, sweep = sweep, linear_velocity = linear_velocity,
          angular_velocity = angular_velocity, force = force, torque = torque,
          prev = prev, next = next, fixture_list = fixture_list, data = data,
          fixture_count = fixture_count, joint_list = joint_list,
          contact_list = contact_list, mass = mass, inv_mass = inv_mass,
          i = i, inv_i = inv_i, linear_damping = linear_damping,
          angular_damping = angular_damping, sleep_time = sleep_time }

    fun set_angular_damping (r as ref (B { typ, flags, island_index, xf, sweep,
                               linear_velocity, angular_velocity, force, torque,
                               prev, next, fixture_list, fixture_count,
                               joint_list, contact_list, mass, inv_mass, i,
                               inv_i, linear_damping, angular_damping = _,
                               sleep_time, data, world }), angular_damping) =
        r := B
        { world = world, typ = typ, flags = flags, island_index = island_index,
          xf = xf, sweep = sweep, linear_velocity = linear_velocity,
          angular_velocity = angular_velocity, force = force, torque = torque,
          prev = prev, next = next, fixture_list = fixture_list, data = data,
          fixture_count = fixture_count, joint_list = joint_list,
          contact_list = contact_list, mass = mass, inv_mass = inv_mass,
          i = i, inv_i = inv_i, linear_damping = linear_damping,
          angular_damping = angular_damping, sleep_time = sleep_time }

    fun set_sleep_time (r as ref (B { typ, flags, island_index, xf, sweep,
                               linear_velocity, angular_velocity, force, torque,
                               prev, next, fixture_list, fixture_count,
                               joint_list, contact_list, mass, inv_mass, i,
                               inv_i, linear_damping, angular_damping,
                               sleep_time = _, data, world }), sleep_time) =
        r := B
        { world = world, typ = typ, flags = flags, island_index = island_index,
          xf = xf, sweep = sweep, linear_velocity = linear_velocity,
          angular_velocity = angular_velocity, force = force, torque = torque,
          prev = prev, next = next, fixture_list = fixture_list, data = data,
          fixture_count = fixture_count, joint_list = joint_list,
          contact_list = contact_list, mass = mass, inv_mass = inv_mass,
          i = i, inv_i = inv_i, linear_damping = linear_damping,
          angular_damping = angular_damping, sleep_time = sleep_time }

    fun set_data (r as ref (B { typ, flags, island_index, xf, sweep,
                               linear_velocity, angular_velocity, force, torque,
                               prev, next, fixture_list, fixture_count,
                               joint_list, contact_list, mass, inv_mass, i,
                               inv_i, linear_damping, angular_damping,
                               sleep_time, data = _, world }), data) =
        r := B
        { world = world, typ = typ, flags = flags, island_index = island_index,
          xf = xf, sweep = sweep, linear_velocity = linear_velocity,
          angular_velocity = angular_velocity, force = force, torque = torque,
          prev = prev, next = next, fixture_list = fixture_list, data = data,
          fixture_count = fixture_count, joint_list = joint_list,
          contact_list = contact_list, mass = mass, inv_mass = inv_mass,
          i = i, inv_i = inv_i, linear_damping = linear_damping,
          angular_damping = angular_damping, sleep_time = sleep_time }

    fun get_flag (b, f) = Word8.andb (f, get_flags b) <> 0w0
    fun set_flag (b, f) = set_flags (b, Word8.orb(get_flags b, f))
    fun clear_flag (b, f) = set_flags (b, Word8.andb(get_flags b, Word8.notb f))

    fun new ({ typ : body_type,
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
               data : 'b,
               inertia_scale : real }, 
             world : ('b, 'f, 'j) world,
             next : ('b, 'f, 'j) body option) : ('b, 'f, 'j) body =
        let 
            val xf = transform_pos_angle (position, angle)
            val center = xf @*: vec2 (0.0, 0.0)
            val (mass, inv_mass) =
                case typ of
                    Dynamic => (1.0, 1.0)
                  | _ => (0.0, 0.0)
            val b = 
                ref (B { typ = typ,
                         flags = 0w0,
                         island_index = 0, (* ? *)
                         xf = xf,
                         sweep = sweep { local_center = vec2 (0.0, 0.0),
                                         a0 = angle, a = angle,
                                         c0 = center, c = center },
                         linear_velocity = linear_velocity,
                         angular_velocity = angular_velocity,
                         force = vec2 (0.0, 0.0),
                         torque = 0.0,
                         world = world,
                         prev = NONE,
                         next = next,
                         fixture_list = NONE,
                         fixture_count = 0,
                         joint_list = NONE,
                         contact_list = NONE,
                         mass = mass,
                         inv_mass = inv_mass,
                         i = 0.0,
                         inv_i = 0.0,
                         linear_damping = linear_damping,
                         angular_damping = angular_damping,
                         sleep_time = 0.0,
                         data = data })

        in
            (* PERF asserts *)
            if vec2is_valid position then () 
            else raise BDDDynamics "invalid position";
            if vec2is_valid linear_velocity then () 
            else raise BDDDynamics "invalid linear_velocity";
            if is_valid angle then () 
            else raise BDDDynamics "invalid angle";
            if is_valid angular_velocity then () 
            else raise BDDDynamics "invalid angular_velocity";
            if is_valid inertia_scale andalso inertia_scale >= 0.0 then ()
            else raise BDDDynamics "invalid inertia_scale";
            if is_valid angular_damping andalso angular_damping >= 0.0 then ()
            else raise BDDDynamics "invalid angular_damping";
            if is_valid linear_damping andalso linear_damping >= 0.0 then ()
            else raise BDDDynamics "invalid linear_damping";

            if bullet then set_flag (b, FLAG_BULLET) else ();
            if fixed_rotation then set_flag (b, FLAG_FIXED_ROTATION) else ();
            if allow_sleep then set_flag (b, FLAG_AUTO_SLEEP) else ();
            if awake then set_flag (b, FLAG_AWAKE) else ();
            if active then set_flag (b, FLAG_ACTIVE) else ();

            b
        end

    (* This is used to prevent connected bodies from colliding.
       It may lie, depending on the collideConnected flag.
       Port note: Used in ContactManager. *)
    fun should_collide (body : ('b, 'f, 'j) body, 
                        other : ('b, 'f, 'j) body) : bool =
        (* At least one body should be dynamic. *)
        if get_typ body <> Dynamic andalso get_typ other <> Dynamic
        then false
        else
            (* Does a joint prevent collision? *)
            let exception ReturnFalse
            in (oapp G.get_next
                  (fn jn =>
                   if G.get_other jn = other
                   then if J.get_collide_connected (G.get_joint jn) = false
                        then raise ReturnFalse
                        else ()
                   else ())
                  (get_joint_list body);
                true)
                handle ReturnFalse => false
            end

   (* Port note: used in TOISolver. *)
   fun synchronize_transform b : unit =
       let 
           val sweep : sweep = get_sweep b
           val r : mat22 = mat22angle (sweepa sweep)
           val pos : vec2 = sweepc sweep :-: (r +*: sweeplocalcenter sweep)
       in
           set_xf (b, transform (pos, r))
       end

    (* Port note: Used in world. *)
    fun advance (body : ('b, 'f, 'j) body, t : real) : unit =
        (* Advance to the new safe time. *)
        let
            val sweep = get_sweep body
        in
            sweep_advance (sweep, t);
            sweep_set_c (sweep, sweepc0 sweep);
            sweep_set_a (sweep, sweepa0 sweep);
            synchronize_transform body
        end

    (* Used in world. *)
    (* Port note: Passing broadphase instead of whole world value,
       to reduce dependencies. *)
    fun synchronize_fixtures 
        (b : ('b, 'f, 'j) body,
         broadphase : ('b, 'f, 'j) fixture BDDBroadPhase.broadphase) : unit =
      let
          val sweep : sweep = get_sweep b
          val r = mat22angle (sweepa0 sweep)
          val xf1 = transform (sweepc0 sweep :-: 
                               (r +*: sweeplocalcenter sweep),
                               r)
      in
          oapp F.get_next
          (fn f => F.synchronize (f, broadphase, xf1, get_xf b))
          (get_fixture_list b)
      end

    fun get_world_point (b, p) = get_xf b @*: p
    fun get_world_vector (b, v) = transformr (get_xf b) +*: v
    fun get_local_point (b, p) = mul_ttransformv (get_xf b, p)
    fun get_local_vector (b, v) = mul_t22mv (transformr (get_xf b), v)

    fun get_linear_velocity_from_world_point (b, world_point : vec2) =
        get_linear_velocity b :+: 
        cross2sv (get_angular_velocity b,
                  world_point :-: sweepc (get_sweep b))

    fun get_linear_velocity_from_local_point (b, local_point : vec2) =
        get_linear_velocity_from_world_point(b, get_world_point(b, 
                                                                local_point))

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

  end

  (* Internal, contact edges *)
  structure E =
  struct
      fun get_other (ref (E { other, ... })) = other
      fun get_contact (ref (E { contact, ... })) = contact
      fun get_prev (ref (E { prev, ... })) = prev
      fun get_next (ref (E { next, ... })) = next

      fun set_other (r as ref (E { other = _, contact, prev, next }), other) =
          r := E { other = other, contact = contact, prev = prev, next = next }
      fun set_contact (r as ref (E { other, contact = _, prev, next }), contact) =
          r := E { other = other, contact = contact, prev = prev, next = next }
      fun set_next (r as ref (E { other, contact, prev, next = _ }), next) =
          r := E { other = other, contact = contact, prev = prev, next = next }
      fun set_prev (r as ref (E { other, contact, prev = _, next }), prev) =
          r := E { other = other, contact = contact, prev = prev, next = next }

      fun new () = ref (E { contact = NONE, other = NONE,
                            prev = NONE, next = NONE })
  end

  (* Internal, contacts *)
  structure C =
  struct

    (* Used when crawling contact graph when forming islands. *)
    val FLAG_ISLAND = 0wx1 : Word32.word
    (* Set when the shapes are touching. *)
    val FLAG_TOUCHING  = 0wx2 : Word32.word
    (* This contact can be disabled (by user). *)
    val FLAG_ENABLED = 0wx4 : Word32.word
    (* This contact needs filtering because a fixture filter was changed. *)
    val FLAG_FILTER = 0wx8 : Word32.word
    (* This bullet contact had a TOI event. *)
    val FLAG_BULLET_HIT = 0wx10 : Word32.word

    fun get_flags (ref (C { flags, ... })) = flags
    fun get_prev (ref (C { prev, ... })) = prev
    fun get_next (ref (C { next, ... })) = next
    fun get_node_a (ref (C { node_a, ... })) = node_a
    fun get_node_b (ref (C { node_b, ... })) = node_b
    fun get_fixture_a (ref (C { fixture_a, ... })) = !! "cfa" fixture_a
    fun get_fixture_b (ref (C { fixture_b, ... })) = !! "cfb" fixture_b
    fun get_manifold (ref (C { manifold, ... })) = manifold
    fun get_toi_count (ref (C { toi_count, ... })) = toi_count

    fun set_flags (r as ref (C { flags = _, prev, next, node_a, node_b,
                                 fixture_a, fixture_b, manifold, toi_count }),
                   flags) =
        r := C { flags = flags, prev = prev, next = next, node_a = node_a,
                 node_b = node_b, fixture_a = fixture_a, fixture_b = fixture_b,
                 manifold = manifold, toi_count = toi_count }

    fun set_prev (r as ref (C { flags, prev = _, next, node_a, node_b,
                                fixture_a, fixture_b, manifold, toi_count }),
                   prev) =
        r := C { flags = flags, prev = prev, next = next, node_a = node_a,
                 node_b = node_b, fixture_a = fixture_a, fixture_b = fixture_b,
                 manifold = manifold, toi_count = toi_count }

    fun set_next (r as ref (C { flags, prev, next = _, node_a, node_b,
                                fixture_a, fixture_b, manifold, toi_count }),
                  next) =
        r := C { flags = flags, prev = prev, next = next, node_a = node_a,
                 node_b = node_b, fixture_a = fixture_a, fixture_b = fixture_b,
                 manifold = manifold, toi_count = toi_count }

    fun set_node_a (r as ref (C { flags, prev, next, node_a = _, node_b,
                                  fixture_a, fixture_b, manifold, toi_count }),
                    node_a) =
        r := C { flags = flags, prev = prev, next = next, node_a = node_a,
                 node_b = node_b, fixture_a = fixture_a, fixture_b = fixture_b,
                 manifold = manifold, toi_count = toi_count }

    fun set_node_b (r as ref (C { flags, prev, next, node_a, node_b = _,
                                  fixture_a, fixture_b, manifold, toi_count }),
                    node_b) =
        r := C { flags = flags, prev = prev, next = next, node_a = node_a,
                 node_b = node_b, fixture_a = fixture_a, fixture_b = fixture_b,
                 manifold = manifold, toi_count = toi_count }

    fun set_fixture_a (r as ref (C { flags, prev, next, node_a, node_b,
                                     fixture_a = _, fixture_b, manifold,
                                     toi_count }),
                       fixture_a) =
        r := C { flags = flags, prev = prev, next = next, node_a = node_a,
                 node_b = node_b, fixture_a = fixture_a, fixture_b = fixture_b,
                 manifold = manifold, toi_count = toi_count }

    fun set_fixture_b (r as ref (C { flags, prev, next, node_a, node_b,
                                     fixture_a, fixture_b = _, manifold,
                                     toi_count }),
                       fixture_b) =
        r := C { flags = flags, prev = prev, next = next, node_a = node_a,
                 node_b = node_b, fixture_a = fixture_a, fixture_b = fixture_b,
                 manifold = manifold, toi_count = toi_count }

    fun set_manifold (r as ref (C { flags, prev, next, node_a, node_b,
                                    fixture_a, fixture_b, manifold = _,
                                    toi_count }),
                      manifold) =
        r := C { flags = flags, prev = prev, next = next, node_a = node_a,
                 node_b = node_b, fixture_a = fixture_a, fixture_b = fixture_b,
                 manifold = manifold, toi_count = toi_count }

    fun set_toi_count (r as ref (C { flags, prev, next, node_a, node_b,
                                     fixture_a, fixture_b, manifold,
                                     toi_count = _ }),
                       toi_count) =
        r := C { flags = flags, prev = prev, next = next, node_a = node_a,
                 node_b = node_b, fixture_a = fixture_a, fixture_b = fixture_b,
                 manifold = manifold, toi_count = toi_count }

    fun get_flag (b, f) = Word32.andb (f, get_flags b) <> 0w0
    fun set_flag (b, f) = set_flags (b, Word32.orb(get_flags b, f))
    fun clear_flag (b, f) = set_flags (b, Word32.andb(get_flags b, Word32.notb f))

    fun get_fixtures c = (get_fixture_a c, get_fixture_b c)

    fun flag_for_filtering c = set_flag (c, FLAG_FILTER)
    fun get_enabled c = get_flag (c, FLAG_ENABLED)
    fun set_enabled (c, b) = if b
                             then set_flag (c, FLAG_ENABLED)
                             else clear_flag (c, FLAG_ENABLED)

    fun is_touching c = get_flag (c, FLAG_TOUCHING)

(*
        static b2Contact* Create(b2Fixture* fixtureA, b2Fixture* fixtureB, b2BlockAllocator* allocator);
        static void Destroy(b2Contact* contact, b2Shape::Type typeA, b2Shape::Type typeB, b2BlockAllocator* allocator);
        static void Destroy(b2Contact* contact, b2BlockAllocator* allocator);

        b2Contact() : m_fixtureA(NULL), m_fixtureB(NULL) {}
        b2Contact(b2Fixture* fixtureA, b2Fixture* fixtureB);
        virtual ~b2Contact() {}

        void Update(b2ContactListener* listener);
*)
    (* Port note: Box2D implements its own binary vtable here; pattern
       matching is much simpler. *)
    fun shapes (fix_a, fix_b) = (F.get_shape fix_a, F.get_shape fix_b)

(*
   truth is: these do nothing!
   just need to do this dispatch in the evaluate function,
   which passes off to b2CollideXandX.

    fun create (fix_a, fix_b) =
        case shapes (fix_a, fix_b) of
            (BDDShape.Circle ca, BDDShape.Circle cb) =>
                BDDCircleContact.create (ca, cb)
          | (BDDShape.Polygon pa, BDDShape.Circle cb) =>
                BDDPolygonAndCircleContact.create (pa, cb)
          | (BDDShape.Circle ca, BDDShape.Polygon pb) =>
                (* Port note: This is what the "primary" flag in the
                   "registers" table is about; just putting the
                   arguments in order. *)
                BDDPolygonAndCircleContact.create (pb, ca)
          | (BDDShape.Polygon pa, BDDShape.Polygon pb) =>
                BDDPolygonContact.create (pa, pb)

    fun destroy (fix_a, fix_b) =
        case shapes (fix_a, fix_b) of
            (BDDShape.Circle ca, BDDShape.Circle cb) =>
                BDDCircleContact.destroy (ca, cb)
          | (BDDShape.Polygon pa, BDDShape.Circle cb) =>
                BDDPolygonAndCircleContact.destroy (pa, cb)
          | (BDDShape.Circle ca, BDDShape.Polygon pb) =>
                BDDPolygonAndCircleContact.destroy (pb, ca)
          | (BDDShape.Polygon pa, BDDShape.Polygon pb) =>
                BDDPolygonContact.destroy (pa, pb)
*)

    fun evaluate (c : ('b, 'f, 'j) contact,
                  xfa : BDDMath.transform, xfb : BDDMath.transform) :
        BDDTypes.manifold =
        case (F.get_shape (get_fixture_a c), F.get_shape (get_fixture_b c)) of
            (BDDShape.Polygon pa, BDDShape.Polygon pb) =>
                BDDCollision.collide_polygons(pa, xfa, pb, xfb)
          | (BDDShape.Circle ca, BDDShape.Circle cb) =>
                BDDCollision.collide_circles(ca, xfa, cb, xfb)
          | (BDDShape.Polygon pa, BDDShape.Circle cb) =>
                BDDCollision.collide_polygon_and_circle (pa, xfa, cb, xfb)
          (* XXX: Not sure this is right. A consequence of the create
             dispatch above is that a Contact in Box2D is always in
             normalized order (polygon, circle), so the "A" fixture actually
             changes meaning. 

             Some other code actually mentions this fact (but does not
             appear to rely on it). We should probably reproduce the
             behavior here though. *)
          | (BDDShape.Circle ca, BDDShape.Polygon pb) =>
                BDDCollision.collide_polygon_and_circle (pb, xfb, ca, xfa)

    fun new (fixture_a, fixture_b) =
        ref (C { flags = FLAG_ENABLED,
                 fixture_a = SOME fixture_a,
                 fixture_b = SOME fixture_b,
                 manifold = { point_count = 0,
                              (* PERF uninitialized in Box2D. *)
                              typ = E_Circles,
                              points = Array.fromList nil,
                              local_normal = vec2 (0.0, 0.0),
                              local_point = vec2 (0.0, 0.0) },
                 prev = NONE,
                 next = NONE,
                 node_a = E.new (),
                 node_b = E.new (),
                 toi_count = 0 })
  end

  (* Internal, worlds *)
  structure W =
  struct

    val FLAG_NEW_FIXTURE = 0wx1 : Word32.word
    val FLAG_LOCKED = 0wx2 : Word32.word
    val FLAG_CLEAR_FORCES = 0wx4 : Word32.word

    (* Generated by metautil. XXX wrapping. *)
    fun get_flags (ref (W{ flags, ... })) = flags
    fun get_body_list (ref (W{ body_list, ... })) = body_list
    fun get_joint_list (ref (W{ joint_list, ... })) = joint_list
    fun get_body_count (ref (W{ body_count, ... })) = body_count
    fun get_joint_count (ref (W{ joint_count, ... })) = joint_count
    fun get_gravity (ref (W{ gravity, ... })) = gravity
    fun get_allow_sleep (ref (W{ allow_sleep, ... })) = allow_sleep
    fun get_ground_body (ref (W{ ground_body, ... })) = ground_body
    fun get_goodbye_joint_hook (ref (W{ goodbye_joint_hook, ... })) = goodbye_joint_hook
    fun get_goodbye_fixture_hook (ref (W{ goodbye_fixture_hook, ... })) = goodbye_fixture_hook
    fun get_inv_dt0 (ref (W{ inv_dt0, ... })) = inv_dt0
    fun get_warm_starting (ref (W{ warm_starting, ... })) = warm_starting
    fun get_continuous_physics (ref (W{ continuous_physics, ... })) = continuous_physics
    fun get_broad_phase (ref (W{ broad_phase, ... })) = broad_phase
    fun get_contact_list (ref (W{ contact_list, ... })) = contact_list
    fun get_contact_count (ref (W{ contact_count, ... })) = contact_count
    fun get_should_collide (ref (W{ should_collide, ... })) = should_collide
    fun get_begin_contact (ref (W{ begin_contact, ... })) = begin_contact
    fun get_end_contact (ref (W{ end_contact, ... })) = end_contact
    fun get_pre_solve (ref (W{ pre_solve, ... })) = pre_solve
    fun get_post_solve (ref (W{ post_solve, ... })) = post_solve

    fun set_flags (r as ref (W{ flags = _, body_list, joint_list, body_count, joint_count, gravity, allow_sleep, ground_body, goodbye_joint_hook, goodbye_fixture_hook, inv_dt0, warm_starting, continuous_physics, broad_phase, contact_list, contact_count, should_collide, begin_contact, end_contact, pre_solve, post_solve }), flags) = r := W { flags = flags, body_list = body_list, joint_list = joint_list, body_count = body_count, joint_count = joint_count, gravity = gravity, allow_sleep = allow_sleep, ground_body = ground_body, goodbye_joint_hook = goodbye_joint_hook, goodbye_fixture_hook = goodbye_fixture_hook, inv_dt0 = inv_dt0, warm_starting = warm_starting, continuous_physics = continuous_physics, broad_phase = broad_phase, contact_list = contact_list, contact_count = contact_count, should_collide = should_collide, begin_contact = begin_contact, end_contact = end_contact, pre_solve = pre_solve, post_solve = post_solve}
    fun set_body_list (r as ref (W{ flags, body_list = _, joint_list, body_count, joint_count, gravity, allow_sleep, ground_body, goodbye_joint_hook, goodbye_fixture_hook, inv_dt0, warm_starting, continuous_physics, broad_phase, contact_list, contact_count, should_collide, begin_contact, end_contact, pre_solve, post_solve }), body_list) = r := W { flags = flags, body_list = body_list, joint_list = joint_list, body_count = body_count, joint_count = joint_count, gravity = gravity, allow_sleep = allow_sleep, ground_body = ground_body, goodbye_joint_hook = goodbye_joint_hook, goodbye_fixture_hook = goodbye_fixture_hook, inv_dt0 = inv_dt0, warm_starting = warm_starting, continuous_physics = continuous_physics, broad_phase = broad_phase, contact_list = contact_list, contact_count = contact_count, should_collide = should_collide, begin_contact = begin_contact, end_contact = end_contact, pre_solve = pre_solve, post_solve = post_solve}
    fun set_joint_list (r as ref (W{ flags, body_list, joint_list = _, body_count, joint_count, gravity, allow_sleep, ground_body, goodbye_joint_hook, goodbye_fixture_hook, inv_dt0, warm_starting, continuous_physics, broad_phase, contact_list, contact_count, should_collide, begin_contact, end_contact, pre_solve, post_solve }), joint_list) = r := W { flags = flags, body_list = body_list, joint_list = joint_list, body_count = body_count, joint_count = joint_count, gravity = gravity, allow_sleep = allow_sleep, ground_body = ground_body, goodbye_joint_hook = goodbye_joint_hook, goodbye_fixture_hook = goodbye_fixture_hook, inv_dt0 = inv_dt0, warm_starting = warm_starting, continuous_physics = continuous_physics, broad_phase = broad_phase, contact_list = contact_list, contact_count = contact_count, should_collide = should_collide, begin_contact = begin_contact, end_contact = end_contact, pre_solve = pre_solve, post_solve = post_solve}
    fun set_body_count (r as ref (W{ flags, body_list, joint_list, body_count = _, joint_count, gravity, allow_sleep, ground_body, goodbye_joint_hook, goodbye_fixture_hook, inv_dt0, warm_starting, continuous_physics, broad_phase, contact_list, contact_count, should_collide, begin_contact, end_contact, pre_solve, post_solve }), body_count) = r := W { flags = flags, body_list = body_list, joint_list = joint_list, body_count = body_count, joint_count = joint_count, gravity = gravity, allow_sleep = allow_sleep, ground_body = ground_body, goodbye_joint_hook = goodbye_joint_hook, goodbye_fixture_hook = goodbye_fixture_hook, inv_dt0 = inv_dt0, warm_starting = warm_starting, continuous_physics = continuous_physics, broad_phase = broad_phase, contact_list = contact_list, contact_count = contact_count, should_collide = should_collide, begin_contact = begin_contact, end_contact = end_contact, pre_solve = pre_solve, post_solve = post_solve}
    fun set_joint_count (r as ref (W{ flags, body_list, joint_list, body_count, joint_count = _, gravity, allow_sleep, ground_body, goodbye_joint_hook, goodbye_fixture_hook, inv_dt0, warm_starting, continuous_physics, broad_phase, contact_list, contact_count, should_collide, begin_contact, end_contact, pre_solve, post_solve }), joint_count) = r := W { flags = flags, body_list = body_list, joint_list = joint_list, body_count = body_count, joint_count = joint_count, gravity = gravity, allow_sleep = allow_sleep, ground_body = ground_body, goodbye_joint_hook = goodbye_joint_hook, goodbye_fixture_hook = goodbye_fixture_hook, inv_dt0 = inv_dt0, warm_starting = warm_starting, continuous_physics = continuous_physics, broad_phase = broad_phase, contact_list = contact_list, contact_count = contact_count, should_collide = should_collide, begin_contact = begin_contact, end_contact = end_contact, pre_solve = pre_solve, post_solve = post_solve}
    fun set_gravity (r as ref (W{ flags, body_list, joint_list, body_count, joint_count, gravity = _, allow_sleep, ground_body, goodbye_joint_hook, goodbye_fixture_hook, inv_dt0, warm_starting, continuous_physics, broad_phase, contact_list, contact_count, should_collide, begin_contact, end_contact, pre_solve, post_solve }), gravity) = r := W { flags = flags, body_list = body_list, joint_list = joint_list, body_count = body_count, joint_count = joint_count, gravity = gravity, allow_sleep = allow_sleep, ground_body = ground_body, goodbye_joint_hook = goodbye_joint_hook, goodbye_fixture_hook = goodbye_fixture_hook, inv_dt0 = inv_dt0, warm_starting = warm_starting, continuous_physics = continuous_physics, broad_phase = broad_phase, contact_list = contact_list, contact_count = contact_count, should_collide = should_collide, begin_contact = begin_contact, end_contact = end_contact, pre_solve = pre_solve, post_solve = post_solve}
    fun set_allow_sleep (r as ref (W{ flags, body_list, joint_list, body_count, joint_count, gravity, allow_sleep = _, ground_body, goodbye_joint_hook, goodbye_fixture_hook, inv_dt0, warm_starting, continuous_physics, broad_phase, contact_list, contact_count, should_collide, begin_contact, end_contact, pre_solve, post_solve }), allow_sleep) = r := W { flags = flags, body_list = body_list, joint_list = joint_list, body_count = body_count, joint_count = joint_count, gravity = gravity, allow_sleep = allow_sleep, ground_body = ground_body, goodbye_joint_hook = goodbye_joint_hook, goodbye_fixture_hook = goodbye_fixture_hook, inv_dt0 = inv_dt0, warm_starting = warm_starting, continuous_physics = continuous_physics, broad_phase = broad_phase, contact_list = contact_list, contact_count = contact_count, should_collide = should_collide, begin_contact = begin_contact, end_contact = end_contact, pre_solve = pre_solve, post_solve = post_solve}
    fun set_ground_body (r as ref (W{ flags, body_list, joint_list, body_count, joint_count, gravity, allow_sleep, ground_body = _, goodbye_joint_hook, goodbye_fixture_hook, inv_dt0, warm_starting, continuous_physics, broad_phase, contact_list, contact_count, should_collide, begin_contact, end_contact, pre_solve, post_solve }), ground_body) = r := W { flags = flags, body_list = body_list, joint_list = joint_list, body_count = body_count, joint_count = joint_count, gravity = gravity, allow_sleep = allow_sleep, ground_body = ground_body, goodbye_joint_hook = goodbye_joint_hook, goodbye_fixture_hook = goodbye_fixture_hook, inv_dt0 = inv_dt0, warm_starting = warm_starting, continuous_physics = continuous_physics, broad_phase = broad_phase, contact_list = contact_list, contact_count = contact_count, should_collide = should_collide, begin_contact = begin_contact, end_contact = end_contact, pre_solve = pre_solve, post_solve = post_solve}
    fun set_goodbye_joint_hook (r as ref (W{ flags, body_list, joint_list, body_count, joint_count, gravity, allow_sleep, ground_body, goodbye_joint_hook = _, goodbye_fixture_hook, inv_dt0, warm_starting, continuous_physics, broad_phase, contact_list, contact_count, should_collide, begin_contact, end_contact, pre_solve, post_solve }), goodbye_joint_hook) = r := W { flags = flags, body_list = body_list, joint_list = joint_list, body_count = body_count, joint_count = joint_count, gravity = gravity, allow_sleep = allow_sleep, ground_body = ground_body, goodbye_joint_hook = goodbye_joint_hook, goodbye_fixture_hook = goodbye_fixture_hook, inv_dt0 = inv_dt0, warm_starting = warm_starting, continuous_physics = continuous_physics, broad_phase = broad_phase, contact_list = contact_list, contact_count = contact_count, should_collide = should_collide, begin_contact = begin_contact, end_contact = end_contact, pre_solve = pre_solve, post_solve = post_solve}
    fun set_goodbye_fixture_hook (r as ref (W{ flags, body_list, joint_list, body_count, joint_count, gravity, allow_sleep, ground_body, goodbye_joint_hook, goodbye_fixture_hook = _, inv_dt0, warm_starting, continuous_physics, broad_phase, contact_list, contact_count, should_collide, begin_contact, end_contact, pre_solve, post_solve }), goodbye_fixture_hook) = r := W { flags = flags, body_list = body_list, joint_list = joint_list, body_count = body_count, joint_count = joint_count, gravity = gravity, allow_sleep = allow_sleep, ground_body = ground_body, goodbye_joint_hook = goodbye_joint_hook, goodbye_fixture_hook = goodbye_fixture_hook, inv_dt0 = inv_dt0, warm_starting = warm_starting, continuous_physics = continuous_physics, broad_phase = broad_phase, contact_list = contact_list, contact_count = contact_count, should_collide = should_collide, begin_contact = begin_contact, end_contact = end_contact, pre_solve = pre_solve, post_solve = post_solve}
    fun set_inv_dt0 (r as ref (W{ flags, body_list, joint_list, body_count, joint_count, gravity, allow_sleep, ground_body, goodbye_joint_hook, goodbye_fixture_hook, inv_dt0 = _, warm_starting, continuous_physics, broad_phase, contact_list, contact_count, should_collide, begin_contact, end_contact, pre_solve, post_solve }), inv_dt0) = r := W { flags = flags, body_list = body_list, joint_list = joint_list, body_count = body_count, joint_count = joint_count, gravity = gravity, allow_sleep = allow_sleep, ground_body = ground_body, goodbye_joint_hook = goodbye_joint_hook, goodbye_fixture_hook = goodbye_fixture_hook, inv_dt0 = inv_dt0, warm_starting = warm_starting, continuous_physics = continuous_physics, broad_phase = broad_phase, contact_list = contact_list, contact_count = contact_count, should_collide = should_collide, begin_contact = begin_contact, end_contact = end_contact, pre_solve = pre_solve, post_solve = post_solve}
    fun set_warm_starting (r as ref (W{ flags, body_list, joint_list, body_count, joint_count, gravity, allow_sleep, ground_body, goodbye_joint_hook, goodbye_fixture_hook, inv_dt0, warm_starting = _, continuous_physics, broad_phase, contact_list, contact_count, should_collide, begin_contact, end_contact, pre_solve, post_solve }), warm_starting) = r := W { flags = flags, body_list = body_list, joint_list = joint_list, body_count = body_count, joint_count = joint_count, gravity = gravity, allow_sleep = allow_sleep, ground_body = ground_body, goodbye_joint_hook = goodbye_joint_hook, goodbye_fixture_hook = goodbye_fixture_hook, inv_dt0 = inv_dt0, warm_starting = warm_starting, continuous_physics = continuous_physics, broad_phase = broad_phase, contact_list = contact_list, contact_count = contact_count, should_collide = should_collide, begin_contact = begin_contact, end_contact = end_contact, pre_solve = pre_solve, post_solve = post_solve}
    fun set_continuous_physics (r as ref (W{ flags, body_list, joint_list, body_count, joint_count, gravity, allow_sleep, ground_body, goodbye_joint_hook, goodbye_fixture_hook, inv_dt0, warm_starting, continuous_physics = _, broad_phase, contact_list, contact_count, should_collide, begin_contact, end_contact, pre_solve, post_solve }), continuous_physics) = r := W { flags = flags, body_list = body_list, joint_list = joint_list, body_count = body_count, joint_count = joint_count, gravity = gravity, allow_sleep = allow_sleep, ground_body = ground_body, goodbye_joint_hook = goodbye_joint_hook, goodbye_fixture_hook = goodbye_fixture_hook, inv_dt0 = inv_dt0, warm_starting = warm_starting, continuous_physics = continuous_physics, broad_phase = broad_phase, contact_list = contact_list, contact_count = contact_count, should_collide = should_collide, begin_contact = begin_contact, end_contact = end_contact, pre_solve = pre_solve, post_solve = post_solve}
    fun set_broad_phase (r as ref (W{ flags, body_list, joint_list, body_count, joint_count, gravity, allow_sleep, ground_body, goodbye_joint_hook, goodbye_fixture_hook, inv_dt0, warm_starting, continuous_physics, broad_phase = _, contact_list, contact_count, should_collide, begin_contact, end_contact, pre_solve, post_solve }), broad_phase) = r := W { flags = flags, body_list = body_list, joint_list = joint_list, body_count = body_count, joint_count = joint_count, gravity = gravity, allow_sleep = allow_sleep, ground_body = ground_body, goodbye_joint_hook = goodbye_joint_hook, goodbye_fixture_hook = goodbye_fixture_hook, inv_dt0 = inv_dt0, warm_starting = warm_starting, continuous_physics = continuous_physics, broad_phase = broad_phase, contact_list = contact_list, contact_count = contact_count, should_collide = should_collide, begin_contact = begin_contact, end_contact = end_contact, pre_solve = pre_solve, post_solve = post_solve}
    fun set_contact_list (r as ref (W{ flags, body_list, joint_list, body_count, joint_count, gravity, allow_sleep, ground_body, goodbye_joint_hook, goodbye_fixture_hook, inv_dt0, warm_starting, continuous_physics, broad_phase, contact_list = _, contact_count, should_collide, begin_contact, end_contact, pre_solve, post_solve }), contact_list) = r := W { flags = flags, body_list = body_list, joint_list = joint_list, body_count = body_count, joint_count = joint_count, gravity = gravity, allow_sleep = allow_sleep, ground_body = ground_body, goodbye_joint_hook = goodbye_joint_hook, goodbye_fixture_hook = goodbye_fixture_hook, inv_dt0 = inv_dt0, warm_starting = warm_starting, continuous_physics = continuous_physics, broad_phase = broad_phase, contact_list = contact_list, contact_count = contact_count, should_collide = should_collide, begin_contact = begin_contact, end_contact = end_contact, pre_solve = pre_solve, post_solve = post_solve}
    fun set_contact_count (r as ref (W{ flags, body_list, joint_list, body_count, joint_count, gravity, allow_sleep, ground_body, goodbye_joint_hook, goodbye_fixture_hook, inv_dt0, warm_starting, continuous_physics, broad_phase, contact_list, contact_count = _, should_collide, begin_contact, end_contact, pre_solve, post_solve }), contact_count) = r := W { flags = flags, body_list = body_list, joint_list = joint_list, body_count = body_count, joint_count = joint_count, gravity = gravity, allow_sleep = allow_sleep, ground_body = ground_body, goodbye_joint_hook = goodbye_joint_hook, goodbye_fixture_hook = goodbye_fixture_hook, inv_dt0 = inv_dt0, warm_starting = warm_starting, continuous_physics = continuous_physics, broad_phase = broad_phase, contact_list = contact_list, contact_count = contact_count, should_collide = should_collide, begin_contact = begin_contact, end_contact = end_contact, pre_solve = pre_solve, post_solve = post_solve}
    fun set_should_collide (r as ref (W{ flags, body_list, joint_list, body_count, joint_count, gravity, allow_sleep, ground_body, goodbye_joint_hook, goodbye_fixture_hook, inv_dt0, warm_starting, continuous_physics, broad_phase, contact_list, contact_count, should_collide = _, begin_contact, end_contact, pre_solve, post_solve }), should_collide) = r := W { flags = flags, body_list = body_list, joint_list = joint_list, body_count = body_count, joint_count = joint_count, gravity = gravity, allow_sleep = allow_sleep, ground_body = ground_body, goodbye_joint_hook = goodbye_joint_hook, goodbye_fixture_hook = goodbye_fixture_hook, inv_dt0 = inv_dt0, warm_starting = warm_starting, continuous_physics = continuous_physics, broad_phase = broad_phase, contact_list = contact_list, contact_count = contact_count, should_collide = should_collide, begin_contact = begin_contact, end_contact = end_contact, pre_solve = pre_solve, post_solve = post_solve}
    fun set_begin_contact (r as ref (W{ flags, body_list, joint_list, body_count, joint_count, gravity, allow_sleep, ground_body, goodbye_joint_hook, goodbye_fixture_hook, inv_dt0, warm_starting, continuous_physics, broad_phase, contact_list, contact_count, should_collide, begin_contact = _, end_contact, pre_solve, post_solve }), begin_contact) = r := W { flags = flags, body_list = body_list, joint_list = joint_list, body_count = body_count, joint_count = joint_count, gravity = gravity, allow_sleep = allow_sleep, ground_body = ground_body, goodbye_joint_hook = goodbye_joint_hook, goodbye_fixture_hook = goodbye_fixture_hook, inv_dt0 = inv_dt0, warm_starting = warm_starting, continuous_physics = continuous_physics, broad_phase = broad_phase, contact_list = contact_list, contact_count = contact_count, should_collide = should_collide, begin_contact = begin_contact, end_contact = end_contact, pre_solve = pre_solve, post_solve = post_solve}
    fun set_end_contact (r as ref (W{ flags, body_list, joint_list, body_count, joint_count, gravity, allow_sleep, ground_body, goodbye_joint_hook, goodbye_fixture_hook, inv_dt0, warm_starting, continuous_physics, broad_phase, contact_list, contact_count, should_collide, begin_contact, end_contact = _, pre_solve, post_solve }), end_contact) = r := W { flags = flags, body_list = body_list, joint_list = joint_list, body_count = body_count, joint_count = joint_count, gravity = gravity, allow_sleep = allow_sleep, ground_body = ground_body, goodbye_joint_hook = goodbye_joint_hook, goodbye_fixture_hook = goodbye_fixture_hook, inv_dt0 = inv_dt0, warm_starting = warm_starting, continuous_physics = continuous_physics, broad_phase = broad_phase, contact_list = contact_list, contact_count = contact_count, should_collide = should_collide, begin_contact = begin_contact, end_contact = end_contact, pre_solve = pre_solve, post_solve = post_solve}
    fun set_pre_solve (r as ref (W{ flags, body_list, joint_list, body_count, joint_count, gravity, allow_sleep, ground_body, goodbye_joint_hook, goodbye_fixture_hook, inv_dt0, warm_starting, continuous_physics, broad_phase, contact_list, contact_count, should_collide, begin_contact, end_contact, pre_solve = _, post_solve }), pre_solve) = r := W { flags = flags, body_list = body_list, joint_list = joint_list, body_count = body_count, joint_count = joint_count, gravity = gravity, allow_sleep = allow_sleep, ground_body = ground_body, goodbye_joint_hook = goodbye_joint_hook, goodbye_fixture_hook = goodbye_fixture_hook, inv_dt0 = inv_dt0, warm_starting = warm_starting, continuous_physics = continuous_physics, broad_phase = broad_phase, contact_list = contact_list, contact_count = contact_count, should_collide = should_collide, begin_contact = begin_contact, end_contact = end_contact, pre_solve = pre_solve, post_solve = post_solve}
    fun set_post_solve (r as ref (W{ flags, body_list, joint_list, body_count, joint_count, gravity, allow_sleep, ground_body, goodbye_joint_hook, goodbye_fixture_hook, inv_dt0, warm_starting, continuous_physics, broad_phase, contact_list, contact_count, should_collide, begin_contact, end_contact, pre_solve, post_solve = _ }), post_solve) = r := W { flags = flags, body_list = body_list, joint_list = joint_list, body_count = body_count, joint_count = joint_count, gravity = gravity, allow_sleep = allow_sleep, ground_body = ground_body, goodbye_joint_hook = goodbye_joint_hook, goodbye_fixture_hook = goodbye_fixture_hook, inv_dt0 = inv_dt0, warm_starting = warm_starting, continuous_physics = continuous_physics, broad_phase = broad_phase, contact_list = contact_list, contact_count = contact_count, should_collide = should_collide, begin_contact = begin_contact, end_contact = end_contact, pre_solve = pre_solve, post_solve = post_solve}

    fun get_flag (w, f) = Word32.andb (f, get_flags w) <> 0w0
    fun set_flag (w, f) = set_flags (w, Word32.orb(get_flags w, f))
    fun clear_flag (w, f) = set_flags (w, Word32.andb(get_flags w, Word32.notb f))

    fun is_locked w = get_flag (w, FLAG_LOCKED)
    fun set_auto_clear_forces (w, b) = if b then set_flag (w, FLAG_CLEAR_FORCES)
                                       else clear_flag (w, FLAG_CLEAR_FORCES)
    fun get_auto_clear_forces w = get_flag (w, FLAG_CLEAR_FORCES)

    structure CM =
    struct
      (* Regrettably, used by body.sml when setting a body inactive. 
         XXX figure out the right place for this. *)
         
      fun destroy (world : ('b, 'f, 'j) world, 
                   c : ('b, 'f, 'j) contact) : unit =
        let
          val fixture_a = C.get_fixture_a c
          val fixture_b = C.get_fixture_b c
          val body_a = F.get_body fixture_a
          val body_b = F.get_body fixture_b

          val () = if C.is_touching c
                   then get_end_contact world c
                   else ()

          (* remove from world DLL. *)
          val prev = C.get_prev c
          val next = C.get_next c
          val () = case prev of
              NONE => ()
            | SOME prev => C.set_next (prev, next)
          val () = case next of
              NONE => ()
            | SOME next => C.set_prev (next, prev)

          val () = if SOME c = get_contact_list world
                   then set_contact_list (world, next)
                   else ()

          (* Remove from body A *)
          val nodea = C.get_node_a c
          val prev = E.get_prev nodea
          val next = E.get_next nodea
          val () = case prev of
              NONE => ()
            | SOME prev => E.set_next (prev, next)
          val () = case next of
              NONE => ()
            | SOME next => E.set_prev (next, prev)
          (* Port note: The original code uses pointers to the interior of the
             contact (&c->m_nodeA == ?); I've made these their own cells. I 
             think this is right, but if something is going wrong here, this 
             is a good thing to take a look at. *)
          val () = if SOME nodea = B.get_contact_list body_a
                   then B.set_contact_list (body_a, next)
                   else ()

          (* Remove from body B *)
          val nodeb = C.get_node_b c
          val prev = E.get_prev nodeb
          val next = E.get_next nodeb
          val () = case prev of
              NONE => ()
            | SOME prev => E.set_next (prev, next)
          val () = case next of
              NONE => ()
            | SOME next => E.set_prev (next, prev)
          val () = if SOME nodeb = B.get_contact_list body_b
                   then B.set_contact_list (body_b, next)
                   else ()
        in
            set_contact_count (world, get_contact_count world - 1)
        end

      (* Callback used in find_new_contacts. *)
      exception Return
      fun add_pair (world : ('b, 'f, 'j) world) 
          (fixture_a : ('b, 'f, 'j) fixture, 
           fixture_b : ('b, 'f, 'j) fixture) : unit =
        let
          val body_a = F.get_body fixture_a
          val body_b = F.get_body fixture_b
          (* No self-contact *)
          val () = if body_a = body_b then raise Return
                   else ()

          (* Just raises Return if a contact already exists. *)
          fun one_edge e =
              if SOME body_a = E.get_other e
              then
                  let val fa = C.get_fixture_a (!! "fa" (E.get_contact e))
                      val fb = C.get_fixture_b (!! "fb" (E.get_contact e))
                  in
                      if (fa = fixture_a andalso fb = fixture_b) orelse
                         (fa = fixture_b andalso fb = fixture_a)
                      (* Contact already exists. *)
                      then raise Return
                      else ()
                  end
              else ()
          val () = oapp E.get_next one_edge (B.get_contact_list body_b)

          (* Does a joint override collision? Is at least one body dynamic? *)
          val () = if B.should_collide (body_b, body_a)
                   then ()
                   else raise Return

          (* Check user filtering. *)
          val () = if get_should_collide world (fixture_a, fixture_b)
                   then ()
                   else raise Return

          (* Call the factory. *)
          val c = C.new (fixture_a, fixture_b)

          (* Contact creation may swap fixtures.
             XXX (it doesn't, currently.)
             *)
          val fixture_a = C.get_fixture_a c
          val fixture_b = C.get_fixture_b c
          val body_a = F.get_body fixture_a
          val body_b = F.get_body fixture_b

          (* Add to world DLL. *)
          val () = C.set_next (c, get_contact_list world)
          val () = case get_contact_list world of
              NONE => ()
            | SOME prev => C.set_prev (prev, SOME c)
          val () = set_contact_list (world, SOME c)

          (* Connect to island graph. *)
          val node_a = C.get_node_a c
          val () = E.set_contact (node_a, SOME c)
          val () = E.set_other (node_a, SOME body_b)

          val () = E.set_next (node_a, B.get_contact_list body_a)
          val () = case B.get_contact_list body_a of
              NONE => ()
            | SOME prev => E.set_prev (prev, SOME node_a)
          val () = B.set_contact_list (body_a, SOME node_a)

          val node_b = C.get_node_b c
          val () = E.set_contact (node_b, SOME c)
          val () = E.set_other (node_b, SOME body_a)

          val () = E.set_next (node_b, B.get_contact_list body_b)
          val () = case B.get_contact_list body_b of
              NONE => ()
            | SOME prev => E.set_prev (prev, SOME node_b)
          val () = B.set_contact_list (body_b, SOME node_b)
        in
          set_contact_count (world, get_contact_count world + 1)
        end handle Return => ()

      fun find_new_contacts world =
        BDDBroadPhase.update_pairs (get_broad_phase world, add_pair world)

    end

  end

end

functor BDDDynamicsTypes(Arg :
                         sig
                             type body_data
                             type fixture_data
                             type joint_data
                         end) =
struct

  type body = (Arg.body_data, Arg.fixture_data, Arg.joint_data)
      BDDDynamics.bodycell ref
  type fixture = (Arg.body_data, Arg.fixture_data, Arg.joint_data)
      BDDDynamics.fixturecell ref
  type contact = (Arg.body_data, Arg.fixture_data, Arg.joint_data)
      BDDDynamics.contactcell ref
  type contactedge = (Arg.body_data, Arg.fixture_data, Arg.joint_data)
      BDDDynamics.contactedgecell ref
  type joint = (Arg.body_data, Arg.fixture_data, Arg.joint_data)
      BDDDynamics.jointcell ref
  type jointedge = (Arg.body_data, Arg.fixture_data, Arg.joint_data)
      BDDDynamics.jointedgecell ref
  type world = (Arg.body_data, Arg.fixture_data, Arg.joint_data)
      BDDDynamics.worldcell ref
end
