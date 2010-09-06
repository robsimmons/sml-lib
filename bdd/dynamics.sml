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

  exception BDDWorld of string

  (* 16 category bits, then 16 mask bits; group index *)
  type filter = Word32.word * int
  val default_filter = (0wx0001FFFF, 1)
  datatype body_type =
      Static
    | Kinematic
    | Dynamic

      (* XXX *)
  type world = unit
  type joint = unit

  (* Bodies and fixtures are refs to fuctional records
     ("cells" in local terminology). *)
  datatype ('b, 'f) bodycell =
      B of { typ : body_type,
             flags : Word16.word,
             island_index : int,
             (* Body origin transform *)
             xf : transform,
             (* Sept motion for CCD *)
             sweep : sweep,
             linear_velocity : vec2,
             angular_velocity : real,
             force : vec2,
             torque : real,
             world : world,

             (* XXX option? *)
             prev : ('b, 'f) bodycell ref option,
             next : ('b, 'f) bodycell ref option,

             fixture_list : ('b, 'f) fixturecell ref option,
             fixture_count : int,

             (* XXX no *)
             joint_list : unit,
             contact_list : unit,

             mass : real,
             inv_mass : real,
             
             (* Rotational inertia about the center of mass. *)
             i : real,
             inv_i : real,

             linear_damping : real,
             angular_damping : real,

             sleep_time : real,
             data : 'b }

  and ('b, 'f) fixturecell =
      F of { aabb : aabb,
             density : real,
             next : ('b, 'f) fixturecell ref option,
             (* Should always be SOME unless deleted. *)
             body : ('b, 'f) bodycell ref option,
             shape : BDDShape.shape,
             friction : real,
             restitution : real,
             (* Broad phase proxy, where the user data is
                this fixture. *)
             proxy : ('b, 'f) fixturecell ref BDDBroadPhase.proxy,
             filter : filter,
             sensor : bool,
             data : 'f }

  and ('b, 'f) contactcell =
      C of { flags : Word32.word,
	     (* All contacts in the world. *)
	     prev : ('b, 'f) contactcell ref option,
	     next : ('b, 'f) contactcell ref option,
	     (* nodes for connecting bodies *)
	     node_a : ('b, 'f) contactedge,
	     node_b : ('b, 'f) contactedge,
	     fixture_a : ('b, 'f) fixturecell ref,
	     fixture_b : ('b, 'f) fixturecell ref,
	     manifold : BDDTypes.manifold,
	     toi_count : int }

  (* A contact edge is used to connect bodies and contacts together
     in a contact graph where each body is a node and each contact
     is an edge. A contact edge belongs to a doubly linked list
     maintained in each attached body. Each contact has two contact
     nodes, one for each attached body.

     *)
  and ('b, 'f) contactedge =
      E of { (* provides quick access to the other body attached. *)
	     other : ('b, 'f) bodycell ref,
	     contact : ('b, 'f) contactcell ref,
	     (* the previous and next contact edge in the body's contact list *)
	     prev : ('b, 'f) contactedge ref option,
	     next : ('b, 'f) contactedge ref option }


  type ('b, 'f) fixture = ('b, 'f) fixturecell ref
  type ('b, 'f) body = ('b, 'f) bodycell ref

  (* Internal, fixtures *)      
  structure F =
  struct

    fun get_aabb (ref (F{ aabb, ... })) = aabb
    fun get_density (ref (F{ density, ... })) = density
    fun get_next (ref (F{ next, ... })) = next
    fun get_body (ref (F{ body, ... })) = body
    fun get_shape (ref (F{ shape, ... })) = shape
    fun get_friction (ref (F{ friction, ... })) = friction
    fun get_restitution (ref (F{ restitution, ... })) = restitution
    fun get_filter (ref (F{ filter, ... })) = filter
    fun get_sensor (ref (F{ sensor, ... })) = sensor
    fun get_data (ref (F{ data, ... })) = data

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

    (* XXX exposed function needs to do SetFilterData below. *)
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
  end

  (* Internal, bodies *)
  structure B =
  struct

    val FLAG_ISLAND = 0wx1 : Word16.word
    val FLAG_AWAKE  = 0wx2 : Word16.word
    val FLAG_AUTO_SLEEP = 0wx4 : Word16.word
    val FLAG_BULLET = 0wx8 : Word16.word
    val FLAG_FIXED_ROTATION = 0wx10 : Word16.word
    val FLAG_ACTIVE = 0wx20 : Word16.word
    val FLAG_TOI = 0wx40 : Word16.word

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

    fun get_flag (b, f) = Word16.andb (f, get_flags b) <> 0w0
    fun set_flag (b, f) = set_flags (b, Word16.orb(get_flags b, f))
    fun clear_flag (b, f) = set_flags (b, Word16.andb(get_flags b, Word16.notb f))

  end

  structure C =
  struct

    fun get_flags (ref (C { flags, ... })) = flags
    fun get_prev (ref (C { prev, ... })) = prev
    fun get_next (ref (C { next, ... })) = next
    fun get_node_a (ref (C { node_a, ... })) = node_a
    fun get_node_b (ref (C { node_b, ... })) = node_b
    fun get_fixture_a (ref (C { fixture_a, ... })) = fixture_a
    fun get_fixture_b (ref (C { fixture_b, ... })) = fixture_b
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

  end

 (*
void b2Fixture::CreateProxy(b2BroadPhase* broadPhase, const b2Transform& xf)
{
        b2Assert(m_proxyId == b2BroadPhase::e_nullProxy);

        // Create proxy in the broad-phase.
        m_shape->ComputeAABB(&m_aabb, xf);
        m_proxyId = broadPhase->CreateProxy(m_aabb, this);
}

void b2Fixture::DestroyProxy(b2BroadPhase* broadPhase)
{
        if (m_proxyId == b2BroadPhase::e_nullProxy)
        {
                return;
        }

        // Destroy proxy in the broad-phase.
        broadPhase->DestroyProxy(m_proxyId);
        m_proxyId = b2BroadPhase::e_nullProxy;
}

void b2Fixture::Synchronize(b2BroadPhase* broadPhase, const b2Transform& transform1, const b2Transform& transform2)
{
        if (m_proxyId == b2BroadPhase::e_nullProxy)
        {       
                return;
        }

        // Compute an AABB that covers the swept shape (may miss some rotation effect).
        b2AABB aabb1, aabb2;
        m_shape->ComputeAABB(&aabb1, transform1);
        m_shape->ComputeAABB(&aabb2, transform2);
        
        m_aabb.Combine(aabb1, aabb2);

        b2Vec2 displacement = transform2.position - transform1.position;

        broadPhase->MoveProxy(m_proxyId, m_aabb, displacement);
}

 *)

end
