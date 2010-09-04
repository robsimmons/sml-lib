(* Copyright 2010 Tom Murphy VII and Erin Catto. See COPYING for details. *)

(* Corresponding to dynamics/b2body.cpp, dynamics/b2fixture.cpp, and the
   implementation portions of their headers. *)
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
  type filter = Word32.word * int
  val default_filter = (0wx0001FFFF, 1)
  datatype body_type =
      Static
    | Kinematic
    | Dynamic

      (* XXX *)
  type world = unit
  type contact = unit
  type joint = unit

  (* Bodies and fixtures are refs to fuctional records
     ("cells" in local terminology). *)
  datatype bodycell =
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
             prev : bodycell ref option,
             next : bodycell ref option,

             fixture_list : fixturecell ref option,
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
             data : body_data }

  and fixturecell =
      F of { aabb : aabb,
             density : real,
             next : fixturecell ref option,
             (* Should always be SOME unless deleted. *)
             body : bodycell ref option,
             shape : BDDShape.shape,
             friction : real,
             restitution : real,
             (* Broad phase proxy, where the user data is
                this fixture. *)
             proxy : fixturecell ref BDDBroadPhase.proxy,
             filter : filter,
             sensor : bool,
             data : fixture_data }

  type fixture = fixturecell ref
  type body = bodycell ref

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
                                restitution, proxy, filter, sensor, data }), aabb) =
        r := F { aabb = aabb, density = density, next = next, body = body,
                 shape = shape, friction = friction, restitution = restitution,
                 proxy = proxy, filter = filter, sensor = sensor, data = data }

    fun set_density (r as ref (F { aabb, density = _, next, body, shape, friction,
                                   restitution, proxy, filter, sensor, data }), density) =
        (* XXX Box2D has check on range *)
        r := F { aabb = aabb, density = density, next = next, body = body,
                 shape = shape, friction = friction, restitution = restitution,
                 proxy = proxy, filter = filter, sensor = sensor, data = data }

    fun set_next (r as ref (F { aabb, density, next = _, body, shape, friction,
                                restitution, proxy, filter, sensor, data }), next) =
        r := F { aabb = aabb, density = density, next = next, body = body,
                 shape = shape, friction = friction, restitution = restitution,
                 proxy = proxy, filter = filter, sensor = sensor, data = data }

    fun set_body (r as ref (F { aabb, density, next, body = _, shape, friction,
                                restitution, proxy, filter, sensor, data }), body) =
        r := F { aabb = aabb, density = density, next = next, body = body,
                 shape = shape, friction = friction, restitution = restitution,
                 proxy = proxy, filter = filter, sensor = sensor, data = data }

    fun set_shape (r as ref (F { aabb, density, next, body, shape = _, friction,
                                 restitution, proxy, filter, sensor, data }), shape) =
        r := F { aabb = aabb, density = density, next = next, body = body,
                 shape = shape, friction = friction, restitution = restitution,
                 proxy = proxy, filter = filter, sensor = sensor, data = data }

    fun set_friction (r as ref (F { aabb, density, next, body, shape, friction = _,
                                restitution, proxy, filter, sensor, data }), friction) =
        r := F { aabb = aabb, density = density, next = next, body = body,
                 shape = shape, friction = friction, restitution = restitution,
                 proxy = proxy, filter = filter, sensor = sensor, data = data }

    fun set_restitution (r as ref (F { aabb, density, next, body, shape, friction,
                                       restitution = _, proxy, filter, sensor, data }), 
                         restitution) =
        r := F { aabb = aabb, density = density, next = next, body = body,
                 shape = shape, friction = friction, restitution = restitution,
                 proxy = proxy, filter = filter, sensor = sensor, data = data }

    fun set_proxy (r as ref (F { aabb, density, next, body, shape, friction,
                                 restitution, proxy = _, filter, sensor, data }), proxy) =
        r := F { aabb = aabb, density = density, next = next, body = body,
                 shape = shape, friction = friction, restitution = restitution,
                 proxy = proxy, filter = filter, sensor = sensor, data = data }

    (* XXX exposed function needs to do SetFilterData below. *)
    fun set_filter (r as ref (F { aabb, density, next, body, shape, friction,
                                restitution, proxy, filter = _, sensor, data }), filter) =
        r := F { aabb = aabb, density = density, next = next, body = body,
                 shape = shape, friction = friction, restitution = restitution,
                 proxy = proxy, filter = filter, sensor = sensor, data = data }

    fun set_sensor (r as ref (F { aabb, density, next, body, shape, friction,
                                  restitution, proxy, filter, sensor = _, data }), sensor) =
        r := F { aabb = aabb, density = density, next = next, body = body,
                 shape = shape, friction = friction, restitution = restitution,
                 proxy = proxy, filter = filter, sensor = sensor, data = data }

    fun set_data (r as ref (F { aabb, density, next, body, shape, friction,
                                restitution, proxy, filter, sensor, data = _ }), data) =
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
        r := B { world = world, typ = typ, flags = flags, island_index = island_index, xf = xf,
                 sweep = sweep, linear_velocity = linear_velocity,
                 angular_velocity = angular_velocity, force = force, torque = torque,
                 prev = prev, next = next, fixture_list = fixture_list, 
                 fixture_count = fixture_count, joint_list = joint_list,
                 contact_list = contact_list, mass = mass, inv_mass = inv_mass, i = i, 
                 inv_i = inv_i, linear_damping = linear_damping, 
                 angular_damping = angular_damping, sleep_time = sleep_time, data = data }

    fun set_flags (r as ref (B { typ, flags = _, island_index, xf, sweep, 
                                 linear_velocity, angular_velocity, force, torque, 
                                 prev, next, fixture_list, fixture_count, 
                                 joint_list, contact_list, mass, inv_mass, i, 
                                 inv_i, linear_damping, angular_damping, 
                                 sleep_time, data, world }), flags) =
        r := B { world = world, typ = typ, flags = flags, island_index = island_index, xf = xf,
                 sweep = sweep, linear_velocity = linear_velocity,
                 angular_velocity = angular_velocity, force = force, torque = torque,
                 prev = prev, next = next, fixture_list = fixture_list, 
                 fixture_count = fixture_count, joint_list = joint_list,
                 contact_list = contact_list, mass = mass, inv_mass = inv_mass, i = i, 
                 inv_i = inv_i, linear_damping = linear_damping, 
                 angular_damping = angular_damping, sleep_time = sleep_time, data = data }

    fun set_island_index (r as ref (B { typ, flags, island_index = _, xf, sweep, 
                               linear_velocity, angular_velocity, force, torque, 
                               prev, next, fixture_list, fixture_count, 
                               joint_list, contact_list, mass, inv_mass, i, 
                               inv_i, linear_damping, angular_damping, 
                               sleep_time, data, world }), island_index) =
        r := B { world = world, typ = typ, flags = flags, island_index = island_index, xf = xf,
                 sweep = sweep, linear_velocity = linear_velocity,
                 angular_velocity = angular_velocity, force = force, torque = torque,
                 prev = prev, next = next, fixture_list = fixture_list, 
                 fixture_count = fixture_count, joint_list = joint_list,
                 contact_list = contact_list, mass = mass, inv_mass = inv_mass, i = i, 
                 inv_i = inv_i, linear_damping = linear_damping, 
                 angular_damping = angular_damping, sleep_time = sleep_time, data = data }

    fun set_xf (r as ref (B { typ, flags, island_index, xf = _, sweep, 
                               linear_velocity, angular_velocity, force, torque, 
                               prev, next, fixture_list, fixture_count, 
                               joint_list, contact_list, mass, inv_mass, i, 
                               inv_i, linear_damping, angular_damping, 
                               sleep_time, data, world }), xf) =
        r := B { world = world, typ = typ, flags = flags, island_index = island_index, xf = xf,
                 sweep = sweep, linear_velocity = linear_velocity,
                 angular_velocity = angular_velocity, force = force, torque = torque,
                 prev = prev, next = next, fixture_list = fixture_list, 
                 fixture_count = fixture_count, joint_list = joint_list,
                 contact_list = contact_list, mass = mass, inv_mass = inv_mass, i = i, 
                 inv_i = inv_i, linear_damping = linear_damping, 
                 angular_damping = angular_damping, sleep_time = sleep_time, data = data }

    fun set_sweep (r as ref (B { typ, flags, island_index, xf, sweep = _, 
                               linear_velocity, angular_velocity, force, torque, 
                               prev, next, fixture_list, fixture_count, 
                               joint_list, contact_list, mass, inv_mass, i, 
                               inv_i, linear_damping, angular_damping, 
                               sleep_time, data, world }), sweep) =
        r := B { world = world, typ = typ, flags = flags, island_index = island_index, xf = xf,
                 sweep = sweep, linear_velocity = linear_velocity,
                 angular_velocity = angular_velocity, force = force, torque = torque,
                 prev = prev, next = next, fixture_list = fixture_list, 
                 fixture_count = fixture_count, joint_list = joint_list,
                 contact_list = contact_list, mass = mass, inv_mass = inv_mass, i = i, 
                 inv_i = inv_i, linear_damping = linear_damping, 
                 angular_damping = angular_damping, sleep_time = sleep_time, data = data }

    fun set_linear_velocity (r as ref (B { typ, flags, island_index, xf, sweep, 
                               linear_velocity = _, angular_velocity, force, torque, 
                               prev, next, fixture_list, fixture_count, 
                               joint_list, contact_list, mass, inv_mass, i, 
                               inv_i, linear_damping, angular_damping, 
                               sleep_time, data, world }), linear_velocity) =
        r := B { world = world, typ = typ, flags = flags, island_index = island_index, xf = xf,
                 sweep = sweep, linear_velocity = linear_velocity,
                 angular_velocity = angular_velocity, force = force, torque = torque,
                 prev = prev, next = next, fixture_list = fixture_list, 
                 fixture_count = fixture_count, joint_list = joint_list,
                 contact_list = contact_list, mass = mass, inv_mass = inv_mass, i = i, 
                 inv_i = inv_i, linear_damping = linear_damping, 
                 angular_damping = angular_damping, sleep_time = sleep_time, data = data }

    fun set_angular_velocity (r as ref (B { typ, flags, island_index, xf, sweep, 
                               linear_velocity, angular_velocity = _, force, torque, 
                               prev, next, fixture_list, fixture_count, 
                               joint_list, contact_list, mass, inv_mass, i, 
                               inv_i, linear_damping, angular_damping, 
                               sleep_time, data, world }), angular_velocity) =
        r := B { world = world, typ = typ, flags = flags, island_index = island_index, xf = xf,
                 sweep = sweep, linear_velocity = linear_velocity,
                 angular_velocity = angular_velocity, force = force, torque = torque,
                 prev = prev, next = next, fixture_list = fixture_list, 
                 fixture_count = fixture_count, joint_list = joint_list,
                 contact_list = contact_list, mass = mass, inv_mass = inv_mass, i = i, 
                 inv_i = inv_i, linear_damping = linear_damping, 
                 angular_damping = angular_damping, sleep_time = sleep_time, data = data }

    fun set_force (r as ref (B { typ, flags, island_index, xf, sweep, 
                               linear_velocity, angular_velocity, force = _, torque, 
                               prev, next, fixture_list, fixture_count, 
                               joint_list, contact_list, mass, inv_mass, i, 
                               inv_i, linear_damping, angular_damping, 
                               sleep_time, data, world }), force) =
        r := B { world = world, typ = typ, flags = flags, island_index = island_index, xf = xf,
                 sweep = sweep, linear_velocity = linear_velocity,
                 angular_velocity = angular_velocity, force = force, torque = torque,
                 prev = prev, next = next, fixture_list = fixture_list, 
                 fixture_count = fixture_count, joint_list = joint_list,
                 contact_list = contact_list, mass = mass, inv_mass = inv_mass, i = i, 
                 inv_i = inv_i, linear_damping = linear_damping, 
                 angular_damping = angular_damping, sleep_time = sleep_time, data = data }

    fun set_torque (r as ref (B { typ, flags, island_index, xf, sweep, 
                               linear_velocity, angular_velocity, force, torque = _, 
                               prev, next, fixture_list, fixture_count, 
                               joint_list, contact_list, mass, inv_mass, i, 
                               inv_i, linear_damping, angular_damping, 
                               sleep_time, data, world }), torque) =
        r := B { world = world, typ = typ, flags = flags, island_index = island_index, xf = xf,
                 sweep = sweep, linear_velocity = linear_velocity,
                 angular_velocity = angular_velocity, force = force, torque = torque,
                 prev = prev, next = next, fixture_list = fixture_list, 
                 fixture_count = fixture_count, joint_list = joint_list,
                 contact_list = contact_list, mass = mass, inv_mass = inv_mass, i = i, 
                 inv_i = inv_i, linear_damping = linear_damping, 
                 angular_damping = angular_damping, sleep_time = sleep_time, data = data }

    fun set_prev (r as ref (B { typ, flags, island_index, xf, sweep, 
                               linear_velocity, angular_velocity, force, torque, 
                               prev = _, next, fixture_list, fixture_count, 
                               joint_list, contact_list, mass, inv_mass, i, 
                               inv_i, linear_damping, angular_damping, 
                               sleep_time, data, world }), prev) =
        r := B { world = world, typ = typ, flags = flags, island_index = island_index, xf = xf,
                 sweep = sweep, linear_velocity = linear_velocity,
                 angular_velocity = angular_velocity, force = force, torque = torque,
                 prev = prev, next = next, fixture_list = fixture_list, 
                 fixture_count = fixture_count, joint_list = joint_list,
                 contact_list = contact_list, mass = mass, inv_mass = inv_mass, i = i, 
                 inv_i = inv_i, linear_damping = linear_damping, 
                 angular_damping = angular_damping, sleep_time = sleep_time, data = data }

    fun set_next (r as ref (B { typ, flags, island_index, xf, sweep, 
                               linear_velocity, angular_velocity, force, torque, 
                               prev, next = _, fixture_list, fixture_count, 
                               joint_list, contact_list, mass, inv_mass, i, 
                               inv_i, linear_damping, angular_damping, 
                               sleep_time, data, world }), next) =
        r := B { world = world, typ = typ, flags = flags, island_index = island_index, xf = xf,
                 sweep = sweep, linear_velocity = linear_velocity,
                 angular_velocity = angular_velocity, force = force, torque = torque,
                 prev = prev, next = next, fixture_list = fixture_list, 
                 fixture_count = fixture_count, joint_list = joint_list,
                 contact_list = contact_list, mass = mass, inv_mass = inv_mass, i = i, 
                 inv_i = inv_i, linear_damping = linear_damping, 
                 angular_damping = angular_damping, sleep_time = sleep_time, data = data }

    fun set_fixture_list (r as ref (B { typ, flags, island_index, xf, sweep, 
                               linear_velocity, angular_velocity, force, torque, 
                               prev, next, fixture_list = _, fixture_count, 
                               joint_list, contact_list, mass, inv_mass, i, 
                               inv_i, linear_damping, angular_damping, 
                               sleep_time, data, world }), fixture_list) =
        r := B { world = world, typ = typ, flags = flags, island_index = island_index, xf = xf,
                 sweep = sweep, linear_velocity = linear_velocity,
                 angular_velocity = angular_velocity, force = force, torque = torque,
                 prev = prev, next = next, fixture_list = fixture_list, 
                 fixture_count = fixture_count, joint_list = joint_list,
                 contact_list = contact_list, mass = mass, inv_mass = inv_mass, i = i, 
                 inv_i = inv_i, linear_damping = linear_damping, 
                 angular_damping = angular_damping, sleep_time = sleep_time, data = data }

    fun set_fixture_count (r as ref (B { typ, flags, island_index, xf, sweep, 
                               linear_velocity, angular_velocity, force, torque, 
                               prev, next, fixture_list, fixture_count = _, 
                               joint_list, contact_list, mass, inv_mass, i, 
                               inv_i, linear_damping, angular_damping, 
                               sleep_time, data, world }), fixture_count) =
        r := B { world = world, typ = typ, flags = flags, island_index = island_index, xf = xf,
                 sweep = sweep, linear_velocity = linear_velocity,
                 angular_velocity = angular_velocity, force = force, torque = torque,
                 prev = prev, next = next, fixture_list = fixture_list, 
                 fixture_count = fixture_count, joint_list = joint_list,
                 contact_list = contact_list, mass = mass, inv_mass = inv_mass, i = i, 
                 inv_i = inv_i, linear_damping = linear_damping, 
                 angular_damping = angular_damping, sleep_time = sleep_time, data = data }

    fun set_joint_list (r as ref (B { typ, flags, island_index, xf, sweep, 
                               linear_velocity, angular_velocity, force, torque, 
                               prev, next, fixture_list, fixture_count, 
                               joint_list = _, contact_list, mass, inv_mass, i, 
                               inv_i, linear_damping, angular_damping, 
                               sleep_time, data, world }), joint_list) =
        r := B { world = world, typ = typ, flags = flags, island_index = island_index, xf = xf,
                 sweep = sweep, linear_velocity = linear_velocity,
                 angular_velocity = angular_velocity, force = force, torque = torque,
                 prev = prev, next = next, fixture_list = fixture_list, 
                 fixture_count = fixture_count, joint_list = joint_list,
                 contact_list = contact_list, mass = mass, inv_mass = inv_mass, i = i, 
                 inv_i = inv_i, linear_damping = linear_damping, 
                 angular_damping = angular_damping, sleep_time = sleep_time, data = data }

    fun set_contact_list (r as ref (B { typ, flags, island_index, xf, sweep, 
                               linear_velocity, angular_velocity, force, torque, 
                               prev, next, fixture_list, fixture_count, 
                               joint_list, contact_list = _, mass, inv_mass, i, 
                               inv_i, linear_damping, angular_damping, 
                               sleep_time, data, world }), contact_list) =
        r := B { world = world, typ = typ, flags = flags, island_index = island_index, xf = xf,
                 sweep = sweep, linear_velocity = linear_velocity,
                 angular_velocity = angular_velocity, force = force, torque = torque,
                 prev = prev, next = next, fixture_list = fixture_list, 
                 fixture_count = fixture_count, joint_list = joint_list,
                 contact_list = contact_list, mass = mass, inv_mass = inv_mass, i = i, 
                 inv_i = inv_i, linear_damping = linear_damping, 
                 angular_damping = angular_damping, sleep_time = sleep_time, data = data }

    fun set_mass (r as ref (B { typ, flags, island_index, xf, sweep, 
                               linear_velocity, angular_velocity, force, torque, 
                               prev, next, fixture_list, fixture_count, 
                               joint_list, contact_list, mass = _, inv_mass, i, 
                               inv_i, linear_damping, angular_damping, 
                               sleep_time, data, world }), mass) =
        r := B { world = world, typ = typ, flags = flags, island_index = island_index, xf = xf,
                 sweep = sweep, linear_velocity = linear_velocity,
                 angular_velocity = angular_velocity, force = force, torque = torque,
                 prev = prev, next = next, fixture_list = fixture_list, 
                 fixture_count = fixture_count, joint_list = joint_list,
                 contact_list = contact_list, mass = mass, inv_mass = inv_mass, i = i, 
                 inv_i = inv_i, linear_damping = linear_damping, 
                 angular_damping = angular_damping, sleep_time = sleep_time, data = data }

    fun set_inv_mass (r as ref (B { typ, flags, island_index, xf, sweep, 
                               linear_velocity, angular_velocity, force, torque, 
                               prev, next, fixture_list, fixture_count, 
                               joint_list, contact_list, mass, inv_mass = _, i, 
                               inv_i, linear_damping, angular_damping, 
                               sleep_time, data, world }), inv_mass) =
        r := B { world = world, typ = typ, flags = flags, island_index = island_index, xf = xf,
                 sweep = sweep, linear_velocity = linear_velocity,
                 angular_velocity = angular_velocity, force = force, torque = torque,
                 prev = prev, next = next, fixture_list = fixture_list, 
                 fixture_count = fixture_count, joint_list = joint_list,
                 contact_list = contact_list, mass = mass, inv_mass = inv_mass, i = i, 
                 inv_i = inv_i, linear_damping = linear_damping, 
                 angular_damping = angular_damping, sleep_time = sleep_time, data = data }

    fun set_i (r as ref (B { typ, flags, island_index, xf, sweep, 
                               linear_velocity, angular_velocity, force, torque, 
                               prev, next, fixture_list, fixture_count, 
                               joint_list, contact_list, mass, inv_mass, i = _, 
                               inv_i, linear_damping, angular_damping, 
                               sleep_time, data, world }), i) =
        r := B { world = world, typ = typ, flags = flags, island_index = island_index, xf = xf,
                 sweep = sweep, linear_velocity = linear_velocity,
                 angular_velocity = angular_velocity, force = force, torque = torque,
                 prev = prev, next = next, fixture_list = fixture_list, 
                 fixture_count = fixture_count, joint_list = joint_list,
                 contact_list = contact_list, mass = mass, inv_mass = inv_mass, i = i, 
                 inv_i = inv_i, linear_damping = linear_damping, 
                 angular_damping = angular_damping, sleep_time = sleep_time, data = data }

    fun set_inv_i (r as ref (B { typ, flags, island_index, xf, sweep, 
                               linear_velocity, angular_velocity, force, torque, 
                               prev, next, fixture_list, fixture_count, 
                               joint_list, contact_list, mass, inv_mass, i, 
                               inv_i = _, linear_damping, angular_damping, 
                               sleep_time, data, world }), inv_i) =
        r := B { world = world, typ = typ, flags = flags, island_index = island_index, xf = xf,
                 sweep = sweep, linear_velocity = linear_velocity,
                 angular_velocity = angular_velocity, force = force, torque = torque,
                 prev = prev, next = next, fixture_list = fixture_list, 
                 fixture_count = fixture_count, joint_list = joint_list,
                 contact_list = contact_list, mass = mass, inv_mass = inv_mass, i = i, 
                 inv_i = inv_i, linear_damping = linear_damping, 
                 angular_damping = angular_damping, sleep_time = sleep_time, data = data }

    fun set_linear_damping (r as ref (B { typ, flags, island_index, xf, sweep, 
                               linear_velocity, angular_velocity, force, torque, 
                               prev, next, fixture_list, fixture_count, 
                               joint_list, contact_list, mass, inv_mass, i, 
                               inv_i, linear_damping = _, angular_damping, 
                               sleep_time, data, world }), linear_damping) =
        r := B { world = world, typ = typ, flags = flags, island_index = island_index, xf = xf,
                 sweep = sweep, linear_velocity = linear_velocity,
                 angular_velocity = angular_velocity, force = force, torque = torque,
                 prev = prev, next = next, fixture_list = fixture_list, 
                 fixture_count = fixture_count, joint_list = joint_list,
                 contact_list = contact_list, mass = mass, inv_mass = inv_mass, i = i, 
                 inv_i = inv_i, linear_damping = linear_damping, 
                 angular_damping = angular_damping, sleep_time = sleep_time, data = data }

    fun set_angular_damping (r as ref (B { typ, flags, island_index, xf, sweep, 
                               linear_velocity, angular_velocity, force, torque, 
                               prev, next, fixture_list, fixture_count, 
                               joint_list, contact_list, mass, inv_mass, i, 
                               inv_i, linear_damping, angular_damping = _, 
                               sleep_time, data, world }), angular_damping) =
        r := B { world = world, typ = typ, flags = flags, island_index = island_index, xf = xf,
                 sweep = sweep, linear_velocity = linear_velocity,
                 angular_velocity = angular_velocity, force = force, torque = torque,
                 prev = prev, next = next, fixture_list = fixture_list, 
                 fixture_count = fixture_count, joint_list = joint_list,
                 contact_list = contact_list, mass = mass, inv_mass = inv_mass, i = i, 
                 inv_i = inv_i, linear_damping = linear_damping, 
                 angular_damping = angular_damping, sleep_time = sleep_time, data = data }

    fun set_sleep_time (r as ref (B { typ, flags, island_index, xf, sweep, 
                               linear_velocity, angular_velocity, force, torque, 
                               prev, next, fixture_list, fixture_count, 
                               joint_list, contact_list, mass, inv_mass, i, 
                               inv_i, linear_damping, angular_damping, 
                               sleep_time = _, data, world }), sleep_time) =
        r := B { world = world, typ = typ, flags = flags, island_index = island_index, xf = xf,
                 sweep = sweep, linear_velocity = linear_velocity,
                 angular_velocity = angular_velocity, force = force, torque = torque,
                 prev = prev, next = next, fixture_list = fixture_list, 
                 fixture_count = fixture_count, joint_list = joint_list,
                 contact_list = contact_list, mass = mass, inv_mass = inv_mass, i = i, 
                 inv_i = inv_i, linear_damping = linear_damping, 
                 angular_damping = angular_damping, sleep_time = sleep_time, data = data }

    fun set_data (r as ref (B { typ, flags, island_index, xf, sweep, 
                               linear_velocity, angular_velocity, force, torque, 
                               prev, next, fixture_list, fixture_count, 
                               joint_list, contact_list, mass, inv_mass, i, 
                               inv_i, linear_damping, angular_damping, 
                               sleep_time, data = _, world }), data) =
        r := B { world = world, typ = typ, flags = flags, island_index = island_index, xf = xf,
                 sweep = sweep, linear_velocity = linear_velocity,
                 angular_velocity = angular_velocity, force = force, torque = torque,
                 prev = prev, next = next, fixture_list = fixture_list, 
                 fixture_count = fixture_count, joint_list = joint_list,
                 contact_list = contact_list, mass = mass, inv_mass = inv_mass, i = i, 
                 inv_i = inv_i, linear_damping = linear_damping, 
                 angular_damping = angular_damping, sleep_time = sleep_time, data = data }

    fun get_flag (b, f) = Word16.andb (f, get_flags b) <> 0w0
    fun set_flag (b, f) = set_flags (b, Word16.orb(get_flags b, f))
    fun clear_flag (b, f) = set_flags (b, Word16.andb(get_flags b, Word16.notb f))

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

  (* Exported *)
  structure Body =
  struct
    open B
    datatype body_type = datatype body_type
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

    fun get_world_point (b, p) = get_xf b @*: p
    fun get_world_vector (b, v) = transformr (get_xf b) +*: v
    fun get_local_point (b, p) = mul_ttransformv (get_xf b, p)
    fun get_local_vector (b, v) = mul_t22mv (transformr (get_xf b), v)

    fun get_linear_velocity_from_world_point (b, world_point : vec2) =
        get_linear_velocity b :+: cross2sv (get_angular_velocity b,
                                            world_point :-: sweepc (get_sweep b))
        
    fun get_linear_velocity_from_local_point (b, local_point : vec2) =
        get_linear_velocity_from_world_point(b, get_world_point(b, local_point))

    fun set_bullet (b, f) = if f then set_flag (b, FLAG_BULLET)
                            else clear_flag (b, FLAG_BULLET)
    fun get_bullet b = get_flag (b, FLAG_BULLET)

    fun get_active b = get_flag (b, FLAG_ACTIVE)
    fun set_active (b, f) =
        raise BDDWorld "unimplemented (cpp)"

    fun reset_mass_data b =
        raise BDDWorld "unimplemented (cpp)"

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
                          filter : filter }) : fixture =
        raise BDDWorld "unimplemented"
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
                          filter = default_filter })

    fun destroy_fixture (body : body, fixture : fixture) : unit =
        raise BDDWorld "unimplemented"
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

    fun set_mass_data (body : body, mass_data : mass_data) : unit =
        raise BDDWorld "unimplemented"
(*
        b2Assert(m_world->IsLocked() == false);
        if (m_world->IsLocked() == true)
        {
                return;
        }

        if (m_type != b2_dynamicBody)
        {
                return;
        }

        m_invMass = 0.0f;
        m_I = 0.0f;
        m_invI = 0.0f;

        m_mass = massData->mass;
        if (m_mass <= 0.0f)
        {
                m_mass = 1.0f;
        }

        m_invMass = 1.0f / m_mass;

        if (massData->I > 0.0f && (m_flags & b2Body::e_fixedRotationFlag) == 0)
        {
                m_I = massData->I - m_mass * b2Dot(massData->center, massData->center);
                b2Assert(m_I > 0.0f);
                m_invI = 1.0f / m_I;
        }

        // Move center of mass.
        b2Vec2 oldCenter = m_sweep.c;
        m_sweep.localCenter = massData->center;
        m_sweep.c0 = m_sweep.c = b2Mul(m_xf, m_sweep.localCenter);

        // Update center of mass velocity.
        m_linearVelocity += b2Cross(m_angularVelocity, m_sweep.c - oldCenter);
}
*)

(*

inline void b2Body::SynchronizeTransform()
{
        m_xf.R.Set(m_sweep.a);
        m_xf.position = m_sweep.c - b2Mul(m_xf.R, m_sweep.localCenter);
}

inline void b2Body::Advance(float32 t)
{
        // Advance to the new safe time.
        m_sweep.Advance(t);
        m_sweep.c = m_sweep.c0;
        m_sweep.a = m_sweep.a0;
        SynchronizeTransform();
}
*)

    fun set_transform (b, position : vec2, angle : real) : unit =
        raise BDDWorld "unimplemented"
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
b2Body::b2Body(const b2BodyDef* bd, b2World* world)
{
        b2Assert(bd->position.IsValid());
        b2Assert(bd->linearVelocity.IsValid());
        b2Assert(b2IsValid(bd->angle));
        b2Assert(b2IsValid(bd->angularVelocity));
        b2Assert(b2IsValid(bd->inertiaScale) && bd->inertiaScale >= 0.0f);
        b2Assert(b2IsValid(bd->angularDamping) && bd->angularDamping >= 0.0f);
        b2Assert(b2IsValid(bd->linearDamping) && bd->linearDamping >= 0.0f);

        m_flags = 0;

        if (bd->bullet)
        {
                m_flags |= e_bulletFlag;
        }
        if (bd->fixedRotation)
        {
                m_flags |= e_fixedRotationFlag;
        }
        if (bd->allowSleep)
        {
                m_flags |= e_autoSleepFlag;
        }
        if (bd->awake)
        {
                m_flags |= e_awakeFlag;
        }
        if (bd->active)
        {
                m_flags |= e_activeFlag;
        }

        m_world = world;

        m_xf.position = bd->position;
        m_xf.R.Set(bd->angle);

        m_sweep.localCenter.SetZero();
        m_sweep.a0 = m_sweep.a = bd->angle;
        m_sweep.c0 = m_sweep.c = b2Mul(m_xf, m_sweep.localCenter);

        m_jointList = NULL;
        m_contactList = NULL;
        m_prev = NULL;
        m_next = NULL;

        m_linearVelocity = bd->linearVelocity;
        m_angularVelocity = bd->angularVelocity;

        m_linearDamping = bd->linearDamping;
        m_angularDamping = bd->angularDamping;

        m_force.SetZero();
        m_torque = 0.0f;

        m_sleepTime = 0.0f;

        m_type = bd->type;

        if (m_type == b2_dynamicBody)
        {
                m_mass = 1.0f;
                m_invMass = 1.0f;
        }
        else
        {
                m_mass = 0.0f;
                m_invMass = 0.0f;
        }

        m_I = 0.0f;
        m_invI = 0.0f;

        m_userData = bd->userData;

        m_fixtureList = NULL;
        m_fixtureCount = 0;
}

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


bool b2Body::ShouldCollide(const b2Body* other) const
{
        // At least one body should be dynamic.
        if (m_type != b2_dynamicBody && other->m_type != b2_dynamicBody)
        {
                return false;
        }

        // Does a joint prevent collision?
        for (b2JointEdge* jn = m_jointList; jn; jn = jn->next)
        {
                if (jn->other == other)
                {
                        if (jn->joint->m_collideConnected == false)
                        {
                                return false;
                        }
                }
        }

        return true;
}

void b2Body::SynchronizeFixtures()
{
        b2Transform xf1;
        xf1.R.Set(m_sweep.a0);
        xf1.position = m_sweep.c0 - b2Mul(xf1.R, m_sweep.localCenter);

        b2BroadPhase* broadPhase = &m_world->m_contactManager.m_broadPhase;
        for (b2Fixture* f = m_fixtureList; f; f = f->m_next)
        {
                f->Synchronize(broadPhase, xf1, m_xf);
        }
}

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

  (* Exported *)
  structure Fixture =
  struct
    type filter = filter
    open F

    fun filter_list { categories : int list,
                      mask : int list,
                      group_index : int } : filter =
        raise BDDWorld "unimplemented"

    fun filter_mask { category_bits : Word16.word,
                      mask_bits : Word16.word,
                      group_index : int } : filter =
        raise BDDWorld "unimplemented"

    fun fixture_transform f =
        case get_body f of
            NONE => raise BDDWorld "fixture is not attached to a body."
          | SOME b => B.get_xf b

    fun test_point (f, p : vec2) : bool =
        BDDShape.test_point (get_shape f, fixture_transform f, p)
        
    fun ray_cast (f, input) =
        BDDShape.ray_cast (get_shape f, fixture_transform f, input)

    fun get_mass_data f =
        BDDShape.compute_mass (get_shape f, get_density f)

    val is_sensor = get_sensor
    val shape = get_shape

    fun set_filter _ =
        raise BDDWorld "unimplemented"
(*
void b2Fixture::SetFilterData(const b2Filter& filter)
{
        m_filter = filter;

        if (m_body == NULL)
        {
                return;
        }

        // Flag associated contacts for filtering.
        b2ContactEdge* edge = m_body->GetContactList();
        while (edge)
        {
                b2Contact* contact = edge->contact;
                b2Fixture* fixtureA = contact->GetFixtureA();
                b2Fixture* fixtureB = contact->GetFixtureB();
                if (fixtureA == this || fixtureB == this)
                {
                        contact->FlagForFiltering();
                }

                edge = edge->next;
        }
}
*)

  end

  structure World =
  struct

    fun create_body (world : world,
        { (* The body type: static, kinematic, or dynamic.
             Note: if a dynamic body would have zero mass, the mass is set to one. *)
          typ : Body.body_type,

          (* The initial world position of the body. Avoid creating bodies at
             the origin since this can lead to many overlapping shapes. *)
          position : BDDMath.vec2,

          (* The world angle of the body in radians. *)
          angle : real,

          (* The linear velocity of the body's origin in world co-ordinates. *)
          linear_velocity : BDDMath.vec2,

          (* The angular velocity of the body. *)
          angular_velocity : real,

          (* Linear damping is use to reduce the linear velocity. The damping
             parameter can be larger than 1.0f but the damping effect
             becomes sensitive to the time step when the damping parameter
             is large.
             Default: 0.0 *)
          linear_damping : real,

          (* Angular damping is use to reduce the angular velocity. The damping
             parameter can be larger than 1.0f but the damping effect
             becomes sensitive to the time step when the damping
             parameter is large.
             Default: 0.0 *)
          angular_damping : real,

          (* Set this flag to false if this body should never fall asleep. Note
             that this increases CPU usage. *)
          allow_sleep : bool,

          (* Is this body initially awake or sleeping? *)
          awake : bool,

          (* Should this body be prevented from rotating? Useful for characters. *)
          fixed_rotation : bool,

          (* Is this a fast moving body that should be prevented from tunneling through
             other moving bodies? Note that all bodies are prevented from tunneling through
             kinematic and static bodies. This setting is only considered on dynamic bodies.
             You should use this flag sparingly since it increases processing time. *)
          bullet : bool,

          (* Does this body start out active? *)
          active : bool,

          (* Use this to store application specific body data. *)
          data : body_data,

          (* Experimental: scales the inertia tensor.
             Default: 1.0 *)
          inertia_scale : real }) : body =
        raise BDDWorld "unimplemented"
  
  end

end
