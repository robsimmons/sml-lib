(* Generated file. Do not edit! *)
structure BDDCells :> BDDCELLS =
struct
  datatype ('b, 'f, 'j) bodycell = B of {
    typ : BDDDynamicsTypes.body_type,
    flags : Word8.word,
    island_index : int,
    xf : BDDMath.transform,
    sweep : BDDMath.sweep,
    linear_velocity : BDDMath.vec2,
    angular_velocity : real,
    force : BDDMath.vec2,
    torque : real,
    world : ('b, 'f, 'j) worldcell ref,
    prev : ('b, 'f, 'j) bodycell ref option,
    next : ('b, 'f, 'j) bodycell ref option,
    fixture_list : ('b, 'f, 'j) fixturecell ref option,
    fixture_count : int,
    joint_list : ('b, 'f, 'j) jointedgecell ref option,
    contact_list : ('b, 'f, 'j) contactedgecell ref option,
    mass : real,
    inv_mass : real,
    i : real,
    inv_i : real,
    linear_damping : real,
    angular_damping : real,
    sleep_time : real,
    data : 'b }

  and ('b, 'f, 'j) fixturecell = F of {
    aabb : BDDTypes.aabb,
    density : real,
    next : ('b, 'f, 'j) fixturecell ref option,
    body : ('b, 'f, 'j) bodycell ref option,
    shape : BDDShape.shape,
    friction : real,
    restitution : real,
    proxy : ('b, 'f, 'j) fixturecell ref BDDBroadPhase.proxy option,
    filter : BDDDynamicsTypes.filter,
    sensor : bool,
    data : 'f }

  and ('b, 'f, 'j) contactcell = C of {
    flags : Word32.word,
    prev : ('b, 'f, 'j) contactcell ref option,
    next : ('b, 'f, 'j) contactcell ref option,
    node_a : ('b, 'f, 'j) contactedgecell ref,
    node_b : ('b, 'f, 'j) contactedgecell ref,
    fixture_a : ('b, 'f, 'j) fixturecell ref,
    fixture_b : ('b, 'f, 'j) fixturecell ref,
    manifold : BDDTypes.manifold,
    toi_count : int }

  and ('b, 'f, 'j) contactedgecell = E of {
    other : ('b, 'f, 'j) bodycell ref option,
    contact : ('b, 'f, 'j) contactcell ref option,
    prev : ('b, 'f, 'j) contactedgecell ref option,
    next : ('b, 'f, 'j) contactedgecell ref option }

  and ('b, 'f, 'j) jointcell = J of {
    flags : Word8.word,
    typ : BDDDynamicsTypes.joint_type,
    prev : ('b, 'f, 'j) jointcell ref option,
    next : ('b, 'f, 'j) jointcell ref option,
    edge_a : ('b, 'f, 'j) jointedgecell ref,
    edge_b : ('b, 'f, 'j) jointedgecell ref,
    body_a : ('b, 'f, 'j) bodycell ref,
    body_b : ('b, 'f, 'j) bodycell ref,
    data : 'j,
    local_center_a : BDDMath.vec2,
    local_center_b : BDDMath.vec2,
    inv_mass_a : real,
    inv_i_a : real,
    inv_mass_b : real,
    inv_i_b : real }

  and ('b, 'f, 'j) jointedgecell = G of {
    other : ('b, 'f, 'j) bodycell ref,
    joint : ('b, 'f, 'j) jointcell ref,
    prev : ('b, 'f, 'j) jointedgecell ref option,
    next : ('b, 'f, 'j) jointedgecell ref option }

  and ('b, 'f, 'j) worldcell = W of {
    flags : Word32.word,
    body_list : ('b, 'f, 'j) bodycell ref option,
    joint_list : ('b, 'f, 'j) jointcell ref option,
    body_count : int,
    joint_count : int,
    gravity : BDDMath.vec2,
    allow_sleep : bool,
    ground_body : ('b, 'f, 'j) bodycell ref option,
    goodbye_joint_hook : ('b, 'f, 'j) jointcell ref -> unit,
    goodbye_fixture_hook : ('b, 'f, 'j) fixturecell ref -> unit,
    inv_dt0 : real,
    warm_starting : bool,
    continuous_physics : bool,
    broad_phase : ('b, 'f, 'j) fixturecell ref BDDBroadPhase.broadphase,
    contact_list : ('b, 'f, 'j) contactcell ref option,
    contact_count : int,
    should_collide : ('b, 'f, 'j) fixturecell ref * ('b, 'f, 'j) fixturecell ref -> bool,
    begin_contact : ('b, 'f, 'j) contactcell ref -> unit,
    end_contact : ('b, 'f, 'j) contactcell ref -> unit,
    pre_solve : ('b, 'f, 'j) contactcell ref * BDDTypes.manifold -> unit,
    post_solve : ('b, 'f, 'j) contactcell ref * BDDDynamicsTypes.contact_impulse -> unit }

  type ('b, 'f, 'j) body = ('b, 'f, 'j) bodycell ref
  type ('b, 'f, 'j) fixture = ('b, 'f, 'j) fixturecell ref
  type ('b, 'f, 'j) contact = ('b, 'f, 'j) contactcell ref
  type ('b, 'f, 'j) contactedge = ('b, 'f, 'j) contactedgecell ref
  type ('b, 'f, 'j) joint = ('b, 'f, 'j) jointcell ref
  type ('b, 'f, 'j) jointedge = ('b, 'f, 'j) jointedgecell ref
  type ('b, 'f, 'j) world = ('b, 'f, 'j) worldcell ref

  structure B =
  struct
    fun get_typ (ref (B { typ, ... })) = typ
    fun get_flags (ref (B { flags, ... })) = flags
    fun get_island_index (ref (B { island_index, ... })) = island_index
    fun get_xf (ref (B { xf, ... })) = xf
    fun get_sweep (ref (B { sweep, ... })) = sweep
    fun get_linear_velocity (ref (B { linear_velocity, ... })) = linear_velocity
    fun get_angular_velocity (ref (B { angular_velocity, ... })) = angular_velocity
    fun get_force (ref (B { force, ... })) = force
    fun get_torque (ref (B { torque, ... })) = torque
    fun get_world (ref (B { world, ... })) = world
    fun get_prev (ref (B { prev, ... })) = prev
    fun get_next (ref (B { next, ... })) = next
    fun get_fixture_list (ref (B { fixture_list, ... })) = fixture_list
    fun get_fixture_count (ref (B { fixture_count, ... })) = fixture_count
    fun get_joint_list (ref (B { joint_list, ... })) = joint_list
    fun get_contact_list (ref (B { contact_list, ... })) = contact_list
    fun get_mass (ref (B { mass, ... })) = mass
    fun get_inv_mass (ref (B { inv_mass, ... })) = inv_mass
    fun get_i (ref (B { i, ... })) = i
    fun get_inv_i (ref (B { inv_i, ... })) = inv_i
    fun get_linear_damping (ref (B { linear_damping, ... })) = linear_damping
    fun get_angular_damping (ref (B { angular_damping, ... })) = angular_damping
    fun get_sleep_time (ref (B { sleep_time, ... })) = sleep_time
    fun get_data (ref (B { data, ... })) = data
    fun set_typ (r___ as ref (B { typ = _, flags, island_index, xf, sweep,
    linear_velocity, angular_velocity, force, torque, world, prev, next,
    fixture_list, fixture_count, joint_list, contact_list, mass, inv_mass, i,
    inv_i, linear_damping, angular_damping, sleep_time, data }), typ) =
      r___ := B { typ = typ, flags = flags, island_index = island_index, xf =
      xf, sweep = sweep, linear_velocity = linear_velocity, angular_velocity =
      angular_velocity, force = force, torque = torque, world = world, prev =
      prev, next = next, fixture_list = fixture_list, fixture_count =
      fixture_count, joint_list = joint_list, contact_list = contact_list, mass
      = mass, inv_mass = inv_mass, i = i, inv_i = inv_i, linear_damping =
      linear_damping, angular_damping = angular_damping, sleep_time =
      sleep_time, data = data }
    fun set_flags (r___ as ref (B { typ, flags = _, island_index, xf, sweep,
    linear_velocity, angular_velocity, force, torque, world, prev, next,
    fixture_list, fixture_count, joint_list, contact_list, mass, inv_mass, i,
    inv_i, linear_damping, angular_damping, sleep_time, data }), flags) =
      r___ := B { typ = typ, flags = flags, island_index = island_index, xf =
      xf, sweep = sweep, linear_velocity = linear_velocity, angular_velocity =
      angular_velocity, force = force, torque = torque, world = world, prev =
      prev, next = next, fixture_list = fixture_list, fixture_count =
      fixture_count, joint_list = joint_list, contact_list = contact_list, mass
      = mass, inv_mass = inv_mass, i = i, inv_i = inv_i, linear_damping =
      linear_damping, angular_damping = angular_damping, sleep_time =
      sleep_time, data = data }
    fun set_island_index (r___ as ref (B { typ, flags, island_index = _, xf,
    sweep, linear_velocity, angular_velocity, force, torque, world, prev, next,
    fixture_list, fixture_count, joint_list, contact_list, mass, inv_mass, i,
    inv_i, linear_damping, angular_damping, sleep_time, data }), island_index)
    =
      r___ := B { typ = typ, flags = flags, island_index = island_index, xf =
      xf, sweep = sweep, linear_velocity = linear_velocity, angular_velocity =
      angular_velocity, force = force, torque = torque, world = world, prev =
      prev, next = next, fixture_list = fixture_list, fixture_count =
      fixture_count, joint_list = joint_list, contact_list = contact_list, mass
      = mass, inv_mass = inv_mass, i = i, inv_i = inv_i, linear_damping =
      linear_damping, angular_damping = angular_damping, sleep_time =
      sleep_time, data = data }
    fun set_xf (r___ as ref (B { typ, flags, island_index, xf = _, sweep,
    linear_velocity, angular_velocity, force, torque, world, prev, next,
    fixture_list, fixture_count, joint_list, contact_list, mass, inv_mass, i,
    inv_i, linear_damping, angular_damping, sleep_time, data }), xf) =
      r___ := B { typ = typ, flags = flags, island_index = island_index, xf =
      xf, sweep = sweep, linear_velocity = linear_velocity, angular_velocity =
      angular_velocity, force = force, torque = torque, world = world, prev =
      prev, next = next, fixture_list = fixture_list, fixture_count =
      fixture_count, joint_list = joint_list, contact_list = contact_list, mass
      = mass, inv_mass = inv_mass, i = i, inv_i = inv_i, linear_damping =
      linear_damping, angular_damping = angular_damping, sleep_time =
      sleep_time, data = data }
    fun set_sweep (r___ as ref (B { typ, flags, island_index, xf, sweep = _,
    linear_velocity, angular_velocity, force, torque, world, prev, next,
    fixture_list, fixture_count, joint_list, contact_list, mass, inv_mass, i,
    inv_i, linear_damping, angular_damping, sleep_time, data }), sweep) =
      r___ := B { typ = typ, flags = flags, island_index = island_index, xf =
      xf, sweep = sweep, linear_velocity = linear_velocity, angular_velocity =
      angular_velocity, force = force, torque = torque, world = world, prev =
      prev, next = next, fixture_list = fixture_list, fixture_count =
      fixture_count, joint_list = joint_list, contact_list = contact_list, mass
      = mass, inv_mass = inv_mass, i = i, inv_i = inv_i, linear_damping =
      linear_damping, angular_damping = angular_damping, sleep_time =
      sleep_time, data = data }
    fun set_linear_velocity (r___ as ref (B { typ, flags, island_index, xf,
    sweep, linear_velocity = _, angular_velocity, force, torque, world, prev,
    next, fixture_list, fixture_count, joint_list, contact_list, mass,
    inv_mass, i, inv_i, linear_damping, angular_damping, sleep_time, data }),
    linear_velocity) =
      r___ := B { typ = typ, flags = flags, island_index = island_index, xf =
      xf, sweep = sweep, linear_velocity = linear_velocity, angular_velocity =
      angular_velocity, force = force, torque = torque, world = world, prev =
      prev, next = next, fixture_list = fixture_list, fixture_count =
      fixture_count, joint_list = joint_list, contact_list = contact_list, mass
      = mass, inv_mass = inv_mass, i = i, inv_i = inv_i, linear_damping =
      linear_damping, angular_damping = angular_damping, sleep_time =
      sleep_time, data = data }
    fun set_angular_velocity (r___ as ref (B { typ, flags, island_index, xf,
    sweep, linear_velocity, angular_velocity = _, force, torque, world, prev,
    next, fixture_list, fixture_count, joint_list, contact_list, mass,
    inv_mass, i, inv_i, linear_damping, angular_damping, sleep_time, data }),
    angular_velocity) =
      r___ := B { typ = typ, flags = flags, island_index = island_index, xf =
      xf, sweep = sweep, linear_velocity = linear_velocity, angular_velocity =
      angular_velocity, force = force, torque = torque, world = world, prev =
      prev, next = next, fixture_list = fixture_list, fixture_count =
      fixture_count, joint_list = joint_list, contact_list = contact_list, mass
      = mass, inv_mass = inv_mass, i = i, inv_i = inv_i, linear_damping =
      linear_damping, angular_damping = angular_damping, sleep_time =
      sleep_time, data = data }
    fun set_force (r___ as ref (B { typ, flags, island_index, xf, sweep,
    linear_velocity, angular_velocity, force = _, torque, world, prev, next,
    fixture_list, fixture_count, joint_list, contact_list, mass, inv_mass, i,
    inv_i, linear_damping, angular_damping, sleep_time, data }), force) =
      r___ := B { typ = typ, flags = flags, island_index = island_index, xf =
      xf, sweep = sweep, linear_velocity = linear_velocity, angular_velocity =
      angular_velocity, force = force, torque = torque, world = world, prev =
      prev, next = next, fixture_list = fixture_list, fixture_count =
      fixture_count, joint_list = joint_list, contact_list = contact_list, mass
      = mass, inv_mass = inv_mass, i = i, inv_i = inv_i, linear_damping =
      linear_damping, angular_damping = angular_damping, sleep_time =
      sleep_time, data = data }
    fun set_torque (r___ as ref (B { typ, flags, island_index, xf, sweep,
    linear_velocity, angular_velocity, force, torque = _, world, prev, next,
    fixture_list, fixture_count, joint_list, contact_list, mass, inv_mass, i,
    inv_i, linear_damping, angular_damping, sleep_time, data }), torque) =
      r___ := B { typ = typ, flags = flags, island_index = island_index, xf =
      xf, sweep = sweep, linear_velocity = linear_velocity, angular_velocity =
      angular_velocity, force = force, torque = torque, world = world, prev =
      prev, next = next, fixture_list = fixture_list, fixture_count =
      fixture_count, joint_list = joint_list, contact_list = contact_list, mass
      = mass, inv_mass = inv_mass, i = i, inv_i = inv_i, linear_damping =
      linear_damping, angular_damping = angular_damping, sleep_time =
      sleep_time, data = data }
    fun set_world (r___ as ref (B { typ, flags, island_index, xf, sweep,
    linear_velocity, angular_velocity, force, torque, world = _, prev, next,
    fixture_list, fixture_count, joint_list, contact_list, mass, inv_mass, i,
    inv_i, linear_damping, angular_damping, sleep_time, data }), world) =
      r___ := B { typ = typ, flags = flags, island_index = island_index, xf =
      xf, sweep = sweep, linear_velocity = linear_velocity, angular_velocity =
      angular_velocity, force = force, torque = torque, world = world, prev =
      prev, next = next, fixture_list = fixture_list, fixture_count =
      fixture_count, joint_list = joint_list, contact_list = contact_list, mass
      = mass, inv_mass = inv_mass, i = i, inv_i = inv_i, linear_damping =
      linear_damping, angular_damping = angular_damping, sleep_time =
      sleep_time, data = data }
    fun set_prev (r___ as ref (B { typ, flags, island_index, xf, sweep,
    linear_velocity, angular_velocity, force, torque, world, prev = _, next,
    fixture_list, fixture_count, joint_list, contact_list, mass, inv_mass, i,
    inv_i, linear_damping, angular_damping, sleep_time, data }), prev) =
      r___ := B { typ = typ, flags = flags, island_index = island_index, xf =
      xf, sweep = sweep, linear_velocity = linear_velocity, angular_velocity =
      angular_velocity, force = force, torque = torque, world = world, prev =
      prev, next = next, fixture_list = fixture_list, fixture_count =
      fixture_count, joint_list = joint_list, contact_list = contact_list, mass
      = mass, inv_mass = inv_mass, i = i, inv_i = inv_i, linear_damping =
      linear_damping, angular_damping = angular_damping, sleep_time =
      sleep_time, data = data }
    fun set_next (r___ as ref (B { typ, flags, island_index, xf, sweep,
    linear_velocity, angular_velocity, force, torque, world, prev, next = _,
    fixture_list, fixture_count, joint_list, contact_list, mass, inv_mass, i,
    inv_i, linear_damping, angular_damping, sleep_time, data }), next) =
      r___ := B { typ = typ, flags = flags, island_index = island_index, xf =
      xf, sweep = sweep, linear_velocity = linear_velocity, angular_velocity =
      angular_velocity, force = force, torque = torque, world = world, prev =
      prev, next = next, fixture_list = fixture_list, fixture_count =
      fixture_count, joint_list = joint_list, contact_list = contact_list, mass
      = mass, inv_mass = inv_mass, i = i, inv_i = inv_i, linear_damping =
      linear_damping, angular_damping = angular_damping, sleep_time =
      sleep_time, data = data }
    fun set_fixture_list (r___ as ref (B { typ, flags, island_index, xf, sweep,
    linear_velocity, angular_velocity, force, torque, world, prev, next,
    fixture_list = _, fixture_count, joint_list, contact_list, mass, inv_mass,
    i, inv_i, linear_damping, angular_damping, sleep_time, data }),
    fixture_list) =
      r___ := B { typ = typ, flags = flags, island_index = island_index, xf =
      xf, sweep = sweep, linear_velocity = linear_velocity, angular_velocity =
      angular_velocity, force = force, torque = torque, world = world, prev =
      prev, next = next, fixture_list = fixture_list, fixture_count =
      fixture_count, joint_list = joint_list, contact_list = contact_list, mass
      = mass, inv_mass = inv_mass, i = i, inv_i = inv_i, linear_damping =
      linear_damping, angular_damping = angular_damping, sleep_time =
      sleep_time, data = data }
    fun set_fixture_count (r___ as ref (B { typ, flags, island_index, xf,
    sweep, linear_velocity, angular_velocity, force, torque, world, prev, next,
    fixture_list, fixture_count = _, joint_list, contact_list, mass, inv_mass,
    i, inv_i, linear_damping, angular_damping, sleep_time, data }),
    fixture_count) =
      r___ := B { typ = typ, flags = flags, island_index = island_index, xf =
      xf, sweep = sweep, linear_velocity = linear_velocity, angular_velocity =
      angular_velocity, force = force, torque = torque, world = world, prev =
      prev, next = next, fixture_list = fixture_list, fixture_count =
      fixture_count, joint_list = joint_list, contact_list = contact_list, mass
      = mass, inv_mass = inv_mass, i = i, inv_i = inv_i, linear_damping =
      linear_damping, angular_damping = angular_damping, sleep_time =
      sleep_time, data = data }
    fun set_joint_list (r___ as ref (B { typ, flags, island_index, xf, sweep,
    linear_velocity, angular_velocity, force, torque, world, prev, next,
    fixture_list, fixture_count, joint_list = _, contact_list, mass, inv_mass,
    i, inv_i, linear_damping, angular_damping, sleep_time, data }), joint_list)
    =
      r___ := B { typ = typ, flags = flags, island_index = island_index, xf =
      xf, sweep = sweep, linear_velocity = linear_velocity, angular_velocity =
      angular_velocity, force = force, torque = torque, world = world, prev =
      prev, next = next, fixture_list = fixture_list, fixture_count =
      fixture_count, joint_list = joint_list, contact_list = contact_list, mass
      = mass, inv_mass = inv_mass, i = i, inv_i = inv_i, linear_damping =
      linear_damping, angular_damping = angular_damping, sleep_time =
      sleep_time, data = data }
    fun set_contact_list (r___ as ref (B { typ, flags, island_index, xf, sweep,
    linear_velocity, angular_velocity, force, torque, world, prev, next,
    fixture_list, fixture_count, joint_list, contact_list = _, mass, inv_mass,
    i, inv_i, linear_damping, angular_damping, sleep_time, data }),
    contact_list) =
      r___ := B { typ = typ, flags = flags, island_index = island_index, xf =
      xf, sweep = sweep, linear_velocity = linear_velocity, angular_velocity =
      angular_velocity, force = force, torque = torque, world = world, prev =
      prev, next = next, fixture_list = fixture_list, fixture_count =
      fixture_count, joint_list = joint_list, contact_list = contact_list, mass
      = mass, inv_mass = inv_mass, i = i, inv_i = inv_i, linear_damping =
      linear_damping, angular_damping = angular_damping, sleep_time =
      sleep_time, data = data }
    fun set_mass (r___ as ref (B { typ, flags, island_index, xf, sweep,
    linear_velocity, angular_velocity, force, torque, world, prev, next,
    fixture_list, fixture_count, joint_list, contact_list, mass = _, inv_mass,
    i, inv_i, linear_damping, angular_damping, sleep_time, data }), mass) =
      r___ := B { typ = typ, flags = flags, island_index = island_index, xf =
      xf, sweep = sweep, linear_velocity = linear_velocity, angular_velocity =
      angular_velocity, force = force, torque = torque, world = world, prev =
      prev, next = next, fixture_list = fixture_list, fixture_count =
      fixture_count, joint_list = joint_list, contact_list = contact_list, mass
      = mass, inv_mass = inv_mass, i = i, inv_i = inv_i, linear_damping =
      linear_damping, angular_damping = angular_damping, sleep_time =
      sleep_time, data = data }
    fun set_inv_mass (r___ as ref (B { typ, flags, island_index, xf, sweep,
    linear_velocity, angular_velocity, force, torque, world, prev, next,
    fixture_list, fixture_count, joint_list, contact_list, mass, inv_mass = _,
    i, inv_i, linear_damping, angular_damping, sleep_time, data }), inv_mass) =
      r___ := B { typ = typ, flags = flags, island_index = island_index, xf =
      xf, sweep = sweep, linear_velocity = linear_velocity, angular_velocity =
      angular_velocity, force = force, torque = torque, world = world, prev =
      prev, next = next, fixture_list = fixture_list, fixture_count =
      fixture_count, joint_list = joint_list, contact_list = contact_list, mass
      = mass, inv_mass = inv_mass, i = i, inv_i = inv_i, linear_damping =
      linear_damping, angular_damping = angular_damping, sleep_time =
      sleep_time, data = data }
    fun set_i (r___ as ref (B { typ, flags, island_index, xf, sweep,
    linear_velocity, angular_velocity, force, torque, world, prev, next,
    fixture_list, fixture_count, joint_list, contact_list, mass, inv_mass, i =
    _, inv_i, linear_damping, angular_damping, sleep_time, data }), i) =
      r___ := B { typ = typ, flags = flags, island_index = island_index, xf =
      xf, sweep = sweep, linear_velocity = linear_velocity, angular_velocity =
      angular_velocity, force = force, torque = torque, world = world, prev =
      prev, next = next, fixture_list = fixture_list, fixture_count =
      fixture_count, joint_list = joint_list, contact_list = contact_list, mass
      = mass, inv_mass = inv_mass, i = i, inv_i = inv_i, linear_damping =
      linear_damping, angular_damping = angular_damping, sleep_time =
      sleep_time, data = data }
    fun set_inv_i (r___ as ref (B { typ, flags, island_index, xf, sweep,
    linear_velocity, angular_velocity, force, torque, world, prev, next,
    fixture_list, fixture_count, joint_list, contact_list, mass, inv_mass, i,
    inv_i = _, linear_damping, angular_damping, sleep_time, data }), inv_i) =
      r___ := B { typ = typ, flags = flags, island_index = island_index, xf =
      xf, sweep = sweep, linear_velocity = linear_velocity, angular_velocity =
      angular_velocity, force = force, torque = torque, world = world, prev =
      prev, next = next, fixture_list = fixture_list, fixture_count =
      fixture_count, joint_list = joint_list, contact_list = contact_list, mass
      = mass, inv_mass = inv_mass, i = i, inv_i = inv_i, linear_damping =
      linear_damping, angular_damping = angular_damping, sleep_time =
      sleep_time, data = data }
    fun set_linear_damping (r___ as ref (B { typ, flags, island_index, xf,
    sweep, linear_velocity, angular_velocity, force, torque, world, prev, next,
    fixture_list, fixture_count, joint_list, contact_list, mass, inv_mass, i,
    inv_i, linear_damping = _, angular_damping, sleep_time, data }),
    linear_damping) =
      r___ := B { typ = typ, flags = flags, island_index = island_index, xf =
      xf, sweep = sweep, linear_velocity = linear_velocity, angular_velocity =
      angular_velocity, force = force, torque = torque, world = world, prev =
      prev, next = next, fixture_list = fixture_list, fixture_count =
      fixture_count, joint_list = joint_list, contact_list = contact_list, mass
      = mass, inv_mass = inv_mass, i = i, inv_i = inv_i, linear_damping =
      linear_damping, angular_damping = angular_damping, sleep_time =
      sleep_time, data = data }
    fun set_angular_damping (r___ as ref (B { typ, flags, island_index, xf,
    sweep, linear_velocity, angular_velocity, force, torque, world, prev, next,
    fixture_list, fixture_count, joint_list, contact_list, mass, inv_mass, i,
    inv_i, linear_damping, angular_damping = _, sleep_time, data }),
    angular_damping) =
      r___ := B { typ = typ, flags = flags, island_index = island_index, xf =
      xf, sweep = sweep, linear_velocity = linear_velocity, angular_velocity =
      angular_velocity, force = force, torque = torque, world = world, prev =
      prev, next = next, fixture_list = fixture_list, fixture_count =
      fixture_count, joint_list = joint_list, contact_list = contact_list, mass
      = mass, inv_mass = inv_mass, i = i, inv_i = inv_i, linear_damping =
      linear_damping, angular_damping = angular_damping, sleep_time =
      sleep_time, data = data }
    fun set_sleep_time (r___ as ref (B { typ, flags, island_index, xf, sweep,
    linear_velocity, angular_velocity, force, torque, world, prev, next,
    fixture_list, fixture_count, joint_list, contact_list, mass, inv_mass, i,
    inv_i, linear_damping, angular_damping, sleep_time = _, data }),
    sleep_time) =
      r___ := B { typ = typ, flags = flags, island_index = island_index, xf =
      xf, sweep = sweep, linear_velocity = linear_velocity, angular_velocity =
      angular_velocity, force = force, torque = torque, world = world, prev =
      prev, next = next, fixture_list = fixture_list, fixture_count =
      fixture_count, joint_list = joint_list, contact_list = contact_list, mass
      = mass, inv_mass = inv_mass, i = i, inv_i = inv_i, linear_damping =
      linear_damping, angular_damping = angular_damping, sleep_time =
      sleep_time, data = data }
    fun set_data (r___ as ref (B { typ, flags, island_index, xf, sweep,
    linear_velocity, angular_velocity, force, torque, world, prev, next,
    fixture_list, fixture_count, joint_list, contact_list, mass, inv_mass, i,
    inv_i, linear_damping, angular_damping, sleep_time, data = _ }), data) =
      r___ := B { typ = typ, flags = flags, island_index = island_index, xf =
      xf, sweep = sweep, linear_velocity = linear_velocity, angular_velocity =
      angular_velocity, force = force, torque = torque, world = world, prev =
      prev, next = next, fixture_list = fixture_list, fixture_count =
      fixture_count, joint_list = joint_list, contact_list = contact_list, mass
      = mass, inv_mass = inv_mass, i = i, inv_i = inv_i, linear_damping =
      linear_damping, angular_damping = angular_damping, sleep_time =
      sleep_time, data = data }
    fun new ({ typ, flags, island_index, xf, sweep, linear_velocity,
    angular_velocity, force, torque, world, prev, next, fixture_list,
    fixture_count, joint_list, contact_list, mass, inv_mass, i, inv_i,
    linear_damping, angular_damping, sleep_time, data }) = 
      ref (B { typ = typ, flags = flags, island_index = island_index, xf = xf,
      sweep = sweep, linear_velocity = linear_velocity, angular_velocity =
      angular_velocity, force = force, torque = torque, world = world, prev =
      prev, next = next, fixture_list = fixture_list, fixture_count =
      fixture_count, joint_list = joint_list, contact_list = contact_list, mass
      = mass, inv_mass = inv_mass, i = i, inv_i = inv_i, linear_damping =
      linear_damping, angular_damping = angular_damping, sleep_time =
      sleep_time, data = data })
    val eq = op=
  end
  structure F =
  struct
    fun get_aabb (ref (F { aabb, ... })) = aabb
    fun get_density (ref (F { density, ... })) = density
    fun get_next (ref (F { next, ... })) = next
    fun get_body (ref (F { body, ... })) = body
    fun get_shape (ref (F { shape, ... })) = shape
    fun get_friction (ref (F { friction, ... })) = friction
    fun get_restitution (ref (F { restitution, ... })) = restitution
    fun get_proxy (ref (F { proxy, ... })) = proxy
    fun get_filter (ref (F { filter, ... })) = filter
    fun get_sensor (ref (F { sensor, ... })) = sensor
    fun get_data (ref (F { data, ... })) = data
    fun set_aabb (r___ as ref (F { aabb = _, density, next, body, shape,
    friction, restitution, proxy, filter, sensor, data }), aabb) =
      r___ := F { aabb = aabb, density = density, next = next, body = body,
      shape = shape, friction = friction, restitution = restitution, proxy =
      proxy, filter = filter, sensor = sensor, data = data }
    fun set_density (r___ as ref (F { aabb, density = _, next, body, shape,
    friction, restitution, proxy, filter, sensor, data }), density) =
      r___ := F { aabb = aabb, density = density, next = next, body = body,
      shape = shape, friction = friction, restitution = restitution, proxy =
      proxy, filter = filter, sensor = sensor, data = data }
    fun set_next (r___ as ref (F { aabb, density, next = _, body, shape,
    friction, restitution, proxy, filter, sensor, data }), next) =
      r___ := F { aabb = aabb, density = density, next = next, body = body,
      shape = shape, friction = friction, restitution = restitution, proxy =
      proxy, filter = filter, sensor = sensor, data = data }
    fun set_body (r___ as ref (F { aabb, density, next, body = _, shape,
    friction, restitution, proxy, filter, sensor, data }), body) =
      r___ := F { aabb = aabb, density = density, next = next, body = body,
      shape = shape, friction = friction, restitution = restitution, proxy =
      proxy, filter = filter, sensor = sensor, data = data }
    fun set_shape (r___ as ref (F { aabb, density, next, body, shape = _,
    friction, restitution, proxy, filter, sensor, data }), shape) =
      r___ := F { aabb = aabb, density = density, next = next, body = body,
      shape = shape, friction = friction, restitution = restitution, proxy =
      proxy, filter = filter, sensor = sensor, data = data }
    fun set_friction (r___ as ref (F { aabb, density, next, body, shape,
    friction = _, restitution, proxy, filter, sensor, data }), friction) =
      r___ := F { aabb = aabb, density = density, next = next, body = body,
      shape = shape, friction = friction, restitution = restitution, proxy =
      proxy, filter = filter, sensor = sensor, data = data }
    fun set_restitution (r___ as ref (F { aabb, density, next, body, shape,
    friction, restitution = _, proxy, filter, sensor, data }), restitution) =
      r___ := F { aabb = aabb, density = density, next = next, body = body,
      shape = shape, friction = friction, restitution = restitution, proxy =
      proxy, filter = filter, sensor = sensor, data = data }
    fun set_proxy (r___ as ref (F { aabb, density, next, body, shape, friction,
    restitution, proxy = _, filter, sensor, data }), proxy) =
      r___ := F { aabb = aabb, density = density, next = next, body = body,
      shape = shape, friction = friction, restitution = restitution, proxy =
      proxy, filter = filter, sensor = sensor, data = data }
    fun set_filter (r___ as ref (F { aabb, density, next, body, shape,
    friction, restitution, proxy, filter = _, sensor, data }), filter) =
      r___ := F { aabb = aabb, density = density, next = next, body = body,
      shape = shape, friction = friction, restitution = restitution, proxy =
      proxy, filter = filter, sensor = sensor, data = data }
    fun set_sensor (r___ as ref (F { aabb, density, next, body, shape,
    friction, restitution, proxy, filter, sensor = _, data }), sensor) =
      r___ := F { aabb = aabb, density = density, next = next, body = body,
      shape = shape, friction = friction, restitution = restitution, proxy =
      proxy, filter = filter, sensor = sensor, data = data }
    fun set_data (r___ as ref (F { aabb, density, next, body, shape, friction,
    restitution, proxy, filter, sensor, data = _ }), data) =
      r___ := F { aabb = aabb, density = density, next = next, body = body,
      shape = shape, friction = friction, restitution = restitution, proxy =
      proxy, filter = filter, sensor = sensor, data = data }
    fun new ({ aabb, density, next, body, shape, friction, restitution, proxy,
    filter, sensor, data }) = 
      ref (F { aabb = aabb, density = density, next = next, body = body, shape
      = shape, friction = friction, restitution = restitution, proxy = proxy,
      filter = filter, sensor = sensor, data = data })
    val eq = op=
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
    fun set_flags (r___ as ref (C { flags = _, prev, next, node_a, node_b,
    fixture_a, fixture_b, manifold, toi_count }), flags) =
      r___ := C { flags = flags, prev = prev, next = next, node_a = node_a,
      node_b = node_b, fixture_a = fixture_a, fixture_b = fixture_b, manifold =
      manifold, toi_count = toi_count }
    fun set_prev (r___ as ref (C { flags, prev = _, next, node_a, node_b,
    fixture_a, fixture_b, manifold, toi_count }), prev) =
      r___ := C { flags = flags, prev = prev, next = next, node_a = node_a,
      node_b = node_b, fixture_a = fixture_a, fixture_b = fixture_b, manifold =
      manifold, toi_count = toi_count }
    fun set_next (r___ as ref (C { flags, prev, next = _, node_a, node_b,
    fixture_a, fixture_b, manifold, toi_count }), next) =
      r___ := C { flags = flags, prev = prev, next = next, node_a = node_a,
      node_b = node_b, fixture_a = fixture_a, fixture_b = fixture_b, manifold =
      manifold, toi_count = toi_count }
    fun set_node_a (r___ as ref (C { flags, prev, next, node_a = _, node_b,
    fixture_a, fixture_b, manifold, toi_count }), node_a) =
      r___ := C { flags = flags, prev = prev, next = next, node_a = node_a,
      node_b = node_b, fixture_a = fixture_a, fixture_b = fixture_b, manifold =
      manifold, toi_count = toi_count }
    fun set_node_b (r___ as ref (C { flags, prev, next, node_a, node_b = _,
    fixture_a, fixture_b, manifold, toi_count }), node_b) =
      r___ := C { flags = flags, prev = prev, next = next, node_a = node_a,
      node_b = node_b, fixture_a = fixture_a, fixture_b = fixture_b, manifold =
      manifold, toi_count = toi_count }
    fun set_fixture_a (r___ as ref (C { flags, prev, next, node_a, node_b,
    fixture_a = _, fixture_b, manifold, toi_count }), fixture_a) =
      r___ := C { flags = flags, prev = prev, next = next, node_a = node_a,
      node_b = node_b, fixture_a = fixture_a, fixture_b = fixture_b, manifold =
      manifold, toi_count = toi_count }
    fun set_fixture_b (r___ as ref (C { flags, prev, next, node_a, node_b,
    fixture_a, fixture_b = _, manifold, toi_count }), fixture_b) =
      r___ := C { flags = flags, prev = prev, next = next, node_a = node_a,
      node_b = node_b, fixture_a = fixture_a, fixture_b = fixture_b, manifold =
      manifold, toi_count = toi_count }
    fun set_manifold (r___ as ref (C { flags, prev, next, node_a, node_b,
    fixture_a, fixture_b, manifold = _, toi_count }), manifold) =
      r___ := C { flags = flags, prev = prev, next = next, node_a = node_a,
      node_b = node_b, fixture_a = fixture_a, fixture_b = fixture_b, manifold =
      manifold, toi_count = toi_count }
    fun set_toi_count (r___ as ref (C { flags, prev, next, node_a, node_b,
    fixture_a, fixture_b, manifold, toi_count = _ }), toi_count) =
      r___ := C { flags = flags, prev = prev, next = next, node_a = node_a,
      node_b = node_b, fixture_a = fixture_a, fixture_b = fixture_b, manifold =
      manifold, toi_count = toi_count }
    fun new ({ flags, prev, next, node_a, node_b, fixture_a, fixture_b,
    manifold, toi_count }) = 
      ref (C { flags = flags, prev = prev, next = next, node_a = node_a, node_b
      = node_b, fixture_a = fixture_a, fixture_b = fixture_b, manifold =
      manifold, toi_count = toi_count })
    val eq = op=
  end
  structure E =
  struct
    fun get_other (ref (E { other, ... })) = other
    fun get_contact (ref (E { contact, ... })) = contact
    fun get_prev (ref (E { prev, ... })) = prev
    fun get_next (ref (E { next, ... })) = next
    fun set_other (r___ as ref (E { other = _, contact, prev, next }), other) =
      r___ := E { other = other, contact = contact, prev = prev, next = next }
    fun set_contact (r___ as ref (E { other, contact = _, prev, next }),
    contact) =
      r___ := E { other = other, contact = contact, prev = prev, next = next }
    fun set_prev (r___ as ref (E { other, contact, prev = _, next }), prev) =
      r___ := E { other = other, contact = contact, prev = prev, next = next }
    fun set_next (r___ as ref (E { other, contact, prev, next = _ }), next) =
      r___ := E { other = other, contact = contact, prev = prev, next = next }
    fun new ({ other, contact, prev, next }) = 
      ref (E { other = other, contact = contact, prev = prev, next = next })
    val eq = op=
  end
  structure J =
  struct
    fun get_flags (ref (J { flags, ... })) = flags
    fun get_typ (ref (J { typ, ... })) = typ
    fun get_prev (ref (J { prev, ... })) = prev
    fun get_next (ref (J { next, ... })) = next
    fun get_edge_a (ref (J { edge_a, ... })) = edge_a
    fun get_edge_b (ref (J { edge_b, ... })) = edge_b
    fun get_body_a (ref (J { body_a, ... })) = body_a
    fun get_body_b (ref (J { body_b, ... })) = body_b
    fun get_data (ref (J { data, ... })) = data
    fun get_local_center_a (ref (J { local_center_a, ... })) = local_center_a
    fun get_local_center_b (ref (J { local_center_b, ... })) = local_center_b
    fun get_inv_mass_a (ref (J { inv_mass_a, ... })) = inv_mass_a
    fun get_inv_i_a (ref (J { inv_i_a, ... })) = inv_i_a
    fun get_inv_mass_b (ref (J { inv_mass_b, ... })) = inv_mass_b
    fun get_inv_i_b (ref (J { inv_i_b, ... })) = inv_i_b
    fun set_flags (r___ as ref (J { flags = _, typ, prev, next, edge_a, edge_b,
    body_a, body_b, data, local_center_a, local_center_b, inv_mass_a, inv_i_a,
    inv_mass_b, inv_i_b }), flags) =
      r___ := J { flags = flags, typ = typ, prev = prev, next = next, edge_a =
      edge_a, edge_b = edge_b, body_a = body_a, body_b = body_b, data = data,
      local_center_a = local_center_a, local_center_b = local_center_b,
      inv_mass_a = inv_mass_a, inv_i_a = inv_i_a, inv_mass_b = inv_mass_b,
      inv_i_b = inv_i_b }
    fun set_typ (r___ as ref (J { flags, typ = _, prev, next, edge_a, edge_b,
    body_a, body_b, data, local_center_a, local_center_b, inv_mass_a, inv_i_a,
    inv_mass_b, inv_i_b }), typ) =
      r___ := J { flags = flags, typ = typ, prev = prev, next = next, edge_a =
      edge_a, edge_b = edge_b, body_a = body_a, body_b = body_b, data = data,
      local_center_a = local_center_a, local_center_b = local_center_b,
      inv_mass_a = inv_mass_a, inv_i_a = inv_i_a, inv_mass_b = inv_mass_b,
      inv_i_b = inv_i_b }
    fun set_prev (r___ as ref (J { flags, typ, prev = _, next, edge_a, edge_b,
    body_a, body_b, data, local_center_a, local_center_b, inv_mass_a, inv_i_a,
    inv_mass_b, inv_i_b }), prev) =
      r___ := J { flags = flags, typ = typ, prev = prev, next = next, edge_a =
      edge_a, edge_b = edge_b, body_a = body_a, body_b = body_b, data = data,
      local_center_a = local_center_a, local_center_b = local_center_b,
      inv_mass_a = inv_mass_a, inv_i_a = inv_i_a, inv_mass_b = inv_mass_b,
      inv_i_b = inv_i_b }
    fun set_next (r___ as ref (J { flags, typ, prev, next = _, edge_a, edge_b,
    body_a, body_b, data, local_center_a, local_center_b, inv_mass_a, inv_i_a,
    inv_mass_b, inv_i_b }), next) =
      r___ := J { flags = flags, typ = typ, prev = prev, next = next, edge_a =
      edge_a, edge_b = edge_b, body_a = body_a, body_b = body_b, data = data,
      local_center_a = local_center_a, local_center_b = local_center_b,
      inv_mass_a = inv_mass_a, inv_i_a = inv_i_a, inv_mass_b = inv_mass_b,
      inv_i_b = inv_i_b }
    fun set_edge_a (r___ as ref (J { flags, typ, prev, next, edge_a = _,
    edge_b, body_a, body_b, data, local_center_a, local_center_b, inv_mass_a,
    inv_i_a, inv_mass_b, inv_i_b }), edge_a) =
      r___ := J { flags = flags, typ = typ, prev = prev, next = next, edge_a =
      edge_a, edge_b = edge_b, body_a = body_a, body_b = body_b, data = data,
      local_center_a = local_center_a, local_center_b = local_center_b,
      inv_mass_a = inv_mass_a, inv_i_a = inv_i_a, inv_mass_b = inv_mass_b,
      inv_i_b = inv_i_b }
    fun set_edge_b (r___ as ref (J { flags, typ, prev, next, edge_a, edge_b =
    _, body_a, body_b, data, local_center_a, local_center_b, inv_mass_a,
    inv_i_a, inv_mass_b, inv_i_b }), edge_b) =
      r___ := J { flags = flags, typ = typ, prev = prev, next = next, edge_a =
      edge_a, edge_b = edge_b, body_a = body_a, body_b = body_b, data = data,
      local_center_a = local_center_a, local_center_b = local_center_b,
      inv_mass_a = inv_mass_a, inv_i_a = inv_i_a, inv_mass_b = inv_mass_b,
      inv_i_b = inv_i_b }
    fun set_body_a (r___ as ref (J { flags, typ, prev, next, edge_a, edge_b,
    body_a = _, body_b, data, local_center_a, local_center_b, inv_mass_a,
    inv_i_a, inv_mass_b, inv_i_b }), body_a) =
      r___ := J { flags = flags, typ = typ, prev = prev, next = next, edge_a =
      edge_a, edge_b = edge_b, body_a = body_a, body_b = body_b, data = data,
      local_center_a = local_center_a, local_center_b = local_center_b,
      inv_mass_a = inv_mass_a, inv_i_a = inv_i_a, inv_mass_b = inv_mass_b,
      inv_i_b = inv_i_b }
    fun set_body_b (r___ as ref (J { flags, typ, prev, next, edge_a, edge_b,
    body_a, body_b = _, data, local_center_a, local_center_b, inv_mass_a,
    inv_i_a, inv_mass_b, inv_i_b }), body_b) =
      r___ := J { flags = flags, typ = typ, prev = prev, next = next, edge_a =
      edge_a, edge_b = edge_b, body_a = body_a, body_b = body_b, data = data,
      local_center_a = local_center_a, local_center_b = local_center_b,
      inv_mass_a = inv_mass_a, inv_i_a = inv_i_a, inv_mass_b = inv_mass_b,
      inv_i_b = inv_i_b }
    fun set_data (r___ as ref (J { flags, typ, prev, next, edge_a, edge_b,
    body_a, body_b, data = _, local_center_a, local_center_b, inv_mass_a,
    inv_i_a, inv_mass_b, inv_i_b }), data) =
      r___ := J { flags = flags, typ = typ, prev = prev, next = next, edge_a =
      edge_a, edge_b = edge_b, body_a = body_a, body_b = body_b, data = data,
      local_center_a = local_center_a, local_center_b = local_center_b,
      inv_mass_a = inv_mass_a, inv_i_a = inv_i_a, inv_mass_b = inv_mass_b,
      inv_i_b = inv_i_b }
    fun set_local_center_a (r___ as ref (J { flags, typ, prev, next, edge_a,
    edge_b, body_a, body_b, data, local_center_a = _, local_center_b,
    inv_mass_a, inv_i_a, inv_mass_b, inv_i_b }), local_center_a) =
      r___ := J { flags = flags, typ = typ, prev = prev, next = next, edge_a =
      edge_a, edge_b = edge_b, body_a = body_a, body_b = body_b, data = data,
      local_center_a = local_center_a, local_center_b = local_center_b,
      inv_mass_a = inv_mass_a, inv_i_a = inv_i_a, inv_mass_b = inv_mass_b,
      inv_i_b = inv_i_b }
    fun set_local_center_b (r___ as ref (J { flags, typ, prev, next, edge_a,
    edge_b, body_a, body_b, data, local_center_a, local_center_b = _,
    inv_mass_a, inv_i_a, inv_mass_b, inv_i_b }), local_center_b) =
      r___ := J { flags = flags, typ = typ, prev = prev, next = next, edge_a =
      edge_a, edge_b = edge_b, body_a = body_a, body_b = body_b, data = data,
      local_center_a = local_center_a, local_center_b = local_center_b,
      inv_mass_a = inv_mass_a, inv_i_a = inv_i_a, inv_mass_b = inv_mass_b,
      inv_i_b = inv_i_b }
    fun set_inv_mass_a (r___ as ref (J { flags, typ, prev, next, edge_a,
    edge_b, body_a, body_b, data, local_center_a, local_center_b, inv_mass_a =
    _, inv_i_a, inv_mass_b, inv_i_b }), inv_mass_a) =
      r___ := J { flags = flags, typ = typ, prev = prev, next = next, edge_a =
      edge_a, edge_b = edge_b, body_a = body_a, body_b = body_b, data = data,
      local_center_a = local_center_a, local_center_b = local_center_b,
      inv_mass_a = inv_mass_a, inv_i_a = inv_i_a, inv_mass_b = inv_mass_b,
      inv_i_b = inv_i_b }
    fun set_inv_i_a (r___ as ref (J { flags, typ, prev, next, edge_a, edge_b,
    body_a, body_b, data, local_center_a, local_center_b, inv_mass_a, inv_i_a =
    _, inv_mass_b, inv_i_b }), inv_i_a) =
      r___ := J { flags = flags, typ = typ, prev = prev, next = next, edge_a =
      edge_a, edge_b = edge_b, body_a = body_a, body_b = body_b, data = data,
      local_center_a = local_center_a, local_center_b = local_center_b,
      inv_mass_a = inv_mass_a, inv_i_a = inv_i_a, inv_mass_b = inv_mass_b,
      inv_i_b = inv_i_b }
    fun set_inv_mass_b (r___ as ref (J { flags, typ, prev, next, edge_a,
    edge_b, body_a, body_b, data, local_center_a, local_center_b, inv_mass_a,
    inv_i_a, inv_mass_b = _, inv_i_b }), inv_mass_b) =
      r___ := J { flags = flags, typ = typ, prev = prev, next = next, edge_a =
      edge_a, edge_b = edge_b, body_a = body_a, body_b = body_b, data = data,
      local_center_a = local_center_a, local_center_b = local_center_b,
      inv_mass_a = inv_mass_a, inv_i_a = inv_i_a, inv_mass_b = inv_mass_b,
      inv_i_b = inv_i_b }
    fun set_inv_i_b (r___ as ref (J { flags, typ, prev, next, edge_a, edge_b,
    body_a, body_b, data, local_center_a, local_center_b, inv_mass_a, inv_i_a,
    inv_mass_b, inv_i_b = _ }), inv_i_b) =
      r___ := J { flags = flags, typ = typ, prev = prev, next = next, edge_a =
      edge_a, edge_b = edge_b, body_a = body_a, body_b = body_b, data = data,
      local_center_a = local_center_a, local_center_b = local_center_b,
      inv_mass_a = inv_mass_a, inv_i_a = inv_i_a, inv_mass_b = inv_mass_b,
      inv_i_b = inv_i_b }
    fun new ({ flags, typ, prev, next, edge_a, edge_b, body_a, body_b, data,
    local_center_a, local_center_b, inv_mass_a, inv_i_a, inv_mass_b, inv_i_b })
    = 
      ref (J { flags = flags, typ = typ, prev = prev, next = next, edge_a =
      edge_a, edge_b = edge_b, body_a = body_a, body_b = body_b, data = data,
      local_center_a = local_center_a, local_center_b = local_center_b,
      inv_mass_a = inv_mass_a, inv_i_a = inv_i_a, inv_mass_b = inv_mass_b,
      inv_i_b = inv_i_b })
    val eq = op=
  end
  structure G =
  struct
    fun get_other (ref (G { other, ... })) = other
    fun get_joint (ref (G { joint, ... })) = joint
    fun get_prev (ref (G { prev, ... })) = prev
    fun get_next (ref (G { next, ... })) = next
    fun set_other (r___ as ref (G { other = _, joint, prev, next }), other) =
      r___ := G { other = other, joint = joint, prev = prev, next = next }
    fun set_joint (r___ as ref (G { other, joint = _, prev, next }), joint) =
      r___ := G { other = other, joint = joint, prev = prev, next = next }
    fun set_prev (r___ as ref (G { other, joint, prev = _, next }), prev) =
      r___ := G { other = other, joint = joint, prev = prev, next = next }
    fun set_next (r___ as ref (G { other, joint, prev, next = _ }), next) =
      r___ := G { other = other, joint = joint, prev = prev, next = next }
    fun new ({ other, joint, prev, next }) = 
      ref (G { other = other, joint = joint, prev = prev, next = next })
    val eq = op=
  end
  structure W =
  struct
    fun get_flags (ref (W { flags, ... })) = flags
    fun get_body_list (ref (W { body_list, ... })) = body_list
    fun get_joint_list (ref (W { joint_list, ... })) = joint_list
    fun get_body_count (ref (W { body_count, ... })) = body_count
    fun get_joint_count (ref (W { joint_count, ... })) = joint_count
    fun get_gravity (ref (W { gravity, ... })) = gravity
    fun get_allow_sleep (ref (W { allow_sleep, ... })) = allow_sleep
    fun get_ground_body (ref (W { ground_body, ... })) = ground_body
    fun get_goodbye_joint_hook (ref (W { goodbye_joint_hook, ... })) = goodbye_joint_hook
    fun get_goodbye_fixture_hook (ref (W { goodbye_fixture_hook, ... })) = goodbye_fixture_hook
    fun get_inv_dt0 (ref (W { inv_dt0, ... })) = inv_dt0
    fun get_warm_starting (ref (W { warm_starting, ... })) = warm_starting
    fun get_continuous_physics (ref (W { continuous_physics, ... })) = continuous_physics
    fun get_broad_phase (ref (W { broad_phase, ... })) = broad_phase
    fun get_contact_list (ref (W { contact_list, ... })) = contact_list
    fun get_contact_count (ref (W { contact_count, ... })) = contact_count
    fun get_should_collide (ref (W { should_collide, ... })) = should_collide
    fun get_begin_contact (ref (W { begin_contact, ... })) = begin_contact
    fun get_end_contact (ref (W { end_contact, ... })) = end_contact
    fun get_pre_solve (ref (W { pre_solve, ... })) = pre_solve
    fun get_post_solve (ref (W { post_solve, ... })) = post_solve
    fun set_flags (r___ as ref (W { flags = _, body_list, joint_list,
    body_count, joint_count, gravity, allow_sleep, ground_body,
    goodbye_joint_hook, goodbye_fixture_hook, inv_dt0, warm_starting,
    continuous_physics, broad_phase, contact_list, contact_count,
    should_collide, begin_contact, end_contact, pre_solve, post_solve }),
    flags) =
      r___ := W { flags = flags, body_list = body_list, joint_list =
      joint_list, body_count = body_count, joint_count = joint_count, gravity =
      gravity, allow_sleep = allow_sleep, ground_body = ground_body,
      goodbye_joint_hook = goodbye_joint_hook, goodbye_fixture_hook =
      goodbye_fixture_hook, inv_dt0 = inv_dt0, warm_starting = warm_starting,
      continuous_physics = continuous_physics, broad_phase = broad_phase,
      contact_list = contact_list, contact_count = contact_count,
      should_collide = should_collide, begin_contact = begin_contact,
      end_contact = end_contact, pre_solve = pre_solve, post_solve = post_solve
      }
    fun set_body_list (r___ as ref (W { flags, body_list = _, joint_list,
    body_count, joint_count, gravity, allow_sleep, ground_body,
    goodbye_joint_hook, goodbye_fixture_hook, inv_dt0, warm_starting,
    continuous_physics, broad_phase, contact_list, contact_count,
    should_collide, begin_contact, end_contact, pre_solve, post_solve }),
    body_list) =
      r___ := W { flags = flags, body_list = body_list, joint_list =
      joint_list, body_count = body_count, joint_count = joint_count, gravity =
      gravity, allow_sleep = allow_sleep, ground_body = ground_body,
      goodbye_joint_hook = goodbye_joint_hook, goodbye_fixture_hook =
      goodbye_fixture_hook, inv_dt0 = inv_dt0, warm_starting = warm_starting,
      continuous_physics = continuous_physics, broad_phase = broad_phase,
      contact_list = contact_list, contact_count = contact_count,
      should_collide = should_collide, begin_contact = begin_contact,
      end_contact = end_contact, pre_solve = pre_solve, post_solve = post_solve
      }
    fun set_joint_list (r___ as ref (W { flags, body_list, joint_list = _,
    body_count, joint_count, gravity, allow_sleep, ground_body,
    goodbye_joint_hook, goodbye_fixture_hook, inv_dt0, warm_starting,
    continuous_physics, broad_phase, contact_list, contact_count,
    should_collide, begin_contact, end_contact, pre_solve, post_solve }),
    joint_list) =
      r___ := W { flags = flags, body_list = body_list, joint_list =
      joint_list, body_count = body_count, joint_count = joint_count, gravity =
      gravity, allow_sleep = allow_sleep, ground_body = ground_body,
      goodbye_joint_hook = goodbye_joint_hook, goodbye_fixture_hook =
      goodbye_fixture_hook, inv_dt0 = inv_dt0, warm_starting = warm_starting,
      continuous_physics = continuous_physics, broad_phase = broad_phase,
      contact_list = contact_list, contact_count = contact_count,
      should_collide = should_collide, begin_contact = begin_contact,
      end_contact = end_contact, pre_solve = pre_solve, post_solve = post_solve
      }
    fun set_body_count (r___ as ref (W { flags, body_list, joint_list,
    body_count = _, joint_count, gravity, allow_sleep, ground_body,
    goodbye_joint_hook, goodbye_fixture_hook, inv_dt0, warm_starting,
    continuous_physics, broad_phase, contact_list, contact_count,
    should_collide, begin_contact, end_contact, pre_solve, post_solve }),
    body_count) =
      r___ := W { flags = flags, body_list = body_list, joint_list =
      joint_list, body_count = body_count, joint_count = joint_count, gravity =
      gravity, allow_sleep = allow_sleep, ground_body = ground_body,
      goodbye_joint_hook = goodbye_joint_hook, goodbye_fixture_hook =
      goodbye_fixture_hook, inv_dt0 = inv_dt0, warm_starting = warm_starting,
      continuous_physics = continuous_physics, broad_phase = broad_phase,
      contact_list = contact_list, contact_count = contact_count,
      should_collide = should_collide, begin_contact = begin_contact,
      end_contact = end_contact, pre_solve = pre_solve, post_solve = post_solve
      }
    fun set_joint_count (r___ as ref (W { flags, body_list, joint_list,
    body_count, joint_count = _, gravity, allow_sleep, ground_body,
    goodbye_joint_hook, goodbye_fixture_hook, inv_dt0, warm_starting,
    continuous_physics, broad_phase, contact_list, contact_count,
    should_collide, begin_contact, end_contact, pre_solve, post_solve }),
    joint_count) =
      r___ := W { flags = flags, body_list = body_list, joint_list =
      joint_list, body_count = body_count, joint_count = joint_count, gravity =
      gravity, allow_sleep = allow_sleep, ground_body = ground_body,
      goodbye_joint_hook = goodbye_joint_hook, goodbye_fixture_hook =
      goodbye_fixture_hook, inv_dt0 = inv_dt0, warm_starting = warm_starting,
      continuous_physics = continuous_physics, broad_phase = broad_phase,
      contact_list = contact_list, contact_count = contact_count,
      should_collide = should_collide, begin_contact = begin_contact,
      end_contact = end_contact, pre_solve = pre_solve, post_solve = post_solve
      }
    fun set_gravity (r___ as ref (W { flags, body_list, joint_list, body_count,
    joint_count, gravity = _, allow_sleep, ground_body, goodbye_joint_hook,
    goodbye_fixture_hook, inv_dt0, warm_starting, continuous_physics,
    broad_phase, contact_list, contact_count, should_collide, begin_contact,
    end_contact, pre_solve, post_solve }), gravity) =
      r___ := W { flags = flags, body_list = body_list, joint_list =
      joint_list, body_count = body_count, joint_count = joint_count, gravity =
      gravity, allow_sleep = allow_sleep, ground_body = ground_body,
      goodbye_joint_hook = goodbye_joint_hook, goodbye_fixture_hook =
      goodbye_fixture_hook, inv_dt0 = inv_dt0, warm_starting = warm_starting,
      continuous_physics = continuous_physics, broad_phase = broad_phase,
      contact_list = contact_list, contact_count = contact_count,
      should_collide = should_collide, begin_contact = begin_contact,
      end_contact = end_contact, pre_solve = pre_solve, post_solve = post_solve
      }
    fun set_allow_sleep (r___ as ref (W { flags, body_list, joint_list,
    body_count, joint_count, gravity, allow_sleep = _, ground_body,
    goodbye_joint_hook, goodbye_fixture_hook, inv_dt0, warm_starting,
    continuous_physics, broad_phase, contact_list, contact_count,
    should_collide, begin_contact, end_contact, pre_solve, post_solve }),
    allow_sleep) =
      r___ := W { flags = flags, body_list = body_list, joint_list =
      joint_list, body_count = body_count, joint_count = joint_count, gravity =
      gravity, allow_sleep = allow_sleep, ground_body = ground_body,
      goodbye_joint_hook = goodbye_joint_hook, goodbye_fixture_hook =
      goodbye_fixture_hook, inv_dt0 = inv_dt0, warm_starting = warm_starting,
      continuous_physics = continuous_physics, broad_phase = broad_phase,
      contact_list = contact_list, contact_count = contact_count,
      should_collide = should_collide, begin_contact = begin_contact,
      end_contact = end_contact, pre_solve = pre_solve, post_solve = post_solve
      }
    fun set_ground_body (r___ as ref (W { flags, body_list, joint_list,
    body_count, joint_count, gravity, allow_sleep, ground_body = _,
    goodbye_joint_hook, goodbye_fixture_hook, inv_dt0, warm_starting,
    continuous_physics, broad_phase, contact_list, contact_count,
    should_collide, begin_contact, end_contact, pre_solve, post_solve }),
    ground_body) =
      r___ := W { flags = flags, body_list = body_list, joint_list =
      joint_list, body_count = body_count, joint_count = joint_count, gravity =
      gravity, allow_sleep = allow_sleep, ground_body = ground_body,
      goodbye_joint_hook = goodbye_joint_hook, goodbye_fixture_hook =
      goodbye_fixture_hook, inv_dt0 = inv_dt0, warm_starting = warm_starting,
      continuous_physics = continuous_physics, broad_phase = broad_phase,
      contact_list = contact_list, contact_count = contact_count,
      should_collide = should_collide, begin_contact = begin_contact,
      end_contact = end_contact, pre_solve = pre_solve, post_solve = post_solve
      }
    fun set_goodbye_joint_hook (r___ as ref (W { flags, body_list, joint_list,
    body_count, joint_count, gravity, allow_sleep, ground_body,
    goodbye_joint_hook = _, goodbye_fixture_hook, inv_dt0, warm_starting,
    continuous_physics, broad_phase, contact_list, contact_count,
    should_collide, begin_contact, end_contact, pre_solve, post_solve }),
    goodbye_joint_hook) =
      r___ := W { flags = flags, body_list = body_list, joint_list =
      joint_list, body_count = body_count, joint_count = joint_count, gravity =
      gravity, allow_sleep = allow_sleep, ground_body = ground_body,
      goodbye_joint_hook = goodbye_joint_hook, goodbye_fixture_hook =
      goodbye_fixture_hook, inv_dt0 = inv_dt0, warm_starting = warm_starting,
      continuous_physics = continuous_physics, broad_phase = broad_phase,
      contact_list = contact_list, contact_count = contact_count,
      should_collide = should_collide, begin_contact = begin_contact,
      end_contact = end_contact, pre_solve = pre_solve, post_solve = post_solve
      }
    fun set_goodbye_fixture_hook (r___ as ref (W { flags, body_list,
    joint_list, body_count, joint_count, gravity, allow_sleep, ground_body,
    goodbye_joint_hook, goodbye_fixture_hook = _, inv_dt0, warm_starting,
    continuous_physics, broad_phase, contact_list, contact_count,
    should_collide, begin_contact, end_contact, pre_solve, post_solve }),
    goodbye_fixture_hook) =
      r___ := W { flags = flags, body_list = body_list, joint_list =
      joint_list, body_count = body_count, joint_count = joint_count, gravity =
      gravity, allow_sleep = allow_sleep, ground_body = ground_body,
      goodbye_joint_hook = goodbye_joint_hook, goodbye_fixture_hook =
      goodbye_fixture_hook, inv_dt0 = inv_dt0, warm_starting = warm_starting,
      continuous_physics = continuous_physics, broad_phase = broad_phase,
      contact_list = contact_list, contact_count = contact_count,
      should_collide = should_collide, begin_contact = begin_contact,
      end_contact = end_contact, pre_solve = pre_solve, post_solve = post_solve
      }
    fun set_inv_dt0 (r___ as ref (W { flags, body_list, joint_list, body_count,
    joint_count, gravity, allow_sleep, ground_body, goodbye_joint_hook,
    goodbye_fixture_hook, inv_dt0 = _, warm_starting, continuous_physics,
    broad_phase, contact_list, contact_count, should_collide, begin_contact,
    end_contact, pre_solve, post_solve }), inv_dt0) =
      r___ := W { flags = flags, body_list = body_list, joint_list =
      joint_list, body_count = body_count, joint_count = joint_count, gravity =
      gravity, allow_sleep = allow_sleep, ground_body = ground_body,
      goodbye_joint_hook = goodbye_joint_hook, goodbye_fixture_hook =
      goodbye_fixture_hook, inv_dt0 = inv_dt0, warm_starting = warm_starting,
      continuous_physics = continuous_physics, broad_phase = broad_phase,
      contact_list = contact_list, contact_count = contact_count,
      should_collide = should_collide, begin_contact = begin_contact,
      end_contact = end_contact, pre_solve = pre_solve, post_solve = post_solve
      }
    fun set_warm_starting (r___ as ref (W { flags, body_list, joint_list,
    body_count, joint_count, gravity, allow_sleep, ground_body,
    goodbye_joint_hook, goodbye_fixture_hook, inv_dt0, warm_starting = _,
    continuous_physics, broad_phase, contact_list, contact_count,
    should_collide, begin_contact, end_contact, pre_solve, post_solve }),
    warm_starting) =
      r___ := W { flags = flags, body_list = body_list, joint_list =
      joint_list, body_count = body_count, joint_count = joint_count, gravity =
      gravity, allow_sleep = allow_sleep, ground_body = ground_body,
      goodbye_joint_hook = goodbye_joint_hook, goodbye_fixture_hook =
      goodbye_fixture_hook, inv_dt0 = inv_dt0, warm_starting = warm_starting,
      continuous_physics = continuous_physics, broad_phase = broad_phase,
      contact_list = contact_list, contact_count = contact_count,
      should_collide = should_collide, begin_contact = begin_contact,
      end_contact = end_contact, pre_solve = pre_solve, post_solve = post_solve
      }
    fun set_continuous_physics (r___ as ref (W { flags, body_list, joint_list,
    body_count, joint_count, gravity, allow_sleep, ground_body,
    goodbye_joint_hook, goodbye_fixture_hook, inv_dt0, warm_starting,
    continuous_physics = _, broad_phase, contact_list, contact_count,
    should_collide, begin_contact, end_contact, pre_solve, post_solve }),
    continuous_physics) =
      r___ := W { flags = flags, body_list = body_list, joint_list =
      joint_list, body_count = body_count, joint_count = joint_count, gravity =
      gravity, allow_sleep = allow_sleep, ground_body = ground_body,
      goodbye_joint_hook = goodbye_joint_hook, goodbye_fixture_hook =
      goodbye_fixture_hook, inv_dt0 = inv_dt0, warm_starting = warm_starting,
      continuous_physics = continuous_physics, broad_phase = broad_phase,
      contact_list = contact_list, contact_count = contact_count,
      should_collide = should_collide, begin_contact = begin_contact,
      end_contact = end_contact, pre_solve = pre_solve, post_solve = post_solve
      }
    fun set_broad_phase (r___ as ref (W { flags, body_list, joint_list,
    body_count, joint_count, gravity, allow_sleep, ground_body,
    goodbye_joint_hook, goodbye_fixture_hook, inv_dt0, warm_starting,
    continuous_physics, broad_phase = _, contact_list, contact_count,
    should_collide, begin_contact, end_contact, pre_solve, post_solve }),
    broad_phase) =
      r___ := W { flags = flags, body_list = body_list, joint_list =
      joint_list, body_count = body_count, joint_count = joint_count, gravity =
      gravity, allow_sleep = allow_sleep, ground_body = ground_body,
      goodbye_joint_hook = goodbye_joint_hook, goodbye_fixture_hook =
      goodbye_fixture_hook, inv_dt0 = inv_dt0, warm_starting = warm_starting,
      continuous_physics = continuous_physics, broad_phase = broad_phase,
      contact_list = contact_list, contact_count = contact_count,
      should_collide = should_collide, begin_contact = begin_contact,
      end_contact = end_contact, pre_solve = pre_solve, post_solve = post_solve
      }
    fun set_contact_list (r___ as ref (W { flags, body_list, joint_list,
    body_count, joint_count, gravity, allow_sleep, ground_body,
    goodbye_joint_hook, goodbye_fixture_hook, inv_dt0, warm_starting,
    continuous_physics, broad_phase, contact_list = _, contact_count,
    should_collide, begin_contact, end_contact, pre_solve, post_solve }),
    contact_list) =
      r___ := W { flags = flags, body_list = body_list, joint_list =
      joint_list, body_count = body_count, joint_count = joint_count, gravity =
      gravity, allow_sleep = allow_sleep, ground_body = ground_body,
      goodbye_joint_hook = goodbye_joint_hook, goodbye_fixture_hook =
      goodbye_fixture_hook, inv_dt0 = inv_dt0, warm_starting = warm_starting,
      continuous_physics = continuous_physics, broad_phase = broad_phase,
      contact_list = contact_list, contact_count = contact_count,
      should_collide = should_collide, begin_contact = begin_contact,
      end_contact = end_contact, pre_solve = pre_solve, post_solve = post_solve
      }
    fun set_contact_count (r___ as ref (W { flags, body_list, joint_list,
    body_count, joint_count, gravity, allow_sleep, ground_body,
    goodbye_joint_hook, goodbye_fixture_hook, inv_dt0, warm_starting,
    continuous_physics, broad_phase, contact_list, contact_count = _,
    should_collide, begin_contact, end_contact, pre_solve, post_solve }),
    contact_count) =
      r___ := W { flags = flags, body_list = body_list, joint_list =
      joint_list, body_count = body_count, joint_count = joint_count, gravity =
      gravity, allow_sleep = allow_sleep, ground_body = ground_body,
      goodbye_joint_hook = goodbye_joint_hook, goodbye_fixture_hook =
      goodbye_fixture_hook, inv_dt0 = inv_dt0, warm_starting = warm_starting,
      continuous_physics = continuous_physics, broad_phase = broad_phase,
      contact_list = contact_list, contact_count = contact_count,
      should_collide = should_collide, begin_contact = begin_contact,
      end_contact = end_contact, pre_solve = pre_solve, post_solve = post_solve
      }
    fun set_should_collide (r___ as ref (W { flags, body_list, joint_list,
    body_count, joint_count, gravity, allow_sleep, ground_body,
    goodbye_joint_hook, goodbye_fixture_hook, inv_dt0, warm_starting,
    continuous_physics, broad_phase, contact_list, contact_count,
    should_collide = _, begin_contact, end_contact, pre_solve, post_solve }),
    should_collide) =
      r___ := W { flags = flags, body_list = body_list, joint_list =
      joint_list, body_count = body_count, joint_count = joint_count, gravity =
      gravity, allow_sleep = allow_sleep, ground_body = ground_body,
      goodbye_joint_hook = goodbye_joint_hook, goodbye_fixture_hook =
      goodbye_fixture_hook, inv_dt0 = inv_dt0, warm_starting = warm_starting,
      continuous_physics = continuous_physics, broad_phase = broad_phase,
      contact_list = contact_list, contact_count = contact_count,
      should_collide = should_collide, begin_contact = begin_contact,
      end_contact = end_contact, pre_solve = pre_solve, post_solve = post_solve
      }
    fun set_begin_contact (r___ as ref (W { flags, body_list, joint_list,
    body_count, joint_count, gravity, allow_sleep, ground_body,
    goodbye_joint_hook, goodbye_fixture_hook, inv_dt0, warm_starting,
    continuous_physics, broad_phase, contact_list, contact_count,
    should_collide, begin_contact = _, end_contact, pre_solve, post_solve }),
    begin_contact) =
      r___ := W { flags = flags, body_list = body_list, joint_list =
      joint_list, body_count = body_count, joint_count = joint_count, gravity =
      gravity, allow_sleep = allow_sleep, ground_body = ground_body,
      goodbye_joint_hook = goodbye_joint_hook, goodbye_fixture_hook =
      goodbye_fixture_hook, inv_dt0 = inv_dt0, warm_starting = warm_starting,
      continuous_physics = continuous_physics, broad_phase = broad_phase,
      contact_list = contact_list, contact_count = contact_count,
      should_collide = should_collide, begin_contact = begin_contact,
      end_contact = end_contact, pre_solve = pre_solve, post_solve = post_solve
      }
    fun set_end_contact (r___ as ref (W { flags, body_list, joint_list,
    body_count, joint_count, gravity, allow_sleep, ground_body,
    goodbye_joint_hook, goodbye_fixture_hook, inv_dt0, warm_starting,
    continuous_physics, broad_phase, contact_list, contact_count,
    should_collide, begin_contact, end_contact = _, pre_solve, post_solve }),
    end_contact) =
      r___ := W { flags = flags, body_list = body_list, joint_list =
      joint_list, body_count = body_count, joint_count = joint_count, gravity =
      gravity, allow_sleep = allow_sleep, ground_body = ground_body,
      goodbye_joint_hook = goodbye_joint_hook, goodbye_fixture_hook =
      goodbye_fixture_hook, inv_dt0 = inv_dt0, warm_starting = warm_starting,
      continuous_physics = continuous_physics, broad_phase = broad_phase,
      contact_list = contact_list, contact_count = contact_count,
      should_collide = should_collide, begin_contact = begin_contact,
      end_contact = end_contact, pre_solve = pre_solve, post_solve = post_solve
      }
    fun set_pre_solve (r___ as ref (W { flags, body_list, joint_list,
    body_count, joint_count, gravity, allow_sleep, ground_body,
    goodbye_joint_hook, goodbye_fixture_hook, inv_dt0, warm_starting,
    continuous_physics, broad_phase, contact_list, contact_count,
    should_collide, begin_contact, end_contact, pre_solve = _, post_solve }),
    pre_solve) =
      r___ := W { flags = flags, body_list = body_list, joint_list =
      joint_list, body_count = body_count, joint_count = joint_count, gravity =
      gravity, allow_sleep = allow_sleep, ground_body = ground_body,
      goodbye_joint_hook = goodbye_joint_hook, goodbye_fixture_hook =
      goodbye_fixture_hook, inv_dt0 = inv_dt0, warm_starting = warm_starting,
      continuous_physics = continuous_physics, broad_phase = broad_phase,
      contact_list = contact_list, contact_count = contact_count,
      should_collide = should_collide, begin_contact = begin_contact,
      end_contact = end_contact, pre_solve = pre_solve, post_solve = post_solve
      }
    fun set_post_solve (r___ as ref (W { flags, body_list, joint_list,
    body_count, joint_count, gravity, allow_sleep, ground_body,
    goodbye_joint_hook, goodbye_fixture_hook, inv_dt0, warm_starting,
    continuous_physics, broad_phase, contact_list, contact_count,
    should_collide, begin_contact, end_contact, pre_solve, post_solve = _ }),
    post_solve) =
      r___ := W { flags = flags, body_list = body_list, joint_list =
      joint_list, body_count = body_count, joint_count = joint_count, gravity =
      gravity, allow_sleep = allow_sleep, ground_body = ground_body,
      goodbye_joint_hook = goodbye_joint_hook, goodbye_fixture_hook =
      goodbye_fixture_hook, inv_dt0 = inv_dt0, warm_starting = warm_starting,
      continuous_physics = continuous_physics, broad_phase = broad_phase,
      contact_list = contact_list, contact_count = contact_count,
      should_collide = should_collide, begin_contact = begin_contact,
      end_contact = end_contact, pre_solve = pre_solve, post_solve = post_solve
      }
    fun new ({ flags, body_list, joint_list, body_count, joint_count, gravity,
    allow_sleep, ground_body, goodbye_joint_hook, goodbye_fixture_hook,
    inv_dt0, warm_starting, continuous_physics, broad_phase, contact_list,
    contact_count, should_collide, begin_contact, end_contact, pre_solve,
    post_solve }) = 
      ref (W { flags = flags, body_list = body_list, joint_list = joint_list,
      body_count = body_count, joint_count = joint_count, gravity = gravity,
      allow_sleep = allow_sleep, ground_body = ground_body, goodbye_joint_hook
      = goodbye_joint_hook, goodbye_fixture_hook = goodbye_fixture_hook,
      inv_dt0 = inv_dt0, warm_starting = warm_starting, continuous_physics =
      continuous_physics, broad_phase = broad_phase, contact_list =
      contact_list, contact_count = contact_count, should_collide =
      should_collide, begin_contact = begin_contact, end_contact = end_contact,
      pre_solve = pre_solve, post_solve = post_solve })
    val eq = op=
  end

end
