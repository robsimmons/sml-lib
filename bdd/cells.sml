(* Generated file. Do not edit! *)
structure BDDCells :> BDDCELLS =
struct
  datatype ('b, 'f, 'j) bodycell = B of {
    typ : (BDDDynamicsTypes.body_type) ref,
    flags : (Word8.word) ref,
    island_index : (int) ref,
    xf : (BDDMath.transform) ref,
    sweep : (BDDMath.sweep) ref,
    linear_velocity : (BDDMath.vec2) ref,
    angular_velocity : (real) ref,
    force : (BDDMath.vec2) ref,
    torque : (real) ref,
    world : (('b, 'f, 'j) worldcell) ref,
    prev : (('b, 'f, 'j) bodycell option) ref,
    next : (('b, 'f, 'j) bodycell option) ref,
    fixture_list : (('b, 'f, 'j) fixturecell option) ref,
    fixture_count : (int) ref,
    joint_list : (('b, 'f, 'j) jointedgecell option) ref,
    contact_list : (('b, 'f, 'j) contactedgecell option) ref,
    mass : (real) ref,
    inv_mass : (real) ref,
    i : (real) ref,
    inv_i : (real) ref,
    linear_damping : (real) ref,
    angular_damping : (real) ref,
    sleep_time : (real) ref,
    data : ('b) ref }

  and ('b, 'f, 'j) fixturecell = F of {
    aabb : (BDDTypes.aabb) ref,
    density : (real) ref,
    next : (('b, 'f, 'j) fixturecell option) ref,
    body : (('b, 'f, 'j) bodycell option) ref,
    shape : (BDDShape.shape) ref,
    friction : (real) ref,
    restitution : (real) ref,
    proxy : (('b, 'f, 'j) fixturecell BDDBroadPhase.proxy option) ref,
    filter : (BDDDynamicsTypes.filter) ref,
    sensor : (bool) ref,
    data : ('f) ref }

  and ('b, 'f, 'j) contactcell = C of {
    flags : (Word32.word) ref,
    prev : (('b, 'f, 'j) contactcell option) ref,
    next : (('b, 'f, 'j) contactcell option) ref,
    node_a : (('b, 'f, 'j) contactedgecell) ref,
    node_b : (('b, 'f, 'j) contactedgecell) ref,
    fixture_a : (('b, 'f, 'j) fixturecell) ref,
    fixture_b : (('b, 'f, 'j) fixturecell) ref,
    manifold : (BDDTypes.manifold) ref,
    toi_count : (int) ref }

  and ('b, 'f, 'j) contactedgecell = E of {
    other : (('b, 'f, 'j) bodycell option) ref,
    contact : (('b, 'f, 'j) contactcell option) ref,
    prev : (('b, 'f, 'j) contactedgecell option) ref,
    next : (('b, 'f, 'j) contactedgecell option) ref }

  and ('b, 'f, 'j) jointcell = J of {
    flags : (Word8.word) ref,
    typ : (BDDDynamicsTypes.joint_type) ref,
    prev : (('b, 'f, 'j) jointcell option) ref,
    next : (('b, 'f, 'j) jointcell option) ref,
    edge_a : (('b, 'f, 'j) jointedgecell) ref,
    edge_b : (('b, 'f, 'j) jointedgecell) ref,
    body_a : (('b, 'f, 'j) bodycell) ref,
    body_b : (('b, 'f, 'j) bodycell) ref,
    data : ('j) ref,
    local_center_a : (BDDMath.vec2) ref,
    local_center_b : (BDDMath.vec2) ref,
    inv_mass_a : (real) ref,
    inv_i_a : (real) ref,
    inv_mass_b : (real) ref,
    inv_i_b : (real) ref }

  and ('b, 'f, 'j) jointedgecell = G of {
    other : (('b, 'f, 'j) bodycell) ref,
    joint : (('b, 'f, 'j) jointcell) ref,
    prev : (('b, 'f, 'j) jointedgecell option) ref,
    next : (('b, 'f, 'j) jointedgecell option) ref }

  and ('b, 'f, 'j) worldcell = W of {
    flags : (Word32.word) ref,
    body_list : (('b, 'f, 'j) bodycell option) ref,
    joint_list : (('b, 'f, 'j) jointcell option) ref,
    body_count : (int) ref,
    joint_count : (int) ref,
    gravity : (BDDMath.vec2) ref,
    allow_sleep : (bool) ref,
    ground_body : (('b, 'f, 'j) bodycell option) ref,
    goodbye_joint_hook : (('b, 'f, 'j) jointcell -> unit) ref,
    goodbye_fixture_hook : (('b, 'f, 'j) fixturecell -> unit) ref,
    inv_dt0 : (real) ref,
    warm_starting : (bool) ref,
    continuous_physics : (bool) ref,
    broad_phase : (('b, 'f, 'j) fixturecell BDDBroadPhase.broadphase) ref,
    contact_list : (('b, 'f, 'j) contactcell option) ref,
    contact_count : (int) ref,
    should_collide : (('b, 'f, 'j) fixturecell * ('b, 'f, 'j) fixturecell -> bool) ref,
    begin_contact : (('b, 'f, 'j) contactcell -> unit) ref,
    end_contact : (('b, 'f, 'j) contactcell -> unit) ref,
    pre_solve : (('b, 'f, 'j) contactcell * BDDTypes.manifold -> unit) ref,
    post_solve : (('b, 'f, 'j) contactcell * BDDDynamicsTypes.contact_impulse -> unit) ref }

  type ('b, 'f, 'j) body = ('b, 'f, 'j) bodycell  type ('b, 'f, 'j) fixture = ('b, 'f, 'j) fixturecell  type ('b, 'f, 'j) contact = ('b, 'f, 'j) contactcell  type ('b, 'f, 'j) contactedge = ('b, 'f, 'j) contactedgecell  type ('b, 'f, 'j) joint = ('b, 'f, 'j) jointcell  type ('b, 'f, 'j) jointedge = ('b, 'f, 'j) jointedgecell  type ('b, 'f, 'j) world = ('b, 'f, 'j) worldcell
  structure B =
  struct
    fun get_typ (B { typ, ... }) = !typ
    fun get_flags (B { flags, ... }) = !flags
    fun get_island_index (B { island_index, ... }) = !island_index
    fun get_xf (B { xf, ... }) = !xf
    fun get_sweep (B { sweep, ... }) = !sweep
    fun get_linear_velocity (B { linear_velocity, ... }) = !linear_velocity
    fun get_angular_velocity (B { angular_velocity, ... }) = !angular_velocity
    fun get_force (B { force, ... }) = !force
    fun get_torque (B { torque, ... }) = !torque
    fun get_world (B { world, ... }) = !world
    fun get_prev (B { prev, ... }) = !prev
    fun get_next (B { next, ... }) = !next
    fun get_fixture_list (B { fixture_list, ... }) = !fixture_list
    fun get_fixture_count (B { fixture_count, ... }) = !fixture_count
    fun get_joint_list (B { joint_list, ... }) = !joint_list
    fun get_contact_list (B { contact_list, ... }) = !contact_list
    fun get_mass (B { mass, ... }) = !mass
    fun get_inv_mass (B { inv_mass, ... }) = !inv_mass
    fun get_i (B { i, ... }) = !i
    fun get_inv_i (B { inv_i, ... }) = !inv_i
    fun get_linear_damping (B { linear_damping, ... }) = !linear_damping
    fun get_angular_damping (B { angular_damping, ... }) = !angular_damping
    fun get_sleep_time (B { sleep_time, ... }) = !sleep_time
    fun get_data (B { data, ... }) = !data
    fun set_typ (B { typ, ... }, v) = typ := v
    fun set_flags (B { flags, ... }, v) = flags := v
    fun set_island_index (B { island_index, ... }, v) = island_index := v
    fun set_xf (B { xf, ... }, v) = xf := v
    fun set_sweep (B { sweep, ... }, v) = sweep := v
    fun set_linear_velocity (B { linear_velocity, ... }, v) = linear_velocity := v
    fun set_angular_velocity (B { angular_velocity, ... }, v) = angular_velocity := v
    fun set_force (B { force, ... }, v) = force := v
    fun set_torque (B { torque, ... }, v) = torque := v
    fun set_world (B { world, ... }, v) = world := v
    fun set_prev (B { prev, ... }, v) = prev := v
    fun set_next (B { next, ... }, v) = next := v
    fun set_fixture_list (B { fixture_list, ... }, v) = fixture_list := v
    fun set_fixture_count (B { fixture_count, ... }, v) = fixture_count := v
    fun set_joint_list (B { joint_list, ... }, v) = joint_list := v
    fun set_contact_list (B { contact_list, ... }, v) = contact_list := v
    fun set_mass (B { mass, ... }, v) = mass := v
    fun set_inv_mass (B { inv_mass, ... }, v) = inv_mass := v
    fun set_i (B { i, ... }, v) = i := v
    fun set_inv_i (B { inv_i, ... }, v) = inv_i := v
    fun set_linear_damping (B { linear_damping, ... }, v) = linear_damping := v
    fun set_angular_damping (B { angular_damping, ... }, v) = angular_damping := v
    fun set_sleep_time (B { sleep_time, ... }, v) = sleep_time := v
    fun set_data (B { data, ... }, v) = data := v
    fun new ({ typ, flags, island_index, xf, sweep, linear_velocity,
    angular_velocity, force, torque, world, prev, next, fixture_list,
    fixture_count, joint_list, contact_list, mass, inv_mass, i, inv_i,
    linear_damping, angular_damping, sleep_time, data }) = 
      B { typ = ref typ, flags = ref flags, island_index = ref island_index, xf
      = ref xf, sweep = ref sweep, linear_velocity = ref linear_velocity,
      angular_velocity = ref angular_velocity, force = ref force, torque = ref
      torque, world = ref world, prev = ref prev, next = ref next, fixture_list
      = ref fixture_list, fixture_count = ref fixture_count, joint_list = ref
      joint_list, contact_list = ref contact_list, mass = ref mass, inv_mass =
      ref inv_mass, i = ref i, inv_i = ref inv_i, linear_damping = ref
      linear_damping, angular_damping = ref angular_damping, sleep_time = ref
      sleep_time, data = ref data }
    fun eq (B { typ, ... }, B { typ = r___, ... }) =
        typ = r___
  end
  structure F =
  struct
    fun get_aabb (F { aabb, ... }) = !aabb
    fun get_density (F { density, ... }) = !density
    fun get_next (F { next, ... }) = !next
    fun get_body (F { body, ... }) = !body
    fun get_shape (F { shape, ... }) = !shape
    fun get_friction (F { friction, ... }) = !friction
    fun get_restitution (F { restitution, ... }) = !restitution
    fun get_proxy (F { proxy, ... }) = !proxy
    fun get_filter (F { filter, ... }) = !filter
    fun get_sensor (F { sensor, ... }) = !sensor
    fun get_data (F { data, ... }) = !data
    fun set_aabb (F { aabb, ... }, v) = aabb := v
    fun set_density (F { density, ... }, v) = density := v
    fun set_next (F { next, ... }, v) = next := v
    fun set_body (F { body, ... }, v) = body := v
    fun set_shape (F { shape, ... }, v) = shape := v
    fun set_friction (F { friction, ... }, v) = friction := v
    fun set_restitution (F { restitution, ... }, v) = restitution := v
    fun set_proxy (F { proxy, ... }, v) = proxy := v
    fun set_filter (F { filter, ... }, v) = filter := v
    fun set_sensor (F { sensor, ... }, v) = sensor := v
    fun set_data (F { data, ... }, v) = data := v
    fun new ({ aabb, density, next, body, shape, friction, restitution, proxy,
    filter, sensor, data }) = 
      F { aabb = ref aabb, density = ref density, next = ref next, body = ref
      body, shape = ref shape, friction = ref friction, restitution = ref
      restitution, proxy = ref proxy, filter = ref filter, sensor = ref sensor,
      data = ref data }
    fun eq (F { aabb, ... }, F { aabb = r___, ... }) =
        aabb = r___
  end
  structure C =
  struct
    fun get_flags (C { flags, ... }) = !flags
    fun get_prev (C { prev, ... }) = !prev
    fun get_next (C { next, ... }) = !next
    fun get_node_a (C { node_a, ... }) = !node_a
    fun get_node_b (C { node_b, ... }) = !node_b
    fun get_fixture_a (C { fixture_a, ... }) = !fixture_a
    fun get_fixture_b (C { fixture_b, ... }) = !fixture_b
    fun get_manifold (C { manifold, ... }) = !manifold
    fun get_toi_count (C { toi_count, ... }) = !toi_count
    fun set_flags (C { flags, ... }, v) = flags := v
    fun set_prev (C { prev, ... }, v) = prev := v
    fun set_next (C { next, ... }, v) = next := v
    fun set_node_a (C { node_a, ... }, v) = node_a := v
    fun set_node_b (C { node_b, ... }, v) = node_b := v
    fun set_fixture_a (C { fixture_a, ... }, v) = fixture_a := v
    fun set_fixture_b (C { fixture_b, ... }, v) = fixture_b := v
    fun set_manifold (C { manifold, ... }, v) = manifold := v
    fun set_toi_count (C { toi_count, ... }, v) = toi_count := v
    fun new ({ flags, prev, next, node_a, node_b, fixture_a, fixture_b,
    manifold, toi_count }) = 
      C { flags = ref flags, prev = ref prev, next = ref next, node_a = ref
      node_a, node_b = ref node_b, fixture_a = ref fixture_a, fixture_b = ref
      fixture_b, manifold = ref manifold, toi_count = ref toi_count }
    fun eq (C { flags, ... }, C { flags = r___, ... }) =
        flags = r___
  end
  structure E =
  struct
    fun get_other (E { other, ... }) = !other
    fun get_contact (E { contact, ... }) = !contact
    fun get_prev (E { prev, ... }) = !prev
    fun get_next (E { next, ... }) = !next
    fun set_other (E { other, ... }, v) = other := v
    fun set_contact (E { contact, ... }, v) = contact := v
    fun set_prev (E { prev, ... }, v) = prev := v
    fun set_next (E { next, ... }, v) = next := v
    fun new ({ other, contact, prev, next }) = 
      E { other = ref other, contact = ref contact, prev = ref prev, next = ref
      next }
    fun eq (E { other, ... }, E { other = r___, ... }) =
        other = r___
  end
  structure J =
  struct
    fun get_flags (J { flags, ... }) = !flags
    fun get_typ (J { typ, ... }) = !typ
    fun get_prev (J { prev, ... }) = !prev
    fun get_next (J { next, ... }) = !next
    fun get_edge_a (J { edge_a, ... }) = !edge_a
    fun get_edge_b (J { edge_b, ... }) = !edge_b
    fun get_body_a (J { body_a, ... }) = !body_a
    fun get_body_b (J { body_b, ... }) = !body_b
    fun get_data (J { data, ... }) = !data
    fun get_local_center_a (J { local_center_a, ... }) = !local_center_a
    fun get_local_center_b (J { local_center_b, ... }) = !local_center_b
    fun get_inv_mass_a (J { inv_mass_a, ... }) = !inv_mass_a
    fun get_inv_i_a (J { inv_i_a, ... }) = !inv_i_a
    fun get_inv_mass_b (J { inv_mass_b, ... }) = !inv_mass_b
    fun get_inv_i_b (J { inv_i_b, ... }) = !inv_i_b
    fun set_flags (J { flags, ... }, v) = flags := v
    fun set_typ (J { typ, ... }, v) = typ := v
    fun set_prev (J { prev, ... }, v) = prev := v
    fun set_next (J { next, ... }, v) = next := v
    fun set_edge_a (J { edge_a, ... }, v) = edge_a := v
    fun set_edge_b (J { edge_b, ... }, v) = edge_b := v
    fun set_body_a (J { body_a, ... }, v) = body_a := v
    fun set_body_b (J { body_b, ... }, v) = body_b := v
    fun set_data (J { data, ... }, v) = data := v
    fun set_local_center_a (J { local_center_a, ... }, v) = local_center_a := v
    fun set_local_center_b (J { local_center_b, ... }, v) = local_center_b := v
    fun set_inv_mass_a (J { inv_mass_a, ... }, v) = inv_mass_a := v
    fun set_inv_i_a (J { inv_i_a, ... }, v) = inv_i_a := v
    fun set_inv_mass_b (J { inv_mass_b, ... }, v) = inv_mass_b := v
    fun set_inv_i_b (J { inv_i_b, ... }, v) = inv_i_b := v
    fun new ({ flags, typ, prev, next, edge_a, edge_b, body_a, body_b, data,
    local_center_a, local_center_b, inv_mass_a, inv_i_a, inv_mass_b, inv_i_b })
    = 
      J { flags = ref flags, typ = ref typ, prev = ref prev, next = ref next,
      edge_a = ref edge_a, edge_b = ref edge_b, body_a = ref body_a, body_b =
      ref body_b, data = ref data, local_center_a = ref local_center_a,
      local_center_b = ref local_center_b, inv_mass_a = ref inv_mass_a, inv_i_a
      = ref inv_i_a, inv_mass_b = ref inv_mass_b, inv_i_b = ref inv_i_b }
    fun eq (J { flags, ... }, J { flags = r___, ... }) =
        flags = r___
  end
  structure G =
  struct
    fun get_other (G { other, ... }) = !other
    fun get_joint (G { joint, ... }) = !joint
    fun get_prev (G { prev, ... }) = !prev
    fun get_next (G { next, ... }) = !next
    fun set_other (G { other, ... }, v) = other := v
    fun set_joint (G { joint, ... }, v) = joint := v
    fun set_prev (G { prev, ... }, v) = prev := v
    fun set_next (G { next, ... }, v) = next := v
    fun new ({ other, joint, prev, next }) = 
      G { other = ref other, joint = ref joint, prev = ref prev, next = ref
      next }
    fun eq (G { other, ... }, G { other = r___, ... }) =
        other = r___
  end
  structure W =
  struct
    fun get_flags (W { flags, ... }) = !flags
    fun get_body_list (W { body_list, ... }) = !body_list
    fun get_joint_list (W { joint_list, ... }) = !joint_list
    fun get_body_count (W { body_count, ... }) = !body_count
    fun get_joint_count (W { joint_count, ... }) = !joint_count
    fun get_gravity (W { gravity, ... }) = !gravity
    fun get_allow_sleep (W { allow_sleep, ... }) = !allow_sleep
    fun get_ground_body (W { ground_body, ... }) = !ground_body
    fun get_goodbye_joint_hook (W { goodbye_joint_hook, ... }) = !goodbye_joint_hook
    fun get_goodbye_fixture_hook (W { goodbye_fixture_hook, ... }) = !goodbye_fixture_hook
    fun get_inv_dt0 (W { inv_dt0, ... }) = !inv_dt0
    fun get_warm_starting (W { warm_starting, ... }) = !warm_starting
    fun get_continuous_physics (W { continuous_physics, ... }) = !continuous_physics
    fun get_broad_phase (W { broad_phase, ... }) = !broad_phase
    fun get_contact_list (W { contact_list, ... }) = !contact_list
    fun get_contact_count (W { contact_count, ... }) = !contact_count
    fun get_should_collide (W { should_collide, ... }) = !should_collide
    fun get_begin_contact (W { begin_contact, ... }) = !begin_contact
    fun get_end_contact (W { end_contact, ... }) = !end_contact
    fun get_pre_solve (W { pre_solve, ... }) = !pre_solve
    fun get_post_solve (W { post_solve, ... }) = !post_solve
    fun set_flags (W { flags, ... }, v) = flags := v
    fun set_body_list (W { body_list, ... }, v) = body_list := v
    fun set_joint_list (W { joint_list, ... }, v) = joint_list := v
    fun set_body_count (W { body_count, ... }, v) = body_count := v
    fun set_joint_count (W { joint_count, ... }, v) = joint_count := v
    fun set_gravity (W { gravity, ... }, v) = gravity := v
    fun set_allow_sleep (W { allow_sleep, ... }, v) = allow_sleep := v
    fun set_ground_body (W { ground_body, ... }, v) = ground_body := v
    fun set_goodbye_joint_hook (W { goodbye_joint_hook, ... }, v) = goodbye_joint_hook := v
    fun set_goodbye_fixture_hook (W { goodbye_fixture_hook, ... }, v) = goodbye_fixture_hook := v
    fun set_inv_dt0 (W { inv_dt0, ... }, v) = inv_dt0 := v
    fun set_warm_starting (W { warm_starting, ... }, v) = warm_starting := v
    fun set_continuous_physics (W { continuous_physics, ... }, v) = continuous_physics := v
    fun set_broad_phase (W { broad_phase, ... }, v) = broad_phase := v
    fun set_contact_list (W { contact_list, ... }, v) = contact_list := v
    fun set_contact_count (W { contact_count, ... }, v) = contact_count := v
    fun set_should_collide (W { should_collide, ... }, v) = should_collide := v
    fun set_begin_contact (W { begin_contact, ... }, v) = begin_contact := v
    fun set_end_contact (W { end_contact, ... }, v) = end_contact := v
    fun set_pre_solve (W { pre_solve, ... }, v) = pre_solve := v
    fun set_post_solve (W { post_solve, ... }, v) = post_solve := v
    fun new ({ flags, body_list, joint_list, body_count, joint_count, gravity,
    allow_sleep, ground_body, goodbye_joint_hook, goodbye_fixture_hook,
    inv_dt0, warm_starting, continuous_physics, broad_phase, contact_list,
    contact_count, should_collide, begin_contact, end_contact, pre_solve,
    post_solve }) = 
      W { flags = ref flags, body_list = ref body_list, joint_list = ref
      joint_list, body_count = ref body_count, joint_count = ref joint_count,
      gravity = ref gravity, allow_sleep = ref allow_sleep, ground_body = ref
      ground_body, goodbye_joint_hook = ref goodbye_joint_hook,
      goodbye_fixture_hook = ref goodbye_fixture_hook, inv_dt0 = ref inv_dt0,
      warm_starting = ref warm_starting, continuous_physics = ref
      continuous_physics, broad_phase = ref broad_phase, contact_list = ref
      contact_list, contact_count = ref contact_count, should_collide = ref
      should_collide, begin_contact = ref begin_contact, end_contact = ref
      end_contact, pre_solve = ref pre_solve, post_solve = ref post_solve }
    fun eq (W { flags, ... }, W { flags = r___, ... }) =
        flags = r___
  end

end
