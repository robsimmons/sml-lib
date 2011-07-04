(* Generated file. Do not edit! *)
signature BDDCELLS =
sig
  type ('b, 'f, 'j) body
  type ('b, 'f, 'j) fixture
  type ('b, 'f, 'j) contact
  type ('b, 'f, 'j) contactedge
  type ('b, 'f, 'j) joint
  type ('b, 'f, 'j) jointedge
  type ('b, 'f, 'j) world


  structure B :
  sig
    val get_typ : ('b, 'f, 'j) body -> (BDDDynamicsTypes.body_type)
    val get_flags : ('b, 'f, 'j) body -> (Word8.word)
    val get_island_index : ('b, 'f, 'j) body -> (int)
    val get_xf : ('b, 'f, 'j) body -> (BDDMath.transform)
    val get_sweep : ('b, 'f, 'j) body -> (BDDMath.sweep)
    val get_linear_velocity : ('b, 'f, 'j) body -> (BDDMath.vec2)
    val get_angular_velocity : ('b, 'f, 'j) body -> (real)
    val get_force : ('b, 'f, 'j) body -> (BDDMath.vec2)
    val get_torque : ('b, 'f, 'j) body -> (real)
    val get_world : ('b, 'f, 'j) body -> (('b, 'f, 'j) world)
    val get_prev : ('b, 'f, 'j) body -> (('b, 'f, 'j) body option)
    val get_next : ('b, 'f, 'j) body -> (('b, 'f, 'j) body option)
    val get_fixture_list : ('b, 'f, 'j) body -> (('b, 'f, 'j) fixture option)
    val get_fixture_count : ('b, 'f, 'j) body -> (int)
    val get_joint_list : ('b, 'f, 'j) body -> (('b, 'f, 'j) jointedge option)
    val get_contact_list : ('b, 'f, 'j) body -> (('b, 'f, 'j) contactedge option)
    val get_mass : ('b, 'f, 'j) body -> (real)
    val get_inv_mass : ('b, 'f, 'j) body -> (real)
    val get_i : ('b, 'f, 'j) body -> (real)
    val get_inv_i : ('b, 'f, 'j) body -> (real)
    val get_linear_damping : ('b, 'f, 'j) body -> (real)
    val get_angular_damping : ('b, 'f, 'j) body -> (real)
    val get_sleep_time : ('b, 'f, 'j) body -> (real)
    val get_data : ('b, 'f, 'j) body -> ('b)

    val set_typ : ('b, 'f, 'j) body * (BDDDynamicsTypes.body_type) -> unit
    val set_flags : ('b, 'f, 'j) body * (Word8.word) -> unit
    val set_island_index : ('b, 'f, 'j) body * (int) -> unit
    val set_xf : ('b, 'f, 'j) body * (BDDMath.transform) -> unit
    val set_sweep : ('b, 'f, 'j) body * (BDDMath.sweep) -> unit
    val set_linear_velocity : ('b, 'f, 'j) body * (BDDMath.vec2) -> unit
    val set_angular_velocity : ('b, 'f, 'j) body * (real) -> unit
    val set_force : ('b, 'f, 'j) body * (BDDMath.vec2) -> unit
    val set_torque : ('b, 'f, 'j) body * (real) -> unit
    val set_world : ('b, 'f, 'j) body * (('b, 'f, 'j) world) -> unit
    val set_prev : ('b, 'f, 'j) body * (('b, 'f, 'j) body option) -> unit
    val set_next : ('b, 'f, 'j) body * (('b, 'f, 'j) body option) -> unit
    val set_fixture_list : ('b, 'f, 'j) body * (('b, 'f, 'j) fixture option) -> unit
    val set_fixture_count : ('b, 'f, 'j) body * (int) -> unit
    val set_joint_list : ('b, 'f, 'j) body * (('b, 'f, 'j) jointedge option) -> unit
    val set_contact_list : ('b, 'f, 'j) body * (('b, 'f, 'j) contactedge option) -> unit
    val set_mass : ('b, 'f, 'j) body * (real) -> unit
    val set_inv_mass : ('b, 'f, 'j) body * (real) -> unit
    val set_i : ('b, 'f, 'j) body * (real) -> unit
    val set_inv_i : ('b, 'f, 'j) body * (real) -> unit
    val set_linear_damping : ('b, 'f, 'j) body * (real) -> unit
    val set_angular_damping : ('b, 'f, 'j) body * (real) -> unit
    val set_sleep_time : ('b, 'f, 'j) body * (real) -> unit
    val set_data : ('b, 'f, 'j) body * ('b) -> unit

    val new : {
      typ : BDDDynamicsTypes.body_type,
      flags : Word8.word,
      island_index : int,
      xf : BDDMath.transform,
      sweep : BDDMath.sweep,
      linear_velocity : BDDMath.vec2,
      angular_velocity : real,
      force : BDDMath.vec2,
      torque : real,
      world : ('b, 'f, 'j) world,
      prev : ('b, 'f, 'j) body option,
      next : ('b, 'f, 'j) body option,
      fixture_list : ('b, 'f, 'j) fixture option,
      fixture_count : int,
      joint_list : ('b, 'f, 'j) jointedge option,
      contact_list : ('b, 'f, 'j) contactedge option,
      mass : real,
      inv_mass : real,
      i : real,
      inv_i : real,
      linear_damping : real,
      angular_damping : real,
      sleep_time : real,
      data : 'b } -> ('b, 'f, 'j) body
    val eq : ('b, 'f, 'j) body * ('b, 'f, 'j) body -> bool
  end

  structure F :
  sig
    val get_aabb : ('b, 'f, 'j) fixture -> (BDDTypes.aabb)
    val get_density : ('b, 'f, 'j) fixture -> (real)
    val get_next : ('b, 'f, 'j) fixture -> (('b, 'f, 'j) fixture option)
    val get_body : ('b, 'f, 'j) fixture -> (('b, 'f, 'j) body option)
    val get_shape : ('b, 'f, 'j) fixture -> (BDDShape.shape)
    val get_friction : ('b, 'f, 'j) fixture -> (real)
    val get_restitution : ('b, 'f, 'j) fixture -> (real)
    val get_proxy : ('b, 'f, 'j) fixture -> (('b, 'f, 'j) fixture BDDBroadPhase.proxy option)
    val get_filter : ('b, 'f, 'j) fixture -> (BDDDynamicsTypes.filter)
    val get_sensor : ('b, 'f, 'j) fixture -> (bool)
    val get_data : ('b, 'f, 'j) fixture -> ('f)

    val set_aabb : ('b, 'f, 'j) fixture * (BDDTypes.aabb) -> unit
    val set_density : ('b, 'f, 'j) fixture * (real) -> unit
    val set_next : ('b, 'f, 'j) fixture * (('b, 'f, 'j) fixture option) -> unit
    val set_body : ('b, 'f, 'j) fixture * (('b, 'f, 'j) body option) -> unit
    val set_shape : ('b, 'f, 'j) fixture * (BDDShape.shape) -> unit
    val set_friction : ('b, 'f, 'j) fixture * (real) -> unit
    val set_restitution : ('b, 'f, 'j) fixture * (real) -> unit
    val set_proxy : ('b, 'f, 'j) fixture * (('b, 'f, 'j) fixture BDDBroadPhase.proxy option) -> unit
    val set_filter : ('b, 'f, 'j) fixture * (BDDDynamicsTypes.filter) -> unit
    val set_sensor : ('b, 'f, 'j) fixture * (bool) -> unit
    val set_data : ('b, 'f, 'j) fixture * ('f) -> unit

    val new : {
      aabb : BDDTypes.aabb,
      density : real,
      next : ('b, 'f, 'j) fixture option,
      body : ('b, 'f, 'j) body option,
      shape : BDDShape.shape,
      friction : real,
      restitution : real,
      proxy : ('b, 'f, 'j) fixture BDDBroadPhase.proxy option,
      filter : BDDDynamicsTypes.filter,
      sensor : bool,
      data : 'f } -> ('b, 'f, 'j) fixture
    val eq : ('b, 'f, 'j) fixture * ('b, 'f, 'j) fixture -> bool
  end

  structure C :
  sig
    val get_flags : ('b, 'f, 'j) contact -> (Word32.word)
    val get_prev : ('b, 'f, 'j) contact -> (('b, 'f, 'j) contact option)
    val get_next : ('b, 'f, 'j) contact -> (('b, 'f, 'j) contact option)
    val get_node_a : ('b, 'f, 'j) contact -> (('b, 'f, 'j) contactedge)
    val get_node_b : ('b, 'f, 'j) contact -> (('b, 'f, 'j) contactedge)
    val get_fixture_a : ('b, 'f, 'j) contact -> (('b, 'f, 'j) fixture)
    val get_fixture_b : ('b, 'f, 'j) contact -> (('b, 'f, 'j) fixture)
    val get_manifold : ('b, 'f, 'j) contact -> (BDDTypes.manifold)
    val get_toi_count : ('b, 'f, 'j) contact -> (int)

    val set_flags : ('b, 'f, 'j) contact * (Word32.word) -> unit
    val set_prev : ('b, 'f, 'j) contact * (('b, 'f, 'j) contact option) -> unit
    val set_next : ('b, 'f, 'j) contact * (('b, 'f, 'j) contact option) -> unit
    val set_node_a : ('b, 'f, 'j) contact * (('b, 'f, 'j) contactedge) -> unit
    val set_node_b : ('b, 'f, 'j) contact * (('b, 'f, 'j) contactedge) -> unit
    val set_fixture_a : ('b, 'f, 'j) contact * (('b, 'f, 'j) fixture) -> unit
    val set_fixture_b : ('b, 'f, 'j) contact * (('b, 'f, 'j) fixture) -> unit
    val set_manifold : ('b, 'f, 'j) contact * (BDDTypes.manifold) -> unit
    val set_toi_count : ('b, 'f, 'j) contact * (int) -> unit

    val new : {
      flags : Word32.word,
      prev : ('b, 'f, 'j) contact option,
      next : ('b, 'f, 'j) contact option,
      node_a : ('b, 'f, 'j) contactedge,
      node_b : ('b, 'f, 'j) contactedge,
      fixture_a : ('b, 'f, 'j) fixture,
      fixture_b : ('b, 'f, 'j) fixture,
      manifold : BDDTypes.manifold,
      toi_count : int } -> ('b, 'f, 'j) contact
    val eq : ('b, 'f, 'j) contact * ('b, 'f, 'j) contact -> bool
  end

  structure E :
  sig
    val get_other : ('b, 'f, 'j) contactedge -> (('b, 'f, 'j) body option)
    val get_contact : ('b, 'f, 'j) contactedge -> (('b, 'f, 'j) contact option)
    val get_prev : ('b, 'f, 'j) contactedge -> (('b, 'f, 'j) contactedge option)
    val get_next : ('b, 'f, 'j) contactedge -> (('b, 'f, 'j) contactedge option)

    val set_other : ('b, 'f, 'j) contactedge * (('b, 'f, 'j) body option) -> unit
    val set_contact : ('b, 'f, 'j) contactedge * (('b, 'f, 'j) contact option) -> unit
    val set_prev : ('b, 'f, 'j) contactedge * (('b, 'f, 'j) contactedge option) -> unit
    val set_next : ('b, 'f, 'j) contactedge * (('b, 'f, 'j) contactedge option) -> unit

    val new : {
      other : ('b, 'f, 'j) body option,
      contact : ('b, 'f, 'j) contact option,
      prev : ('b, 'f, 'j) contactedge option,
      next : ('b, 'f, 'j) contactedge option } -> ('b, 'f, 'j) contactedge
    val eq : ('b, 'f, 'j) contactedge * ('b, 'f, 'j) contactedge -> bool
  end

  structure J :
  sig
    val get_flags : ('b, 'f, 'j) joint -> (Word8.word)
    val get_typ : ('b, 'f, 'j) joint -> (BDDDynamicsTypes.joint_type)
    val get_prev : ('b, 'f, 'j) joint -> (('b, 'f, 'j) joint option)
    val get_next : ('b, 'f, 'j) joint -> (('b, 'f, 'j) joint option)
    val get_edge_a : ('b, 'f, 'j) joint -> (('b, 'f, 'j) jointedge)
    val get_edge_b : ('b, 'f, 'j) joint -> (('b, 'f, 'j) jointedge)
    val get_body_a : ('b, 'f, 'j) joint -> (('b, 'f, 'j) body)
    val get_body_b : ('b, 'f, 'j) joint -> (('b, 'f, 'j) body)
    val get_data : ('b, 'f, 'j) joint -> ('j)
    val get_local_center_a : ('b, 'f, 'j) joint -> (BDDMath.vec2)
    val get_local_center_b : ('b, 'f, 'j) joint -> (BDDMath.vec2)
    val get_inv_mass_a : ('b, 'f, 'j) joint -> (real)
    val get_inv_i_a : ('b, 'f, 'j) joint -> (real)
    val get_inv_mass_b : ('b, 'f, 'j) joint -> (real)
    val get_inv_i_b : ('b, 'f, 'j) joint -> (real)

    val set_flags : ('b, 'f, 'j) joint * (Word8.word) -> unit
    val set_typ : ('b, 'f, 'j) joint * (BDDDynamicsTypes.joint_type) -> unit
    val set_prev : ('b, 'f, 'j) joint * (('b, 'f, 'j) joint option) -> unit
    val set_next : ('b, 'f, 'j) joint * (('b, 'f, 'j) joint option) -> unit
    val set_edge_a : ('b, 'f, 'j) joint * (('b, 'f, 'j) jointedge) -> unit
    val set_edge_b : ('b, 'f, 'j) joint * (('b, 'f, 'j) jointedge) -> unit
    val set_body_a : ('b, 'f, 'j) joint * (('b, 'f, 'j) body) -> unit
    val set_body_b : ('b, 'f, 'j) joint * (('b, 'f, 'j) body) -> unit
    val set_data : ('b, 'f, 'j) joint * ('j) -> unit
    val set_local_center_a : ('b, 'f, 'j) joint * (BDDMath.vec2) -> unit
    val set_local_center_b : ('b, 'f, 'j) joint * (BDDMath.vec2) -> unit
    val set_inv_mass_a : ('b, 'f, 'j) joint * (real) -> unit
    val set_inv_i_a : ('b, 'f, 'j) joint * (real) -> unit
    val set_inv_mass_b : ('b, 'f, 'j) joint * (real) -> unit
    val set_inv_i_b : ('b, 'f, 'j) joint * (real) -> unit

    val new : {
      flags : Word8.word,
      typ : BDDDynamicsTypes.joint_type,
      prev : ('b, 'f, 'j) joint option,
      next : ('b, 'f, 'j) joint option,
      edge_a : ('b, 'f, 'j) jointedge,
      edge_b : ('b, 'f, 'j) jointedge,
      body_a : ('b, 'f, 'j) body,
      body_b : ('b, 'f, 'j) body,
      data : 'j,
      local_center_a : BDDMath.vec2,
      local_center_b : BDDMath.vec2,
      inv_mass_a : real,
      inv_i_a : real,
      inv_mass_b : real,
      inv_i_b : real } -> ('b, 'f, 'j) joint
    val eq : ('b, 'f, 'j) joint * ('b, 'f, 'j) joint -> bool
  end

  structure G :
  sig
    val get_other : ('b, 'f, 'j) jointedge -> (('b, 'f, 'j) body)
    val get_joint : ('b, 'f, 'j) jointedge -> (('b, 'f, 'j) joint)
    val get_prev : ('b, 'f, 'j) jointedge -> (('b, 'f, 'j) jointedge option)
    val get_next : ('b, 'f, 'j) jointedge -> (('b, 'f, 'j) jointedge option)

    val set_other : ('b, 'f, 'j) jointedge * (('b, 'f, 'j) body) -> unit
    val set_joint : ('b, 'f, 'j) jointedge * (('b, 'f, 'j) joint) -> unit
    val set_prev : ('b, 'f, 'j) jointedge * (('b, 'f, 'j) jointedge option) -> unit
    val set_next : ('b, 'f, 'j) jointedge * (('b, 'f, 'j) jointedge option) -> unit

    val new : {
      other : ('b, 'f, 'j) body,
      joint : ('b, 'f, 'j) joint,
      prev : ('b, 'f, 'j) jointedge option,
      next : ('b, 'f, 'j) jointedge option } -> ('b, 'f, 'j) jointedge
    val eq : ('b, 'f, 'j) jointedge * ('b, 'f, 'j) jointedge -> bool
  end

  structure W :
  sig
    val get_flags : ('b, 'f, 'j) world -> (Word32.word)
    val get_body_list : ('b, 'f, 'j) world -> (('b, 'f, 'j) body option)
    val get_joint_list : ('b, 'f, 'j) world -> (('b, 'f, 'j) joint option)
    val get_body_count : ('b, 'f, 'j) world -> (int)
    val get_joint_count : ('b, 'f, 'j) world -> (int)
    val get_gravity : ('b, 'f, 'j) world -> (BDDMath.vec2)
    val get_allow_sleep : ('b, 'f, 'j) world -> (bool)
    val get_ground_body : ('b, 'f, 'j) world -> (('b, 'f, 'j) body option)
    val get_goodbye_joint_hook : ('b, 'f, 'j) world -> (('b, 'f, 'j) joint -> unit)
    val get_goodbye_fixture_hook : ('b, 'f, 'j) world -> (('b, 'f, 'j) fixture -> unit)
    val get_inv_dt0 : ('b, 'f, 'j) world -> (real)
    val get_warm_starting : ('b, 'f, 'j) world -> (bool)
    val get_continuous_physics : ('b, 'f, 'j) world -> (bool)
    val get_broad_phase : ('b, 'f, 'j) world -> (('b, 'f, 'j) fixture BDDBroadPhase.broadphase)
    val get_contact_list : ('b, 'f, 'j) world -> (('b, 'f, 'j) contact option)
    val get_contact_count : ('b, 'f, 'j) world -> (int)
    val get_should_collide : ('b, 'f, 'j) world -> (('b, 'f, 'j) fixture * ('b, 'f, 'j) fixture -> bool)
    val get_begin_contact : ('b, 'f, 'j) world -> (('b, 'f, 'j) contact -> unit)
    val get_end_contact : ('b, 'f, 'j) world -> (('b, 'f, 'j) contact -> unit)
    val get_pre_solve : ('b, 'f, 'j) world -> (('b, 'f, 'j) contact * BDDTypes.manifold -> unit)
    val get_post_solve : ('b, 'f, 'j) world -> (('b, 'f, 'j) contact * BDDDynamicsTypes.contact_impulse -> unit)

    val set_flags : ('b, 'f, 'j) world * (Word32.word) -> unit
    val set_body_list : ('b, 'f, 'j) world * (('b, 'f, 'j) body option) -> unit
    val set_joint_list : ('b, 'f, 'j) world * (('b, 'f, 'j) joint option) -> unit
    val set_body_count : ('b, 'f, 'j) world * (int) -> unit
    val set_joint_count : ('b, 'f, 'j) world * (int) -> unit
    val set_gravity : ('b, 'f, 'j) world * (BDDMath.vec2) -> unit
    val set_allow_sleep : ('b, 'f, 'j) world * (bool) -> unit
    val set_ground_body : ('b, 'f, 'j) world * (('b, 'f, 'j) body option) -> unit
    val set_goodbye_joint_hook : ('b, 'f, 'j) world * (('b, 'f, 'j) joint -> unit) -> unit
    val set_goodbye_fixture_hook : ('b, 'f, 'j) world * (('b, 'f, 'j) fixture -> unit) -> unit
    val set_inv_dt0 : ('b, 'f, 'j) world * (real) -> unit
    val set_warm_starting : ('b, 'f, 'j) world * (bool) -> unit
    val set_continuous_physics : ('b, 'f, 'j) world * (bool) -> unit
    val set_broad_phase : ('b, 'f, 'j) world * (('b, 'f, 'j) fixture BDDBroadPhase.broadphase) -> unit
    val set_contact_list : ('b, 'f, 'j) world * (('b, 'f, 'j) contact option) -> unit
    val set_contact_count : ('b, 'f, 'j) world * (int) -> unit
    val set_should_collide : ('b, 'f, 'j) world * (('b, 'f, 'j) fixture * ('b, 'f, 'j) fixture -> bool) -> unit
    val set_begin_contact : ('b, 'f, 'j) world * (('b, 'f, 'j) contact -> unit) -> unit
    val set_end_contact : ('b, 'f, 'j) world * (('b, 'f, 'j) contact -> unit) -> unit
    val set_pre_solve : ('b, 'f, 'j) world * (('b, 'f, 'j) contact * BDDTypes.manifold -> unit) -> unit
    val set_post_solve : ('b, 'f, 'j) world * (('b, 'f, 'j) contact * BDDDynamicsTypes.contact_impulse -> unit) -> unit

    val new : {
      flags : Word32.word,
      body_list : ('b, 'f, 'j) body option,
      joint_list : ('b, 'f, 'j) joint option,
      body_count : int,
      joint_count : int,
      gravity : BDDMath.vec2,
      allow_sleep : bool,
      ground_body : ('b, 'f, 'j) body option,
      goodbye_joint_hook : ('b, 'f, 'j) joint -> unit,
      goodbye_fixture_hook : ('b, 'f, 'j) fixture -> unit,
      inv_dt0 : real,
      warm_starting : bool,
      continuous_physics : bool,
      broad_phase : ('b, 'f, 'j) fixture BDDBroadPhase.broadphase,
      contact_list : ('b, 'f, 'j) contact option,
      contact_count : int,
      should_collide : ('b, 'f, 'j) fixture * ('b, 'f, 'j) fixture -> bool,
      begin_contact : ('b, 'f, 'j) contact -> unit,
      end_contact : ('b, 'f, 'j) contact -> unit,
      pre_solve : ('b, 'f, 'j) contact * BDDTypes.manifold -> unit,
      post_solve : ('b, 'f, 'j) contact * BDDDynamicsTypes.contact_impulse -> unit } -> ('b, 'f, 'j) world
    val eq : ('b, 'f, 'j) world * ('b, 'f, 'j) world -> bool
  end

end
