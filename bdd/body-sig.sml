(* Copyright 2010 Tom Murphy VII and Erin Catto. See COPYING for details. *)

(* Rigid bodies.

   Port note: I renamed functions like is_bullet to get_bullet to
   match with the style of the others, since there doesn't seem to be
   any advantage of irregularity.

   In BoxDiaDia this signature is used twice: Once transparently for
   the internal BDDBody structure, which clients should ignore, and
   once abstractly for the final client interface.

   Corresponding to dynamics/b2body.h. *)
signature BDDBODY =
sig

  (* Supplied by functor argument *)
  type fixture_data
  type body_data
  type joint_data

  type body
  type fixture
  type joint
  type world
  type contact
  type filter
  type contactedge
  type jointedge

  (* The body type.
     static: zero mass, zero velocity, may be manually moved
     kinematic: zero mass, non-zero velocity set by user, moved by solver
     dynamic: positive mass, non-zero velocity determined by forces, 
       moved by solver *)
  datatype body_type =
      Static
    | Kinematic
    | Dynamic

  (* Creates a fixture and attaches it to this body. Use this function
     if you need to set some fixture parameters, like friction.
     Otherwise you can use the simpler function below. If the
     density is non-zero, this function automatically updates the
     mass of the body. Contacts are not created until the next time
     step. May not be called during callbacks! *)
  val create_fixture : body *
    { (* Cloned. *)
      shape : BDDShape.shape,
      data : fixture_data,
      (* The friction coefficient, usually in the range [0, 1].
         Default value 0.2. *)
      friction : real,
      (* The restitution (elasticity) usually in the range [0, 1].
         0 is a perfectly inelastic fixture. *)
      restitution : real,
      (* The density, usually in kg/m^2. *)
      density : real,
      (* A sensor shape collects contact information but never
         generates a collision response. *)
      is_sensor : bool,
      (* Contact filtering data. *)
      filter : filter } -> fixture

  (* create_fixture_default (body, shape, data, density)
     Add a fixture based on a shape, with default values for the
     physical parameters above. *)
  val create_fixture_default : body * BDDShape.shape * fixture_data * real ->
                               fixture

  (* Destroy a fixture. This removes the fixture from the broad-phase
     and destroys all contacts associated with this fixture. This
     will automatically adjust the mass of the body if the body is
     dynamic and the fixture has positive density. All fixtures
     attached to a body are implicitly destroyed when the body is
     destroyed. May not be called during callbacks! *)
  (* XXX why not just do this from the fixture itself? It has a body ptr. *)
  val destroy_fixture : body * fixture -> unit

  (* set_transform (body, position, angle)
     Set the world position of the body's origin and its angle in
     radians. This breaks any contacts and wakes the other bodies.
     Manipulating a body's transform may cause non-physical
     behavior. *)
  val set_transform : body * BDDMath.vec2 * real -> unit

  (* Get the body transform for the body's origin. *)
  val get_transform : body -> BDDMath.transform

  (* Get the world body origin position. *)
  val get_position : body -> BDDMath.vec2

  (* Get the world rotation angle in radians. *)
  val get_angle : body -> real

  (* Get the world position of the center of mass. *)
  val get_world_center : body -> BDDMath.vec2

  (* Get the local position of the center of mass. *)
  val get_local_center : body -> BDDMath.vec2

  (* The linear velocity of the center of mass. *)
  val set_linear_velocity : body * BDDMath.vec2 -> unit
  val get_linear_velocity : body -> BDDMath.vec2

  (* The angular velocity in radians per second. *)
  val set_angular_velocity : body * real -> unit
  val get_angular_velocity : body -> real

  (* apply_force (body, force, point)
     Apply a force (in Newtons) at a world point. If the force is not
     applied at the center of mass, it will generate a torque and
     affect the angular velocity. Wakes up the body. *)
  val apply_force : body * BDDMath.vec2 * BDDMath.vec2 -> unit

  (* apply_torgue (body, torque)
     Apply a torque (in Newton meters). This affects the angular
     velocity without affecting the linear velocity of the center of
     mass. Wakes up the body. *)
  val apply_torque : body * real -> unit

  (* apply_linear_impulse (body, impulse, point)
     Apply an impulse (in Newton seconds = kilogram meters per
     second) at a world point. This immediately modifies the velocity. It
     also modifies the angular velocity if the point of application
     is not at the center of mass. Wakes up the body. *)
  val apply_linear_impulse : body * BDDMath.vec2 * BDDMath.vec2 -> unit

  (* apply_angular_impulse (body, impulse)
     Apply an angular impulse (in kg*m*m/s). *)
  val apply_angular_impulse : body * real -> unit

  (* Get the total mass of the body, in kilograms. *)
  val get_mass : body -> real

  (* Get the rotational inertia of the body about the local origin,
     in kg*m^2. *)
  val get_inertia : body -> real

  (* Get the mass data of the body. *)
  val get_mass_data : body -> BDDTypes.mass_data

  (* Set the mass properties to override the mass properties of the
     fixtures. This changes the center of mass position. Note that
     creating or destroying fixtures can also alter the mass. This
     function has no effect if the body isn't dynamic. *)
  val set_mass_data : body * BDDTypes.mass_data -> unit

  (* This resets the mass properties to the sum of the mass properties
     of the fixtures. This normally does not need to be called
     unless you called set_mass_data to override the mass and you
     later want to reset the mass. *)
  val reset_mass_data : body -> unit

  (* Get the world coordinates of a point given the local coordinates. *)
  val get_world_point : body * BDDMath.vec2 -> BDDMath.vec2

  (* Get the world coordinates of a vector given the local coordinates. *)
  val get_world_vector : body * BDDMath.vec2 -> BDDMath.vec2

  (* Gets a local point relative to the body's origin given a world point. *)
  val get_local_point : body * BDDMath.vec2 -> BDDMath.vec2

  (* Gets a local vector given a world vector. *)
  val get_local_vector : body * BDDMath.vec2 -> BDDMath.vec2

  (* Get the world linear velocity of a world point attached to this body. *)
  val get_linear_velocity_from_world_point : body * BDDMath.vec2 -> 
                                             BDDMath.vec2

  (* Get the world velocity of a local point. *)
  val get_linear_velocity_from_local_point : body * BDDMath.vec2 -> 
                                             BDDMath.vec2

  (* The linear damping of the body. *)
  val set_linear_damping : body * real -> unit
  val get_linear_damping : body -> real

  (* The angular damping of the body. *)
  val set_angular_damping : body * real -> unit
  val get_angular_damping : body -> real

  (* Set the type of this body. This may alter the mass and velocity. *)
  val set_type : body * body_type -> unit
  (* Get the type of this body. *)
  val get_type : body -> body_type

  (* Should this body be treated like a bullet for continuous collision
     detection? *)
  val set_bullet : body * bool -> unit
  val get_bullet : body -> bool

  (* You can disable sleeping on this body. If you disable sleeping, the
     body will be woken. *)
  val set_sleeping_allowed : body * bool -> unit
  val get_sleeping_allowed : body -> bool

  (* Set the sleep state of the body. A sleeping body has very
     low CPU cost. *)
  val set_awake : body * bool -> unit
  val get_awake : body -> bool

  (* Set the active state of the body. An inactive body is not
     simulated and cannot be collided with or woken up.
     If you pass a flag of true, all fixtures will be added to the
     broad-phase.
     If you pass a flag of false, all fixtures will be removed from
     the broad-phase and all contacts will be destroyed.
     Fixtures and joints are otherwise unaffected. You may continue
     to create/destroy fixtures and joints on inactive bodies.
     Fixtures on an inactive body are implicitly inactive and will
     not participate in collisions, ray-casts, or queries.
     Joints connected to an inactive body are implicitly inactive.
     An inactive body is still owned by a b2World object and remains
     in the body list. *)
  val set_active : body * bool -> unit
  val get_active : body -> bool

  (* A body with fixed rotation can change position but not angle.
     Setting this causes the mass data to be reset. *)
  val set_fixed_rotation : body * bool -> unit
  val get_fixed_rotation : body -> bool

  (* Get the list of all fixtures attached to this body.
     Use the next field of the fixture to iterate over them. *)
  val get_fixtures : body -> fixture option

  (* Get the list of all joints attached to this body.
     Use the next field of the joint edge to iterate over them. *)
  val get_joints : body -> jointedge option

  (* Get the list of all contacts attached to this body. This list
     changes during the time step so you may miss some collisions if
     you don't use a contact listener. *)
  val get_contact_list : body -> contactedge option

  (* Get the next body in the world's body list. *)
  val get_next : body -> body option

  (* The user data associated with the body. *)
  val get_data : body -> body_data
  val set_data : body * body_data -> unit

  val get_world : body -> world

end
