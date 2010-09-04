(* Copyright 2010 Tom Murphy VII and Erin Catto. See COPYING for details. *)

(* A world is a collection of rigid bodies in simulation.
   Bodies are made up of fixtures, each of which is a shape with some physical
   properties like density and a coefficient of friction.

   Port note: I merged several different Box2D classes into this one module for
   BoxDiaDia, because they need to know about each other's implementations.
   They're arranged into sub-structures roughly corresponding to the original
   classes.

   Corresponding to dynamics/b2world.h, dynamics/b2body.h, dynamics/b2fixture.h. *)

(* The world is defined as a functor over the type of data associated with
   fixtures and bodies, so that these don't have to be threaded through every
   function as ('fixture_data, 'body_data). *)
signature BDDWORLD_ARG =
sig
  type fixture_data
  type body_data
end

signature BDDWORLD =
sig

  (* Supplied by functor argument *)
  type fixture_data
  type body_data

  (* A fixture is used to attach a shape to a body for collision detection. A fixture
     inherits its transform from its parent. Fixtures hold additional non-geometric data
     such as friction, collision filters, etc.
     Fixtures have identity and cannot be reused. *)
  type fixture

  type body

  type world

  structure Fixture :
  sig
      
    (* This holds contact filtering data. *)
    type filter

    val filter_mask : 
      { (* The collision category bits. Normally you would just set one bit. *)
        category_bits : Word16.word,

        (* The collision mask bits. This states the categories that this
           shape would accept for collision. *)
        mask_bits : Word16.word,

        (* Collision groups allow a certain group of objects to never collide (negative)
           or always collide (positive). Zero means no collision group. Non-zero group
           filtering always wins against the mask bits. *)
        group_index : int } -> filter

    (* Give the categories as a list of ints. Usually a fixture
       is just in one category. No category may be larger than 15
       or less than 0. *)
    val filter_list : { categories : int list,
                        mask : int list,
                        group_index : int } -> filter

    (* Get the child shape. You can modify the child shape, however you should not change the
       number of vertices because this will crash some collision caching mechanisms.
       Manipulating the shape may lead to non-physical behavior. *)
    val shape : fixture -> BDDShape.shape
    val set_shape : fixture * BDDShape.shape -> unit

    (* The sensor status of the fixture. *)
    val set_sensor : fixture * bool -> unit
    val is_sensor : fixture -> bool

    (* The contact filtering data. Setting will not update contacts until the next time
       step when either parent body is active and awake. *)
    val set_filter : fixture * filter -> unit
    val get_filter : fixture -> filter

    (* Get the parent body of this fixture. This is NONE if the fixture is not attached. *)
    val get_body : fixture -> body option

    (* Get the next fixture in the parent body's fixture list. *)
    val get_next : fixture -> fixture option (* XXX I assume? *)

    (* The user data that was assigned in the fixture definition. Use this to
       store your application specific data. *)
    val get_data : fixture -> fixture_data
    val set_data : fixture * fixture_data -> unit

    (* Test a point (in world coordinates) for containment in this fixture. *)
    val test_point : fixture * BDDMath.vec2 -> bool

    (* Cast a ray against this shape. *)
    val ray_cast : fixture * BDDTypes.ray_cast_input -> BDDTypes.ray_cast_output option

    (* Get the mass data for this fixture. The mass data is based on the density and
       the shape. The rotational inertia is about the shape's origin. This operation
       may be expensive. *)
    val get_mass_data : fixture -> BDDTypes.mass_data

    (* Density of this fixture. Setting will _not_ automatically adjust the mass
       of the body. You must call reset_mass_data to update the body's mass. *)
    val set_density : fixture * real -> unit
    val get_density : fixture -> real

    (* The coefficient of friction. *)
    val get_friction : fixture -> real
    val set_friction : fixture * real -> unit

    (* The coefficient of restitution. *)
    val get_restitution : fixture -> real
    val set_restitution : fixture * real -> unit

    (* Get the fixture's AABB. This AABB may be enlarge and/or stale.
       If you need a more accurate AABB, compute it using the shape and
       the body transform. *)
    val get_aabb : fixture -> BDDTypes.aabb
  end


  structure Body :
  sig

    (* The body type.
       static: zero mass, zero velocity, may be manually moved
       kinematic: zero mass, non-zero velocity set by user, moved by solver
       dynamic: positive mass, non-zero velocity determined by forces, moved by solver *)
    datatype body_type =
        Static
      | Kinematic
      | Dynamic

    val create_fixture : body *
      { (* Cloned. *)
        shape : BDDShape.shape,
        data : body_data,
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
        filter : Fixture.filter } -> fixture

  end

  structure World :
  sig

    val create_body : world *
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
        inertia_scale : real } -> body

  end

end
