(* Copyright 2010 Tom Murphy VII and Erin Catto. See COPYING for details. *)

(* Fixtures are shapes with some physical properties, that are combined to
   form rigid bodies.
   Corresponding to dynamics/b2fixture.h. *)
signature BDDFIXTURE =
sig

  (* Port note: I've significantly simplified this structure to be more ML-like. *)

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

  type 'a fixture_def =
      { (* Cloned. *)
        shape : BDDShape.shape,
        data : 'a,
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

  (* A fixture is used to attach a shape to a body for collision detection. A fixture
     inherits its transform from its parent. Fixtures hold additional non-geometric data
     such as friction, collision filters, etc.
     Fixtures have identity and cannot be reused. *)
  type 'a fixture 
      
  (* Get the child shape. You can modify the child shape, however you should not change the
     number of vertices because this will crash some collision caching mechanisms.
     Manipulating the shape may lead to non-physical behavior. *)
  val shape : 'a fixture -> shape
  (* XXX should probably have set_shape *)

  (* The sensor status of the fixture. *)
  val set_sensor : 'a fixture * bool -> unit
  val is_sensor : 'a fixture -> bool

  
  (* The contact filtering data. Setting will not update contacts until the next time
     step when either parent body is active and awake. *)
  val set_filter : 'a fixture * filter -> unit
  val get_filter : 'a fixture -> filter

  (* Get the parent body of this fixture. This is NONE if the fixture is not attached. *)
  val get_body : 'a fixture -> body option

  (* Get the next fixture in the parent body's fixture list. *)
  val get_next : 'a fixture -> 'a fixture option (* XXX I assume? *)

  (* The user data that was assigned in the fixture definition. Use this to
     store your application specific data. *)
  val get_data : 'a fixture -> 'a
  val set_data : 'a fixture * 'a -> unit

  (* Test a point (in world coordinates) for containment in this fixture. *)
  val test_point : 'a fixture * BDDMath.vec2 -> bool

  (* Cast a ray against this shape. *)
  val ray_cast : 'a fixture * BDDTypes.ray_cast_input -> BDDTypes.ray_cast_output option

  (* Get the mass data for this fixture. The mass data is based on the density and
     the shape. The rotational inertia is about the shape's origin. This operation
     may be expensive. *)
  val get_mass_data : 'a fixture -> BDDTypes.mass_data

  (* Density of this fixture. Setting will _not_ automatically adjust the mass
     of the body. You must call reset_mass_data to update the body's mass. *)
  val set_density : 'a fixture * real -> unit
  val get_density : 'a fixture -> real

  (* The coefficient of friction. *)
  val get_friction : 'a fixture -> real
  val set_friction : 'a fixture * real -> unit

  (* The coefficient of restitution. *)
  val get_restitution : 'a fixture -> real
  val set_restitution : 'a fixture * real -> unit

  (* Get the fixture's AABB. This AABB may be enlarge and/or stale.
     If you need a more accurate AABB, compute it using the shape and
     the body transform. *)
  val get_aabb : 'a fixture -> BDDTypes.aabb

end
