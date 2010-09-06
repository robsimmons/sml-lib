(* Copyright 2010 Tom Murphy VII and Erin Catto. See COPYING for details. *)

(* Rigid bodies are made up of fixtures, which are shapes with some
   physical properties like density and friction.

   In BoxDiaDia this signature is used twice: Once transparently for
   the internal BDDBody structure, which clients should ignore, and
   once abstractly for the final client interface.

   Corresponding to dynamics/b2fixture.h. *)
signature BDDFIXTURE =
sig

  (* Supplied by functor argument *)
  type fixture_data
  type body_data


  type body
  type fixture
  type joint
  type world
  type contact

  (* This holds contact filtering data. *)
  type filter

  val filter_mask : 
    { (* The collision category bits. Normally you would just set one bit. *)
      category_bits : Word16.word,

      (* The collision mask bits. This states the categories that this shape
         would accept for collision. *)
      mask_bits : Word16.word,

      (* Collision groups allow a certain group of objects to never collide
         (negative) or always collide (positive). Zero means no
         collision group. Non-zero group filtering always wins
         against the mask bits. *)
      group_index : int } -> filter

  (* Give the categories as a list of ints. Usually a fixture
     is just in one category. No category may be larger than 15
     or less than 0. *)
  val filter_list : { categories : int list,
                      mask : int list,
                      group_index : int } -> filter

  (* Get the child shape. You can modify the child shape, however you
     should not change the number of vertices because this will
     crash some collision caching mechanisms. Manipulating the shape
     may lead to non-physical behavior. *)
  val shape : fixture -> BDDShape.shape
  val set_shape : fixture * BDDShape.shape -> unit

  (* The sensor status of the fixture. *)
  val set_sensor : fixture * bool -> unit
  val is_sensor : fixture -> bool

  (* The contact filtering data. Setting will not update contacts until
     the next time step when either parent body is active and awake.
     *)
  val set_filter : fixture * filter -> unit
  val get_filter : fixture -> filter

  (* Get the parent body of this fixture. This is NONE if the fixture is
     not attached. *)
  val get_body : fixture -> body option

  (* Get the next fixture in the parent body's fixture list. *)
  val get_next : fixture -> fixture option (* XXX I assume? *)

  (* The user data that was assigned in the fixture definition. Use this
     to store your application specific data. *)
  val get_data : fixture -> fixture_data
  val set_data : fixture * fixture_data -> unit

  (* Test a point (in world coordinates) for containment in this fixture. *)
  val test_point : fixture * BDDMath.vec2 -> bool

  (* Cast a ray against this shape. *)
  val ray_cast : fixture * BDDTypes.ray_cast_input -> 
      BDDTypes.ray_cast_output option

  (* Get the mass data for this fixture. The mass data is based on the
     density and the shape. The rotational inertia is about the
     shape's origin. This operation may be expensive. *)
  val get_mass_data : fixture -> BDDTypes.mass_data

  (* Density of this fixture. Setting will _not_ automatically adjust
     the mass of the body. You must call reset_mass_data to update
     the body's mass. *)
  val set_density : fixture * real -> unit
  val get_density : fixture -> real

  (* The coefficient of friction. *)
  val get_friction : fixture -> real
  val set_friction : fixture * real -> unit

  (* The coefficient of restitution. *)
  val get_restitution : fixture -> real
  val set_restitution : fixture * real -> unit

  (* Get the fixture's AABB. This AABB may be enlarge and/or stale. If
     you need a more accurate AABB, compute it using the shape and
     the body transform. *)
  val get_aabb : fixture -> BDDTypes.aabb

end
