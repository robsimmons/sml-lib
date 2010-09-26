(* Copyright 2010 Tom Murphy VII and Erin Catto. See COPYING for details. *)

(* A world is a collection of rigid bodies in simulation. Bodies are
   made up of fixtures, each of which is a shape with some physical
   properties like density and a coefficient of friction.

   XXX out of date; describe new strategy if it works
   Port note: I merged several different Box2D classes into this one
   module for BoxDiaDia, because they need to know about each other's
   implementations. They're arranged into sub-structures roughly
   corresponding to the original classes.

   I renamed functions like is_bullet to get_bullet to match with the
   style of the others, since there doesn't seem to be any advantage
   of irregularity.

   Corresponding to dynamics/b2world.h, dynamics/b2body.h, 
   dynamics/b2fixture.h. *)

(* The world is defined as a functor over the type of data associated
   with fixtures, bodies and joints, so that these don't have to be threaded
   through every function as ('fixture_data, 'body_data, 'joint_data). *)
signature BDDWORLD_ARG =
sig
  type fixture_data
  type body_data
  type joint_data
end

signature BDDWORLD =
sig

  (* Supplied by functor argument *)
  type fixture_data
  type body_data
  type joint_data

  (* A fixture is used to attach a shape to a body for collision
     detection. A fixture inherits its transform from its parent.
     Fixtures hold additional non-geometric data such as friction,
     collision filters, etc. Fixtures have identity and cannot be
     reused. *)
  type body
  type fixture
  type joint
  type world
  type contact
  type filter
  type contactedge

  structure Body : BDDBODY 
    where type body = body
      and type fixture = fixture
      and type joint = joint
      and type world = world
      and type contact = contact
      and type filter = filter
      and type contactedge = contactedge
      and type fixture_data = fixture_data
      and type body_data = body_data
      and type joint_data = joint_data

  structure Fixture : BDDFIXTURE 
    where type body = body
      and type fixture = fixture
      and type joint = joint
      and type world = world
      and type contact = contact
      and type filter = filter
      and type fixture_data = fixture_data
      and type body_data = body_data
      and type joint_data = joint_data

  (* XXX *)
  structure Joint : sig
    type joint_type = int
  
  end

  structure World :
  sig

    (* world gravity do_sleep

       Creates a new world with the given gravity vector. If do_sleep is true,
       then bodies are allowed to sleep. *)
    val world : BDDMath.vec2 * bool -> world

    val create_body : world *
      { (* The body type: static, kinematic, or dynamic.
           Note: if a dynamic body would have zero mass, the mass is
           set to one. *)
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

        (* Should this body be prevented from rotating? Useful for
           characters. *)
        fixed_rotation : bool,

        (* Is this a fast moving body that should be prevented from 
           tunneling through other moving bodies? Note that all bodies
           are prevented from tunneling through kinematic and static
           bodies. This setting is only considered on dynamic bodies.
           You should use this flag sparingly since it increases
           processing time. *)
        bullet : bool,

        (* Does this body start out active? *)
        active : bool,

        (* Use this to store application specific body data. *)
        data : body_data,

        (* Experimental: scales the inertia tensor.
           Default: 1.0 *)
        inertia_scale : real } -> body

    (* Destroy a rigid body. This function may not be used during
       callbacks. This automatically deletes all associated shapes and
       joints. *)
    val destroy_body : body -> unit

    (* Create a joint to constrain bodies together. No reference to the
       definition is retained. This may cause the connected bodies to
       cease colliding. This function may not be used during
       callbacks. 
       Port note: This argument was the structure b2JointDef, but this
       is the only place it's used.
       *)
    val create_joint : world * { typ : Joint.joint_type,
                                 (* Use this to attach application-specific data to
                                    your joints. *)
                                 user_data : joint_data,
                                 (* The first and second attached body. *)
                                 body_a : body,
                                 body_b : body,
                                 (* Set this flag to true if the attached bodies
                                    should collide. *)
                                 collide_connected : bool } -> joint

    (* Destroy a joint. This may cause the connected bodies to begin colliding.
       This function may not be used during callbacks. *)
    val destroy_joint : joint -> unit

    (* step world time_step velocity_iterations position_iterations
       Take a time step. This performs collision detection, integration,
       and constraint solution. The time_step parameter should not
       vary from call to call. (XXX take hints from manual about parameter
       values and put them here -twm) *)
    val step : world * real * int * int -> unit

    (* Manually clear forces (they are cleared automatically be default;
       see set_auto_clear_forces). Call this after you are done with
       time steps to clear the forces. You normally call this after
       each call to step, unless you are performing sub-steps. *)
    val clear_forces : world -> unit

    (* Query the world for all fixtures that potentially overlap the 
       provided axis-aligned bounding box. 
       The callback is called for each fixture found; if it returns
       false then the query is terminated.
       *)
    val query_aabb : world * (fixture -> bool) * BDDTypes.aabb -> unit

    (* Action that the raycast callback can take upon encountering a fixture.
       Port note: These were all encoded as special float values in Box2D. *)
    datatype raycast_action =
        IgnoreAndContinue
      | Terminate
        (* Clip to the fraction of the ray; must be in (0, 1). *)
      | Clip of real
      | Don'tClip

    val ray_cast : world * 
                   ({ fixture : fixture, point : BDDMath.vec2,
                      normal : BDDMath.vec2, fraction : real } -> raycast_action) *
                   BDDMath.vec2 * BDDMath.vec2 -> unit

    (* Get the head of the world body list. Body.get_next gives the next
       body in the list, if there is one. *)
    val get_body_list : world -> body option

    (* Get the head of the world joint list. Joint.get_next gives the next
       joint in the list, if there is one. *)
    val get_joint_list : world -> joint option

    (* Get the head of the world contacts list. Contact.get_next gives the
       next contact in the list, if there is one. *)
    val get_contact_list : world -> contact option

    (* Enable/disable warm starting, for testing. *)
    val set_warm_starting : world * bool -> unit

    (* Enable/disable continuous physics, for testing. *)
    val set_continuous_physics : world * bool -> unit

    (* Number of broad-phase proxies. *)
    val get_proxy_count : world -> int

    (* Number of bodies. *)
    val get_body_count : world -> int

    (* Number of joints. *)
    val get_joint_count : world -> int

    (* Number of contacts. Each may have 0 or more contact points. *)
    val get_contact_count : world -> int

    (* Change the global gravity vector. 
       XXX twm: Is the magnitude in m/s^2?
       *)
    val set_gravity : world * BDDMath.vec2 -> unit
    val get_gravity : world -> BDDMath.vec2

    (* Is the world licked (in the middle of a time step)? *)
    val is_locked : world -> bool

    (* Set flag to control automatic clearing of forces after each time step. *)
    val set_auto_clear_forces : world * bool -> unit
    val get_auto_clear_forces : world -> bool

    (* Port note: Corresponding to types in dynamics/b2worldcallbacks.h.
       I broke out the individual callbacks since this is easy in ML and
       it seems useful to have fine-grained control. *)

    (* Called when a joint or filter is about to be destroyed due to the
       destruction of an attached body. *)
    val set_goodbye_joint_hook : world * (joint -> unit) -> unit
    val set_goodbye_fixture_hook : world * (fixture -> unit) -> unit

    (* Set a contact filter to provide fine-grained control over which
       objects should be allowed to collide.
       
       The filter should return true if contact calculations should be
       performed between these two shapes. For performance reasons
       this is only called when the AABBs begin to overlap. *)
    val set_should_collide_filter : world * (fixture * fixture -> bool) -> unit

    (* Contact impulses for reporting. Impulses are used instead of 
       forces because sub-step forces may approach infinity for rigid 
       body collisions. These match up one-to-one with the contact points
       in b2Manifold. *)
    (* Each array has up to BDDSettings.max_manifold_points elements *)
    type contact_impulse = { normal_impulses : real array,
                             tangent_impulses : real array }

    (* Set these to get contact information. You can use these results for
       things like sounds and game logic. You can also get contact
       results by traversing the contact lists after the time step.
       However, you might miss some contacts because continuous
       physics leads to sub-stepping. Additionally you may receive
       multiple callbacks for the same contact in a single time step.
       You should strive to make your callbacks efficient because
       there may be many callbacks per time step.

       Warning: You cannot create/destroy entities inside these callbacks. *)

    (* Called when two fixtures begin to touch. *)
    val set_begin_contact : world * (contact -> unit) -> unit

    (* Called when two fixtures cease to touch. *)
    val set_end_contact : world * (contact -> unit) -> unit

    (* This is called after a contact is updated. This allows you to
       inspect a contact before it goes to the solver. If you are
       careful, you can modify the contact manifold (e.g. disable
       contact). A copy of the old manifold is provided so that you can
       detect changes. Notes: This is called only for awake bodies. It
       is called even when the number of contact points is zero. It is
       not called for sensors. If you set the number of contact points
       to zero, you will not get an EndContact callback. However, you
       may get a BeginContact callback the next step. *)
    val set_pre_solve : world * (contact * BDDTypes.manifold -> unit) -> unit

    (* This lets you inspect a contact after the solver is finished. This
       is useful for inspecting impulses. The contact manifold does
       not include time of impact impulses, which can be arbitrarily
       large if the sub-step is small. Hence the impulse is provided
       explicitly in a separate data structure. Note that this is only
       called for contacts that are touching, solid, and awake. *)
    val set_post_solve : world * (contact * contact_impulse -> unit) -> unit

    (* Port note: I didn't implement debug drawing, since it is obviously
       optional. *)



  end

end
