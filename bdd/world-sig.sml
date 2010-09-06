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
   with fixtures and bodies, so that these don't have to be threaded
   through every function as ('fixture_data, 'body_data). *)
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

  structure Body : BDDBODY
  structure Fixture : BDDFIXTURE
  structure World :
  sig

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

  end

end
