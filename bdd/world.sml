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

  structure Body = BDDBody(Arg)
  structure Fixture = BDDFixture(Arg)

  type body = BDDDynamics

  type world = unit
  type contact = unit
  type joint = unit


  type fixture = fixturecell ref
  type body = bodycell ref


  structure World =
  struct

    fun create_body (world : world,
        { typ : Body.body_type,
          position : BDDMath.vec2,
          angle : real,
          linear_velocity : BDDMath.vec2,
          angular_velocity : real,
          linear_damping : real,
          angular_damping : real,
          allow_sleep : bool,
          awake : bool,
          fixed_rotation : bool,
          bullet : bool,
          active : bool,
          data : body_data,
          inertia_scale : real }) : body =
        raise BDDWorld "unimplemented"
  
  end

end
