(* Copyright 2010 Tom Murphy VII and Erin Catto. See COPYING for details. *)

(* Implement dispatch to the different shape types.
   Corresponding to collision/shapes/b2shape.cpp *)
structure BDDShape :> BDDSHAPE =
struct
  open BDDSettings
  open BDDTypes
  open BDDMath
  open BDDOps
  infix 6 :+: :-: %-% %+% +++
  infix 7 *: *% +*: +*+ #*% @*:

  datatype shape =
      Unknown
    | Circle of BDDCircle.circle
    | Polygon (* of XXX *)

  fun clone (Circle c) = Circle (BDDCircle.clone c)

  fun test_point (Circle c, t, v) = BDDCircle.test_point (c, t, v)

  fun ray_cast (Circle c, input, t) = BDDCircle.ray_cast (c, input, t)

  fun compute_aabb (Circle c, t) = BDDCircle.compute_aabb (c, t)

  fun compute_mass (Circle c, r) = BDDCircle.compute_mass (c, r)

end
