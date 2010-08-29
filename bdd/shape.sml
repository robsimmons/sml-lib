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

  structure C = BDDCircle
  structure P = BDDPolygon

  datatype shape =
      Circle of C.circle
    | Polygon of P.polygon

  fun clone (Circle c) = Circle (C.clone c)
    | clone (Polygon p) = Polygon (P.clone p)

  fun test_point (Circle c, t, v) = C.test_point (c, t, v)
    | test_point (Polygon p, t, v) = P.test_point (p, t, v)

  fun ray_cast (Circle c, t, input) = C.ray_cast (c, t, input)
    | ray_cast (Polygon p, t, input) = P.ray_cast (p, t, input)

  fun compute_aabb (Circle c, t) = C.compute_aabb (c, t)
    | compute_aabb (Polygon p, t) = P.compute_aabb (p, t)

  fun compute_mass (Circle c, r) = C.compute_mass (c, r)
    | compute_mass (Polygon p, r) = P.compute_mass (p, r)

end
