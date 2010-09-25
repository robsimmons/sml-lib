(* Copyright 2010 Tom Murphy VII and Erin Catto. See COPYING for details. *)

(* Disjoint union over the implemented shapes. The Box2D code does not
   use the shape class for abstraction, just dispatch. So this is basically
   what's going on there, anyway.
   Corresponding to collision/shapes/b2shape.h *)
signature BDDSHAPE =
sig

  datatype shape =
      Circle of BDDCircle.circle
    | Polygon of BDDPolygon.polygon

  val clone : shape -> shape

      
  (* test_point (shape, xf, p)
     Test a point p for containment in this shape, under the
     world transform xf. This only works for convex shapes. *)
  val test_point : shape * BDDMath.transform * BDDMath.vec2 -> bool

  (* ray_cast (shape, input, transform)
     Cast a ray against this shape, under the transform. *)
  val ray_cast : shape * BDDMath.transform * BDDTypes.ray_cast_input -> 
                 BDDTypes.ray_cast_output option

  (* compute_aabb (shape, transform)
     Compute the associated axis aligned bounding box for this shape,
     under the transform. *)
  val compute_aabb : shape * BDDMath.transform -> BDDTypes.aabb

  (* compute_mass (shape, density)

     Compute the mass properties of this shape using its dimensions and density
     (measured in kilograms per meter squared).
     The inertia tensor is computed about the local origin. *)
  val compute_mass : shape * real -> BDDTypes.mass_data

  (* Returns the radius of the shape. 

     Port note: This is implemented as a member access in Box2D. *)
  val get_radius : shape -> real

end