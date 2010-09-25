(* Copyright 2010 Tom Murphy VII and Erin Catto. See COPYING for details. *)

(* Implementation of contacts.

   Corresponding to parts of dynamics/contacts/b2contact.cpp. *)
functor BDDContact(Arg : 
                   sig
                     type fixture_data
                     type body_data
                     type joint_data
                   end) : BDDCONTACT =
struct
  open Arg
  open BDDSettings
  open BDDTypes
  open BDDMath
  open BDDOps
  infix 6 :+: :-: %-% %+% +++
  infix 7 *: *% +*: +*+ #*% @*:

  exception BDDBody of string
  

  structure D = BDDDynamics
  datatype bodycell = datatype D.bodycell
  datatype body_type = datatype D.body_type
  structure DT = BDDDynamicsTypes(Arg)
  open DT
  type filter = D.filter

  open D.C

  fun get_world_manifold (world_manifold, c : contact) =
      let
          val fix_a = D.C.get_fixture_a c
          val fix_b = D.C.get_fixture_b c
          val body_a = D.F.get_body fix_a
          val body_b = D.F.get_body fix_b
          val shape_a = D.F.get_shape fix_a
          val shape_b = D.F.get_shape fix_b
              
          val manifold = D.C.get_manifold c
      in
          BDDCollision.initialize_manifold
          (world_manifold, manifold, 
           D.B.get_xf body_a, BDDShape.get_radius shape_a,
           D.B.get_xf body_b, BDDShape.get_radius shape_b)
      end


end