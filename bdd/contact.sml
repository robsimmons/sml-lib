(* Copyright 2010 Tom Murphy VII and Erin Catto. See COPYING for details. *)

(* Implementation of contacts.

   Corresponding to parts of dynamics/contacts/b2contact.cpp. *)
functor BDDBody(Arg : 
                sig
                  type fixture_data
                  type body_data
                end) : BDDBODY =
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

  type body = (body_data, fixture_data) D.bodycell ref
  type fixture = (body_data, fixture_data) D.fixturecell ref
  type contact = (body_data, fixture_data) D.contactcell ref
  type joint = unit
  type world = unit
  type filter = D.filter


  fun get_world_manifold (c : contact) =
      let
          val fix_a = D.C.get_fixture_a c
          val fix_b = D.C.get_fixture_b c
          val body_a = D.B.get_body fix_a
          val body_b = D.B.get_body fix_b
          val shape_a = D.B.get_shape fix_a
          val shape_b = D.B.get_shape fix_b
      in
            val initialize_manifold : BDDTypes.world_manifold *
                            BDDTypes.manifold * 
                            BDDMath.transform * real * 
                            BDDMath.transform * real -> unit

      end

end