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
  type contact = unit
  type joint = unit
  type world = unit
  type filter = D.filter

  (* ... *)

end