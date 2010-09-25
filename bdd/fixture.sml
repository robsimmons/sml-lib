(* Copyright 2010 Tom Murphy VII and Erin Catto. See COPYING for details. *)

(* Implementation of body fixtures.

   Corresponding to parts of dynamics/b2fixture.cpp. *)
functor BDDFixture(Arg : 
                   sig
                     type fixture_data
                     type body_data
                     type joint_data
                   end) : BDDFIXTURE =
struct
  open Arg
  open BDDSettings
  open BDDTypes
  open BDDMath
  open BDDOps
  infix 6 :+: :-: %-% %+% +++
  infix 7 *: *% +*: +*+ #*% @*:

  exception BDDFixture of string

  structure D = BDDDynamics
  datatype fixturecell = datatype D.fixturecell
  structure DT = BDDDynamicsTypes(Arg)
  open DT

  type filter = D.filter
  open D.F

  fun filter_list { categories : int list,
                    mask : int list,
                    group_index : int } : filter =
      raise BDDFixture "unimplemented"

  fun filter_mask { category_bits : Word16.word,
                    mask_bits : Word16.word,
                    group_index : int } : filter =
      raise BDDFixture "unimplemented"

  fun fixture_transform f = D.B.get_xf (get_body f)
(*
      case get_body f of
          NONE => raise BDDFixture "fixture is not attached to a body."
        | SOME b => D.B.get_xf b
*)

  fun test_point (f, p : vec2) : bool =
      BDDShape.test_point (get_shape f, fixture_transform f, p)

  fun ray_cast (f, input) =
      BDDShape.ray_cast (get_shape f, fixture_transform f, input)

  fun get_mass_data f =
      BDDShape.compute_mass (get_shape f, get_density f)

  val is_sensor = get_sensor
  val shape = get_shape

  fun set_filter _ =
      raise BDDFixture "unimplemented"
(*
void b2Fixture::SetFilterData(const b2Filter& filter)
{
      m_filter = filter;

      if (m_body == NULL)
      {
              return;
      }

      // Flag associated contacts for filtering.
      b2ContactEdge* edge = m_body->GetContactList();
      while (edge)
      {
              b2Contact* contact = edge->contact;
              b2Fixture* fixtureA = contact->GetFixtureA();
              b2Fixture* fixtureB = contact->GetFixtureB();
              if (fixtureA == this || fixtureB == this)
              {
                      contact->FlagForFiltering();
              }

              edge = edge->next;
      }
}
*)

end
