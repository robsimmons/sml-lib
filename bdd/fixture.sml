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

  fun !! (SOME r) = r
    | !! NONE = raise BDDFixture
      ("Expected non-NONE reference; corresponds to an unchecked NULL " ^
       "dereference in Box2D. This is probably because an element " ^
       "(e.g. fixture, joint) was used after being detached, " ^
       "or before being initialized.")

  structure D = BDDDynamics
  datatype fixturecell = datatype D.fixturecell
  structure DT = BDDDynamicsTypes(Arg)
  open DT

  type filter = D.filter
  open D.F
  val set_filter = () (* overridden *)

  local fun mk16 nil = 0w0
          | mk16 (n :: rest) = Word16.orb (Word16.<< (0w1, Word.fromInt n), mk16 rest)
  in
      fun filter { category_bits : Word16.word,
                   mask_bits : Word16.word,
                   group_index : int } : filter =
          (Word32.orb (Word32.<< (Word32.fromInt (Word16.toInt category_bits), 0w16),
                       Word32.fromInt (Word16.toInt mask_bits)),
           group_index)

      fun filter_list { categories : int list,
                        mask : int list,
                        group_index : int } : filter =
          filter { category_bits = mk16 categories,
                   mask_bits = mk16 mask,
                   group_index = group_index }
  end


  fun filter_group_index (_, g) = g
  fun filter_category_bits (w, _) = Word16.fromInt (Word32.toInt (Word32.andb(Word32.>>(w, 0w16), 0wxFFFF)))
  fun filter_mask_bits (w, _) = Word16.fromInt (Word32.toInt (Word32.andb(w, 0wxFFFF)))

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

  fun set_filter (f : fixture, filter : D.filter) =
      let in
          D.F.set_filter (f, filter);
          (case !f of
               (* Flag associated contacts for filtering. *)
               F { body = SOME body, ... } =>
                   (oapp D.E.get_next 
                    (fn edge =>
                     let
                         val contact = !!(D.E.get_contact edge)
                         val fixture_a = D.C.get_fixture_a contact
                         val fixture_b = D.C.get_fixture_b contact
                     in
                         if fixture_a = f orelse fixture_b = f
                         then D.C.flag_for_filtering contact
                         else ()
                     end) (D.B.get_contact_list body))
             | _ => ())
      end

end
