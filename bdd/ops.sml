(* Copyright 2010 Tom Murphy VII and Erin Catto. See COPYING for details. *)

(* Convenience operators. Use with the infix declaration
   in the comment below, because Standard ML cannot export
   fixity. 
   No corresponding source in Box2d.
   *)

structure BDDOps =
struct

  (* These may seem a little crazy, but there's a plan. Each
     binary operator has ats its middle component a regular
     math operation like * for times. Its left and right
     parts are single-character mnemonics for the type of
     argument:
     
     : is a 2d vector (glyph has two parts)
     % is a 3d vector (three)
     + is a 2x2 matrix (glyph has four compartments)
     # is a 3x3 matrix (nine)
     
     (empty string) is a scalar, so that * is scalar
     times scalar.
     
     @ is a transform (rotation ideograph) *)

  val op :-: = BDDMath.vec2sub
  val op :+: = BDDMath.vec2add
  val op %-% = BDDMath.vec3sub
  val op %+% = BDDMath.vec3add
  val op +++ = BDDMath.mat22add

  val op  *: = BDDMath.vec2stimes
  val op  *% = BDDMath.vec3stimes
  val op +*: = BDDMath.mul22v
  val op +*+ = BDDMath.mul22m
  val op #*% = BDDMath.mul33v
  val op @*: = BDDMath.multransformv

(*
  infix 6 :+: :-: %-% %+% +++
  infix 7 *: *% +*: +*+ #*% @*:
*)

  (* other stuff you want everywhere. *)

  fun for lo hi f =
      if lo > hi then ()
      else (ignore (f lo); for (lo + 1) hi f)

  fun oapp next _ NONE = ()
    | oapp next (f : 'a -> unit) (SOME x) = (f x; oapp next f (next x))

end