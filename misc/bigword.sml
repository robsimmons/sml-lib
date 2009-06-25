(* WordN for arbitrary positive N. 

   Contributed by Sean McLaughlin,
   July 2007. *)

(* 
 We use IntInf to represent large word sizes.  This creates some
 compilicatons.  In particular, IntInf.ints are represented in
 twos complement format, so we have to be careful when negative
 numbers arise.  We also must be sure to mask the upper bits
 for operations that may push bits outside the given word boundary.
*)

functor BigWord(val wordSize : int) :> WORD =
struct 

  structure I = IntInf

  type word = I.int

  infix ~>> << >>
  infixr 0 `
  fun f ` x = f x

  val wordSize = wordSize  

  val () = if wordSize < 1 then raise Fail "word size must be positive" else ()

  (* Get a word consisting of all 1s. *)
  fun get_mask n = I.pow(2,n) - 1
  val word_mask = get_mask wordSize
  val pow2n = I.pow(2,wordSize)
  val lots_of_1s = I.pow(2,3*wordSize) - 1

  (* Largest representable word at the given size *)
  val max_word = I.pow(2,wordSize) - 1

  (* Cut off the upper bits.  (equivalently, do mod(x,2^wordSize)) *)
  fun mask n = I.andb(word_mask,n)

  (* Return true iff the most significant bit is a 1. *)
  fun msb n = I.andb(I.pow(2,wordSize-1),n) > 0

  (* Consistency check for debugging. No 1s outside the word boundary. *)
  fun consistent n = 
      let
        val m = lots_of_1s - word_mask
      in
        I.andb(m,n) = 0
      end

  (* -------------------------------------------------------------------------- *)
  (*  Arithmetic                                                                *)
  (* -------------------------------------------------------------------------- *)

  (* Addition and multiplication can overflow, but we shouldn't have
     to worry about negative values since we assume the inputs are positive. *)
  fun x + y = mask(I.+(x,y))
  fun x * y = mask(I.*(x,y))

  (* For negation, we can't overflow, but we need to consider underflow. *)
  fun x - y = I.-(x,y) + (if x >= y then 0 else pow2n)

  (* Negation should just be mod 2^n *)
  fun ~ x = I.+(I.~ x,pow2n)
  
  (* for mod and div, if inputs are positive, no underflow.
     No overflow possible here either. *)
  fun x div y = I.div(x,y)    
  fun x mod y = I.mod(x,y)    

  (* comparisons should be ok, assuming positive inputs *)
  fun x < y = I.<(x,y)
  fun x > y = I.>(x,y)
  fun x <= y = I.<=(x,y)
  fun x >= y = I.>=(x,y)
  fun min(x,y) = I.min(x,y)
  fun max(x,y) = I.max(x,y)
  fun compare(x,y) = I.compare(x,y)

  (* -------------------------------------------------------------------------- *)
  (*  Logic                                                                     *)
  (* -------------------------------------------------------------------------- *)

  (* left shift can overflow, but not negate. *)
  fun x << y = mask(I.<<(x,y))

  (* because the input is positive, right shift should work correctly as well. 
     no over/underflow *)
  fun x >> y = I.~>>(x,y) 

  (* For arithmetic shift, check the msb, and do an orb mask. 
     No need to worry about overflow or underflow. *)
  fun x ~>> y = 
      let
        val msb = msb x
        val mask = 
            if msb 
            then word_mask - get_mask(Int.-(wordSize,Word.toInt y)) 
            else I.fromInt 0
      in
        I.orb(mask,I.~>>(x,y))
      end

  (* We don't need a mask for the logical ops. They should already be the right size. *)
  fun andb(x,y) = I.andb(x,y)
  fun orb(x,y) = I.orb(x,y)
  fun xorb x = I.xorb x
  (* I.notb is equivalent to ~(i + 1), so the resulting int is negative.  
     Thus, we need to hack the logical not. *)
  fun notb x = mask(I.xorb(x,lots_of_1s))

  (* -------------------------------------------------------------------------- *)
  (*  Conversions                                                               *)
  (* -------------------------------------------------------------------------- *)

  (* Assuming positivity, these functions have the same effect, and are safe. *)
  fun toLargeInt x = x
  (* XXX this is supposed to treat the bits as twos complement *)
  fun toLargeIntX x = x

  (* toLargeWord treats the nuber as positive.  So we're OK. *)
  fun toLargeWord x = LargeWord.fromLargeInt x

  (* toLargeWordX does twos complement, but since x is positive, should be the same behavior. *)
  fun toLargeWordX x = LargeWord.fromLargeInt x

  fun toInt x = LargeInt.toInt (toLargeInt x)
  fun toIntX x = LargeInt.toInt (toLargeIntX x)

  (* fromInt and fromLargeInt consider the int as unsigned. *)
  fun fromInt x = mask (LargeWord.toLargeInt (LargeWord.fromInt x))
  fun fromLargeInt x = mask (LargeWord.toLargeInt (LargeWord.fromLargeInt x))

  (* toLargeInt treats argument as unsigned *)
  fun fromLargeWord x = mask (LargeWord.toLargeInt x)

  fun fromLarge x = fromLargeWord x
  fun toLarge x = toLargeWord x
  fun toLargeX x = toLargeWordX x

  fun toString x = LargeWord.toString ` LargeWord.fromLargeInt x
  fun fromString x = case LargeWord.fromString x of
                       NONE => NONE
                     | SOME x' => SOME(LargeWord.toLargeInt x')

  fun scan x = raise Fail "scan: use LargeWord.scan"
  fun fmt x = raise Fail "fmt: use LargeWord.fmt"

end

