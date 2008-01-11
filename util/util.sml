
structure Util :> UTIL =
struct

  fun uncurry f (a,b) = f a b
  fun curry f a b = f (a, b)

  fun uncurry3 f (a, b, c) = f a b c
  fun curry3 f a b c = f (a, b, c)

  fun uncurry4 f (a, b, c, d) = f a b c d
  fun curry4 f a b c d = f (a, b, c, d)

  fun both f g x = (f x, g x)
  fun doboth f g x = (ignore (f x); ignore (g x))

  fun for lo hi f =
      if lo > hi then ()
      else (ignore (f lo); for (lo + 1) hi f)

  fun ford lo hi b f =
      if lo > hi then b
      else (ford (lo + 1) hi (f (lo, b)) f)

  datatype ('a, 'b) sum = A of 'a | B of 'b

  type 'a orderer = ('a * 'a) -> order

  fun sum_compare ca cb (A a1, A a2) = ca (a1, a2)
    | sum_compare ca cb (B b1, B b2) = cb (b1, b2)
    | sum_compare _  _  (A _,  B _)  = LESS
    | sum_compare _  _  (B _,  A _)  = GREATER

  (* sort of hypocritical for order to not be ordered *)
  fun order_compare (LESS, LESS) = EQUAL
    | order_compare (LESS, _) = LESS
    | order_compare (_, LESS) = GREATER
    | order_compare (EQUAL, EQUAL) = EQUAL
    | order_compare (EQUAL, GREATER) = LESS
    | order_compare (GREATER, EQUAL) = GREATER
    | order_compare (GREATER, GREATER) = EQUAL


  fun bool_compare (false, false) = EQUAL
    | bool_compare (false, true) = LESS
    | bool_compare (true, false) = GREATER
    | bool_compare (true, true) = EQUAL

  fun mapa f (A x) = A (f x)
    | mapa f (B x) = B x

  fun mapb f (B x) = B (f x)
    | mapb f (A x) = A x
    (* interesting: this second case can't be

       ... | mapb f a = a

       because a changes type. *)

  fun sift f nil = (nil, nil)
    | sift f (h::t) =
    let val (aas, bbs) = sift f t
    in
      case f h of
        A a => (a :: aas, bbs)
      | B b => (aas, b :: bbs)
    end

  fun pow n 0 = 1
    | pow n m = n * pow n (m - 1)

  fun I x = x
  fun K x y = x

  fun opandalso (a, b) = a andalso b
  fun oporelse  (a, b) = a orelse b

  fun option_compare _ (NONE, NONE) = EQUAL
    | option_compare _ (SOME _, NONE) = GREATER
    | option_compare _ (NONE, SOME _) = LESS
    | option_compare f (SOME x, SOME y) = f(x, y)

  fun lex_order oa ob ((a, b), (aa, bb)) =
    (case oa (a, aa) of
       LESS => LESS
     | GREATER => GREATER
     | EQUAL => ob (b, bb))

  fun lex_list_order oi (nil, nil) = EQUAL
    | lex_list_order oi (nil, _ :: _) = LESS
    | lex_list_order oi (_ :: _, nil) = GREATER
    | lex_list_order oi (a :: al, b :: bl) =
    (case oi (a, b) of
       EQUAL => lex_list_order oi (al, bl)
     | neq => neq)

  fun c21 f a b = f b a


  fun c132 f a1 a3 a2 = f a1 a2 a3
  fun c213 f a2 a1 a3 = f a1 a2 a3
  fun c231 f a2 a3 a1 = f a1 a2 a3
  fun c312 f a3 a1 a2 = f a1 a2 a3
  fun c321 f a3 a2 a1 = f a1 a2 a3

  fun c1243 f a1 a2 a4 a3 = f a1 a2 a3 a4
  fun c1324 f a1 a3 a2 a4 = f a1 a2 a3 a4
  fun c1342 f a1 a3 a4 a2 = f a1 a2 a3 a4
  fun c1423 f a1 a4 a2 a3 = f a1 a2 a3 a4
  fun c1432 f a1 a4 a3 a2 = f a1 a2 a3 a4
  fun c2134 f a2 a1 a3 a4 = f a1 a2 a3 a4
  fun c2143 f a2 a1 a4 a3 = f a1 a2 a3 a4
  fun c2314 f a2 a3 a1 a4 = f a1 a2 a3 a4
  fun c2341 f a2 a3 a4 a1 = f a1 a2 a3 a4
  fun c2413 f a2 a4 a1 a3 = f a1 a2 a3 a4
  fun c2431 f a2 a4 a3 a1 = f a1 a2 a3 a4
  fun c3124 f a3 a1 a2 a4 = f a1 a2 a3 a4
  fun c3142 f a3 a1 a4 a2 = f a1 a2 a3 a4
  fun c3214 f a3 a2 a1 a4 = f a1 a2 a3 a4
  fun c3241 f a3 a2 a4 a1 = f a1 a2 a3 a4
  fun c3412 f a3 a4 a1 a2 = f a1 a2 a3 a4
  fun c3421 f a3 a4 a2 a1 = f a1 a2 a3 a4
  fun c4123 f a4 a1 a2 a3 = f a1 a2 a3 a4
  fun c4132 f a4 a1 a3 a2 = f a1 a2 a3 a4
  fun c4213 f a4 a2 a1 a3 = f a1 a2 a3 a4
  fun c4231 f a4 a2 a3 a1 = f a1 a2 a3 a4
  fun c4312 f a4 a3 a1 a2 = f a1 a2 a3 a4
  fun c4321 f a4 a3 a2 a1 = f a1 a2 a3 a4


  structure Oneshot =
  struct
      fun I x = x
      exception Oneshot of string

      (* A if unset (holds fn to apply)
         B if set (holds final value) *)
      type 'a oneshot = (('a -> 'a, 'a) sum) ref
      fun oneshot () = ref (A I)
      fun init a = ref (B a)
      fun set (ref (B _), _) = raise Oneshot "Oneshot already set!"
        | set (r as ref (A f), a) = r := B (f a)

      fun deref (ref (B b)) = SOME b
        | deref (ref (A _)) = NONE

      fun eq (a : 'a oneshot, b : 'a oneshot) = a = b

      fun wrap f os =
          case !os of
              B v => os := B (f v)
            | A g => os := A (f o g)
  end

end
