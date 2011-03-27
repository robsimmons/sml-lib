(* General-purpose utilities. *)
signature UTIL =
sig

  val both : ('a -> 'b) -> ('a -> 'c) -> 'a -> ('b * 'c)
  val doboth : ('a -> 'b) -> ('a -> 'c) -> 'a -> unit

  (* run f on every integer lo..hi inclusive *)
  val for  : int -> int -> (int -> 'a) -> unit
  val ford : int -> int -> 'a -> (int * 'a -> 'a) -> 'a

  datatype ('a, 'b) sum = A of 'a | B of 'b

  val sift : ('a -> ('curds, 'whey) sum) -> 'a list -> 'curds list * 'whey list
  val mapa : ('a -> 'c) -> ('a, 'b) sum -> ('c, 'b) sum
  val mapb : ('b -> 'c) -> ('a, 'b) sum -> ('a, 'c) sum

  val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
  val curry   : ('a * 'b -> 'c) -> 'a -> 'b -> 'c

  val uncurry3 : ('a -> 'b -> 'c -> 'd) -> 'a * 'b * 'c -> 'd
  val curry3   : ('a * 'b * 'c -> 'd) -> 'a -> 'b -> 'c -> 'd

  val uncurry4 : ('a -> 'b -> 'c -> 'd -> 'e) -> 'a * 'b * 'c * 'd -> 'e
  val curry4   : ('a * 'b * 'c * 'd -> 'e) -> 'a -> 'b -> 'c -> 'd -> 'e

  type 'a orderer = ('a * 'a) -> order
  val order_field : ('a -> 'b) -> 'b orderer -> 'a orderer

  (* pow n m   gives n^m, defining 0^0 to be 1 *)
  val pow : int -> int -> int

  val order_compare : order orderer
  val bool_compare : bool orderer
  val sum_compare : 'a orderer -> 'b orderer -> ('a, 'b) sum orderer

  (* Apply the orderer to the first or second element of a pair *)
  val byfirst  : 'a orderer -> ('a * 'b) orderer
  val bysecond : 'b orderer -> ('a * 'b) orderer

  (* Reverse the direction of any ordering *)
  val descending : 'a orderer -> 'a orderer

  val option_compare : 'a orderer -> 'a option orderer

  (* Generalization of 'a orderer -> 'b orderer -> ('a * 'b) orderer *)
  val lex_order : ('a * 'b -> order) -> ('c * 'd -> order) ->
                  (('a * 'c) * ('b * 'd) -> order)

  (* Generalization of 'a orderer -> 'a list orderer *)
  val lex_list_order : ('a * 'b -> order) -> ('a list * 'b list -> order)

  (* Generalization of 'a orderer -> 'a Vector.vector orderer *)
  val lex_vector_order : ('a * 'b -> order) -> 
                         ('a Vector.vector * 'b Vector.vector) -> order

  (* For example, on a record { b : bool, l : int list },
     lexicographic [order_field #b bool_compare,
                    order_field #l (lex_list_order Int.compare)]
     *)
  val lexicographic : 'a orderer list -> 'a orderer

  (* swap the order of curried arguments *)

  val c21 : ('a1 -> 'a2 -> 'b) ->
            ('a2 -> 'a1 -> 'b)

  val c132 : ('a1 -> 'a2 -> 'a3 -> 'b) ->
             ('a1 -> 'a3 -> 'a2 -> 'b)
  val c213 : ('a1 -> 'a2 -> 'a3 -> 'b) ->
             ('a2 -> 'a1 -> 'a3 -> 'b)
  val c231 : ('a1 -> 'a2 -> 'a3 -> 'b) ->
             ('a2 -> 'a3 -> 'a1 -> 'b)
  val c312 : ('a1 -> 'a2 -> 'a3 -> 'b) ->
             ('a3 -> 'a1 -> 'a2 -> 'b)
  val c321 : ('a1 -> 'a2 -> 'a3 -> 'b) ->
             ('a3 -> 'a2 -> 'a1 -> 'b)

  val c1243 : ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b) ->
              ('a1 -> 'a2 -> 'a4 -> 'a3 -> 'b)
  val c1324 : ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b) ->
              ('a1 -> 'a3 -> 'a2 -> 'a4 -> 'b)
  val c1342 : ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b) ->
              ('a1 -> 'a3 -> 'a4 -> 'a2 -> 'b)
  val c1423 : ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b) ->
              ('a1 -> 'a4 -> 'a2 -> 'a3 -> 'b)
  val c1432 : ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b) ->
              ('a1 -> 'a4 -> 'a3 -> 'a2 -> 'b)
  val c2134 : ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b) ->
              ('a2 -> 'a1 -> 'a3 -> 'a4 -> 'b)
  val c2143 : ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b) ->
              ('a2 -> 'a1 -> 'a4 -> 'a3 -> 'b)
  val c2314 : ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b) ->
              ('a2 -> 'a3 -> 'a1 -> 'a4 -> 'b)
  val c2341 : ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b) ->
              ('a2 -> 'a3 -> 'a4 -> 'a1 -> 'b)
  val c2413 : ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b) ->
              ('a2 -> 'a4 -> 'a1 -> 'a3 -> 'b)
  val c2431 : ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b) ->
              ('a2 -> 'a4 -> 'a3 -> 'a1 -> 'b)
  val c3124 : ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b) ->
              ('a3 -> 'a1 -> 'a2 -> 'a4 -> 'b)
  val c3142 : ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b) ->
              ('a3 -> 'a1 -> 'a4 -> 'a2 -> 'b)
  val c3214 : ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b) ->
              ('a3 -> 'a2 -> 'a1 -> 'a4 -> 'b)
  val c3241 : ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b) ->
              ('a3 -> 'a2 -> 'a4 -> 'a1 -> 'b)
  val c3412 : ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b) ->
              ('a3 -> 'a4 -> 'a1 -> 'a2 -> 'b)
  val c3421 : ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b) ->
              ('a3 -> 'a4 -> 'a2 -> 'a1 -> 'b)
  val c4123 : ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b) ->
              ('a4 -> 'a1 -> 'a2 -> 'a3 -> 'b)
  val c4132 : ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b) ->
              ('a4 -> 'a1 -> 'a3 -> 'a2 -> 'b)
  val c4213 : ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b) ->
              ('a4 -> 'a2 -> 'a1 -> 'a3 -> 'b)
  val c4231 : ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b) ->
              ('a4 -> 'a2 -> 'a3 -> 'a1 -> 'b)
  val c4312 : ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b) ->
              ('a4 -> 'a3 -> 'a1 -> 'a2 -> 'b)
  val c4321 : ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b) ->
              ('a4 -> 'a3 -> 'a2 -> 'a1 -> 'b)


  val is : ''a -> ''a -> bool

  (* useful combinators *)
  val I : 'a -> 'a
  val K : 'a -> 'b -> 'a

  (* for fold, etc. *)
  val opandalso : bool * bool -> bool
  val oporelse  : bool * bool -> bool

  (* Wrap a function so that a cleanup function is always called
     afterwards, whether it returns normally or with an exception. *)
  val protect : ('a -> 'b) -> (unit -> unit) -> 'a -> 'b

  (* always f cleanup
     Run the function f, and run cleanup after it, whether it returns
     normally or with an exception. *)
  val always : (unit -> 'a) -> (unit -> unit) -> 'a

  (* oneshot is a ref that can be set only once *)
  structure Oneshot :
  sig
      (* holding a value of type 'a *)
      type 'a oneshot
      (* create an uninitialized oneshot *)
      val oneshot     : unit -> 'a oneshot
      val init        : 'a   -> 'a oneshot
      val set         : 'a oneshot * 'a -> unit
      val deref       : 'a oneshot -> 'a option
      val eq          : 'a oneshot * 'a oneshot -> bool
      val wrap        : ('a -> 'a) -> 'a oneshot -> unit
  end

end
