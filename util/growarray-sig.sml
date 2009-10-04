signature GROWARRAY =
sig

  (* imperative arrays that automatically grow to
     accommodate new data. The array can have 'holes'
     where no data are stored, though these are not
     treated efficiently. *)
  type 'a growarray
  val eq : 'a growarray * 'a growarray -> bool

  val growarray : int -> 'a -> 'a growarray
  val empty : unit -> 'a growarray
  val tabulate : int -> (int -> 'a) -> 'a growarray

  (* return actual length, counting holes (position of last element + 1) *)
  val length : 'a growarray -> int

  (* can raise Subscript when out of bounds *)
  val sub : 'a growarray -> int -> 'a
  (* Is there an element at this position? 
     Raises subscript for negative indices. *)
  val has : 'a growarray -> int -> bool

  (* only raises subscript if index is negative. *)
  val update : 'a growarray -> int -> 'a -> unit

  (* stick an element at the end *)
  val append : 'a growarray -> 'a -> unit

  (* can raise subscript if the array has holes.
     
     after calling this, don't use the growarray
     any longer, since it may share data with the returned
     array. *)
  val finalize : 'a growarray -> 'a Array.array

  (* Remove characters from the end so that it is the given length.
     Raises subscript if the array is not at least this long already. *)
  val truncate : 'a growarray -> int -> unit

  (* clear all elements, making it length 0 *)
  val clear : 'a growarray -> unit

  (* Apply to all non-missing elements in order *)
  val app : ('a -> unit) -> 'a growarray -> unit
  val appi : (int * 'a -> unit) -> 'a growarray -> unit

end