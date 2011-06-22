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

  (* Insert at the given index, shifting anything
     after it to the right. The index may not be
     negative. Shifting takes time linear in the
     number of elements shifted. *)
  val insertat : 'a growarray -> int -> 'a -> unit

  (* converts the growarray to a regular array
     and clears the growarray.

     can raise subscript if the array has holes. *)
  val finalize : 'a growarray -> 'a Array.array
  val vector : 'a growarray -> 'a Vector.vector
  val tolist : 'a growarray -> 'a list

  val fromlist : 'a list -> 'a growarray

  (* Remove characters from the end so that it is the given length.
     Raises subscript if the array is not at least this long already. *)
  val truncate : 'a growarray -> int -> unit

  (* clear all elements, making it length 0 *)
  val clear : 'a growarray -> unit

  (* Apply to all non-missing elements in order *)
  val app : ('a -> unit) -> 'a growarray -> unit
  val appi : (int * 'a -> unit) -> 'a growarray -> unit

  (* Like the standard basis functions. Both ignore holes. *)
  val all : ('a -> bool) -> 'a growarray -> bool
  val exists : ('a -> bool) -> 'a growarray -> bool

  val copy : 'a growarray -> 'a growarray

  (* Erase the contents of the cell, replacing it with a hole. Reduces
     the length if this is the last element, which takes time
     proportional to the number of holes immediately before the last
     element. *)
  val erase : 'a growarray -> int -> unit

  (* put the element in the next empty cell, returning the index.
     Linear time. *)
  val update_next : 'a growarray -> 'a -> int

end