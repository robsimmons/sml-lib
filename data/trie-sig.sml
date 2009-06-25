(* A trie is a data structure that efficiently stores a set of strings
   (over an arbitrary alphabet) with associated data. It allows efficient
   queries (or traversals) of the set on prefixes of strings. 

   The trie is built as a functor (TrieFn) over the argument signature
   TRIEARG below. The structure Trie is also supplied with char = Char.char
   and string = String.string, since this is a very common case *)

signature TRIE =
sig

    type char
    type string
    (* maps strings over char to 'a *)
    type 'a trie

    (* new empty trie *)
    val new : unit -> 'a trie

    (* retrieve the value at the root of the trie (empty string) *)
    val value : 'a trie -> 'a option

    (* same as insertwith #2 *)
    val insert : 'a trie -> string -> 'a -> unit
    (* insertwith combine t key value 
       inserts key into the trie t. If the key is not already present,
       associate the given value. If it is present with value 'old',
       associate the value combine(old, value).

       Insertion is approximately linear in the radix of char times
       the length of the string being inserted.
       *)
    val insertwith : ('a * 'a -> 'a) -> 'a trie -> string -> 'a -> unit

    (* Are there any children of this trie? Constant time. *)
    val haschildren : 'a trie -> bool
    (* Constant time. *)
    val child : 'a trie -> char -> 'a trie option

    (* All children. Linear in radix of char. *)
    val children : 'a trie -> (char * 'a trie) list
    (* Arbitrary order. *)
    val foldchildren : 'a trie -> 'b -> (char * 'a trie * 'b -> 'b) -> 'b
    val appchildren : 'a trie -> (char * 'a trie -> unit) -> unit

end

signature TRIEARG =
sig
    type char
    type string
        
    (* dense map of characters to integers *)
    val ord : char -> int
    val chr : int -> char

    (* enumerates characters of a string from beginning to end
       (usually left-to-right) *)
    val fold : (char * 'b -> 'b) -> 'b -> string -> 'b
end