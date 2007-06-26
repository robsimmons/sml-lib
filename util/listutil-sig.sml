
signature LISTUTIL =
sig

    exception ListUtil

    (* Association lists *)
    structure Alist : 
        sig
            val find : ('a * 'a -> bool) -> ('a * 'b) list -> 'a -> 'b option
            val get : ('a * 'a -> bool) -> ('a * 'b) list -> 'a -> 
                         ('a * 'b) option

            (* return also the rest of the list *)
            val extract : ('a * 'a -> bool) -> ('a * 'b) list -> 'a ->
                                            ('b * ('a * 'b) list) option 

            val haskey : ('a * 'a -> bool) -> ('a * 'b) list -> 'a -> bool

            val removeall : ('a * 'a -> bool) -> 
                            ('a * 'b) list -> 'a -> ('a * 'b) list
            val removefirst : ('a * 'a -> bool) -> 
                              ('a * 'b) list -> 'a -> ('a * 'b) list

            val bycompare : ('a * 'a -> order) -> ('a * 'a) -> bool

            val update : ('a * 'a -> bool) -> ('a * 'b) list -> 'a -> 
                            'b -> ('a * 'b) list

            val modify : ('a * 'a -> bool) -> ('a * 'b) list -> 'a -> 
                            ('b -> 'b) -> ('a * 'b) list

            val swap : ('a * 'b) list -> ('b * 'a) list
        end

    structure Sorted :
        sig
            (* insert in ascending order *)
            val insert : ('a * 'a -> order) -> 'a list -> 'a -> 'a list

            (* reverse a sorting function *)
            val reverse : ('a * 'a -> order) -> 'a * 'a -> order

            (* insertbest max cmp l a
               inserts a into l, but ensures that the
               resulting list has at most 'max' elements (by dropping
               elements from the end) *)
            val insertbest : int -> ('a * 'a -> order) -> 'a list -> 'a ->
                              'a list

            (* ... *)
        end

    val stratify : ('a * 'a -> order) -> ('a * 'b) list -> ('a * 'b list) list

    val combiner : ('a * 'a -> 'a) -> 'a list -> 'a
    val combinel : ('a * 'a -> 'a) -> 'a list -> 'a

    val list : 'a -> 'a list

    (* choosemap f l
       
       returns [f(l1, [l2...ln]), f(l2, [l1,l3...ln]), ...] *)
    val choosemap : ('a * 'a list -> 'b) -> 'a list -> 'b list

    (* returns elements satisfying predicate until first failure *)
    val aslongas : ('a -> bool) -> 'a list -> 'a list

    (* return the first success and the entire list behind it.
       nil if nothing satisfies it. *)
    val after : ('a -> bool) -> 'a list -> 'a list

    (* return all that satisfy until the first failure, 
       and separately the rest of the list *)
    val partitionaslongas : ('a -> bool) -> 'a list -> 'a list * 'a list

    (* position f l
       returns smallest SOME n such that f (List.nth (l, n)) = true,
       (that's 0 based), or NONE if no such element exists. *)
    val position : ('a -> bool) -> 'a list -> int option

    (* unlike ListPair.zip, insists that they are the same length
       or else raises ListPair. *)
    val wed : 'a list -> 'b list -> ('a * 'b) list

    (* all2 f a b
       true iff a and b are the same length and f(a,b) for corresponding
       pairs. NOT the same as ListPair.all, which does not require that
       a and b be the same length. *)
    val all2 : ('a * 'b -> bool) -> 'a list -> 'b list -> bool

    (* the n-arg versions of these standard list functions all raise
       ListPair if their argument lists are not the same length. *)
    val map3 : ('a * 'b * 'c -> 'd) -> 'a list -> 'b list -> 'c list -> 'd list

    val foldl3 : (('a * 'b * 'c) * 'e -> 'e) -> 'e -> 
                      'a list -> 'b list -> 'c list -> 'e

    (* these don't check lengths, just like ListPair.zip,unzip *)
    val unzip3 : ('a * 'b * 'c) list -> 'a list * 'b list * 'c list
    val zip3 : 'a list -> 'b list -> 'c list -> ('a * 'b * 'c) list

    val unzip4 : ('a * 'b * 'c *'d) list -> 'a list * 'b list * 'c list * 'd list
    val zip4   : 'a list -> 'b list -> 'c list -> 'd list -> ('a * 'b * 'c * 'd) list

    val mapsecond : ('a -> 'b) -> ('c * 'a) list -> ('c * 'b) list
    val mapfirst  : ('a -> 'b) -> ('a * 'c) list -> ('b * 'c) list

    val appsecond : ('a -> 'b) -> ('c * 'a) list -> unit
    val appfirst  : ('a -> 'b) -> ('a * 'c) list -> unit

    val allfirst  : ('a -> bool) -> ('a * 'b) list -> bool
    val allsecond : ('a -> bool) -> ('b * 'a) list -> bool

    val existsecond : ('a -> bool) -> ('b * 'a) list -> bool
    val existfirst  : ('a -> bool) -> ('a * 'b) list -> bool

    val mapto : ('a -> 'b) -> 'a list -> ('a * 'b) list

    (* Return the first element for which the function returns true,
       or else NONE if no such element exists. *)
    (* XX same as List.find *)
    val example : ('a -> bool) -> 'a list -> 'a option

    (* return also the rest of the list. *)
    val extract : ('a -> bool) -> 'a list -> ('a * 'a list) option

    (* finds an example such that the function returns SOME x,
       returns that mapped x *)
    val findpartial : ('a -> 'b option) -> 'a list -> 'b option

    (* like Vector.mapi, but no slice stuff. 0-based. 
       XXX these take the index and element in the opposite
       order from Vector. and Array... *)
    val mapi : ('a * int -> 'b) -> 'a list -> 'b list
    val appi : ('a * int -> unit) -> 'a list -> unit

    (* alladjacent f l
       true if f(lm, lm+1) for all m < (length l - 1)
       in the list *)
    val alladjacent  : ('a * 'a -> bool) -> 'a list -> bool

    (* allpairs f l
       true if f(la, lb) for all a, b in the list when a <> b.
       *)
    val allpairs     : ('a * 'a -> bool) -> 'a list -> bool

    (* same as allpairs, but assumes f is symmetric. *)
    val allpairssym : ('a * 'a -> bool) -> 'a list -> bool
    val apppairssym : ('a * 'a -> unit) -> 'a list -> unit

    (* sort a list least to greatest *)
    val sort : ('a * 'a -> order) -> 'a list -> 'a list
    (* on equal elements, use original list order *)
    val stablesort : ('a * 'a -> order) -> 'a list -> 'a list

    (* like hd o sort, but linear time *)
    val min : ('a * 'a -> order) -> 'a list -> 'a
    val max : ('a * 'a -> order) -> 'a list -> 'a

    (* (byfirst String.compare) will sort a (string * 'a) list. *)
    val byfirst  : ('a * 'b -> 'c) -> ('a * 'd) * ('b * 'e) -> 'c
    val bysecond : ('a * 'b -> 'c) -> ('d * 'a) * ('e * 'b) -> 'c

    (* like length o filter, but no allocation *)
    val count : ('a -> bool) -> 'a list -> int

    val transpose : 'a list list -> 'a list list

    (* first n items in list 1, rest in list 2 *)
    val cleave : int -> 'a list -> 'a list * 'a list

    (* generate a list of all permutations of the input list *)
    val permutations : 'a list -> 'a list list

end