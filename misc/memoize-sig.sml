
(* memoize arbitrary functions. *)
signature MEMOIZE =
sig

  (* a way of creating maps out of the type 'arg *)
  type ('arg, 'res) tabler

  (* last resort; poor performance *)
  val eq_tabler : ('arg * 'arg -> bool) -> ('arg, 'res) tabler

  (* usually better than eq_tabler *)
  val cmp_tabler : ('arg * 'arg -> order) -> ('arg, 'res) tabler

  (* for arguments that can be mapped to
     dense integers: give index generator, min, max. if the index
     goes outside the range of the table, it won't be memoized *)
  val idx_tabler : ('arg -> int) -> int -> int -> ('arg, 'res) tabler

  (* XXX add auto-growing idx tabler *)

  (* memoize a function *)
  val memoize : ('arg, 'res) tabler -> ('arg -> 'res) -> 'arg -> 'res

  (* memoize a recursive function. it gets a memoized version of itself
     to call recursively as an argument. *)
  val memoizerec : ('arg, 'res) tabler ->
    (('arg -> 'res) -> 'arg -> 'res) -> 
    'arg -> 'res

  (* for convenience when the argument is an equality type; 
     same as: memoize (eq_tabler op=) *)
  val memoize'' : (''arg -> 'res) -> ''arg -> 'res

  (* Example: 

     fun fact self 0 = 1
       | fact self n = n * self(n - 1)
     
     val mf = memoizerec (cmp_tabler Int.compare) fact

     *)

end