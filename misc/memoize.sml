
structure Memoize :> MEMOIZE =
struct

  datatype ('arg, 'res) table =
    T of { lookup : 'arg -> 'res option,
           insert : 'arg -> 'res -> unit }

  (* just a function to create new tables *)
  type ('arg, 'res) tabler = unit -> ('arg, 'res) table


  (* would be nice to use SplayMap, but that requires more module
     power than SML offers *)
  datatype ('a, 'b) tree =
    Empty
  | Node of (('a, 'b) tree * 'a * 'b * ('a, 'b) tree)

  fun cmp_tabler cmp () =
    let
      val t = ref Empty
      fun lookup a = 
        let
          fun look Empty = NONE
            | look (Node(tl, aa, r, tr)) =
            (case cmp (a, aa) of
               EQUAL => SOME r
             | LESS => look tl
             | GREATER => look tr)
        in
          look (!t)
        end
      fun insert a r =
        let
          fun ins Empty = Node(Empty, a, r, Empty)
            | ins (Node(tl, aa, rr, tr)) =
            (case cmp (a, aa) of
               EQUAL => (Node(tl, a, r, tr))
             | LESS => Node(ins tl, aa, rr, tr)
             | GREATER => Node(tl, aa, rr, ins tr))
        in
          t := ins (!t)
        end
    in
      T { lookup = lookup, insert = insert }
    end

  fun idx_tabler gen min max () =
    let
      val arr = Array.array ((max + 1) - min, NONE)
      fun lookup a =
        (* might be out of bounds; treat as none *)
        (Array.sub(arr, gen a - min))
        handle _ => NONE

      (* might be out of bounds; just don't save *)
      fun insert a r =
        (Array.update(arr, gen a - min, SOME r))
        handle _ => ()

    in
      T { lookup = lookup, insert = insert }
    end

  fun eq_tabler eq () = 
    let
      val l = ref nil
      fun lookup a = 
        case List.find (fn (x, _) => eq(a, x)) (!l) of
          SOME(_, r) => SOME r
        | NONE => NONE
      fun insert a r = l := ((a, r) :: !l)
    in
      T { lookup = lookup, insert = insert }
    end


  fun memoize tabler f =
    let 
      val T { lookup, insert } = tabler ()
      fun mf x =
        (case lookup x of
           NONE => 
             let val r = f x
             in
               insert x r;
               r
             end
         | SOME ans => ans)
    in 
      mf
    end

  fun memoizerec tabler f =
    let 
      val T { lookup, insert } = tabler ()
      fun mf x =
        (case lookup x of
           NONE => 
             let val r = f mf x
             in
               insert x r;
               r
             end
         | SOME ans => ans)
    in 
      mf
    end

  fun memoize'' f = memoize (eq_tabler op=) f

end