
functor MapUtil(structure M : ORD_MAP) : MAPUTIL where type 'a map = 'a M.map =
struct

    type ord_key = M.Key.ord_key
    type 'a map = 'a M.map

    exception EarlyExit

    fun exists f m =
        (M.app (fn i => if f i then raise EarlyExit else ()) m;
         false) handle EarlyExit => true

    fun existsi f m =
        (M.appi (fn i => if f i then raise EarlyExit else ()) m;
         false) handle EarlyExit => true


    fun removeopt (m, k) =
      (SOME (M.remove (m, k))) handle LibBase.NotFound => NONE

    (* PERF? *)
(*
    datatype ('a, 'b) part = LL of 'a | RR of 'b | MM of 'a * 'b
    fun venn (a, b) = 
      let
        val a = map LL a
        val b = map RR b
        val universe = M.unionWith (fn MM (a, b)

        val l = ref M.empty
        val m = ref M.empty
        val r = ref M.empty

        fun one (v, LL x) = l := M.insert(v, x)
          | one (v, MM x) = m := M.insert(v, x)
          | one (v, RR x) = r := M.insert(v, x)
      in
        M.appi one universe;
        (!l, !m, !r)
      end
*)

    fun venn (a, b) =
      let
        fun folder (key, elt, (l, m, b)) =
          case removeopt (b, key) of
            SOME (b, elt') => (l, M.insert(m, key, (elt, elt')), b)
          | NONE => (M.insert(l, key, elt), m, b)
      in
        M.foldri folder (M.empty, M.empty, b) a
      end

end
