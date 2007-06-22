
(* Topological sort algorithm. Naive
   list version.
*)

structure TopoSort :> TOPOSORT =
struct
    
    exception TopoSort of string

    type stamp = unit ref

    type 'a node = 'a * unit ref

    (* (a, b) => "a must appear before b" *)
    type 'a constraint = unit ref * unit ref

    fun constraint ((_,a), (_,b)) = (a, b)

    fun node a = (a, ref())

    (* XXX PERF this could be a lot more efficient with better
       data structures *)

    fun sort nil nil = nil
      | sort nil _ = raise TopoSort "constraints on non-members"
      | sort nl cl =
        case List.partition 
              (fn (_,x) =>
               (* must something come before it? *)
               List.exists (fn (a, b) => b = x) cl) nl of
              (_, nil) => raise TopoSort "sort impossible"
            | (wait, ready) =>
                  ready @ sort wait 
                  (List.filter
                   (fn (a, b) =>
                    not 
                    (List.exists 
                     (fn (_, x) => x = a) 
                     ready)) cl)

    fun get (a, _) = a

end
