
structure StringOnce :> STRINGONCE =
struct

    structure SM = 
        SplayMapFn(type ord_key = string val compare = String.compare)


    type stringarena =
        { sep : string,
          ctr : int ref,
          table : (int ref) SM.map ref }

    fun arena () = { sep = "", ctr = ref 0, table = ref SM.empty }

    fun arenaex sep = { sep = sep, ctr = ref 0, table = ref SM.empty }

    fun clear ({ ctr, table, ... } : stringarena) = 
        let in
            table := SM.empty;
            ctr := 0
        end

    fun ++ x = (x := !x + 1; !x)

    (* XXX this is not optimal. 
       the best behavior would be to delay the
       choice of who gets to be 's' (without
       digits) to the first one whose f is
       called. *)

    fun symbol { sep, ctr, table } s =
        (case SM.find (!table, s) of
             NONE =>
               let
                   val ir = ref 0
               in
                 (* first var with this name.
                    it will always be called 's'
                    *)
                 table := SM.insert(!table, s, ir);
                 (++ ctr,
                  fn () => s)
               (*
                  if !ir = 0 
                  then s
                  else s ^ sep ^ "0") *)
               end
           | SOME ir => 
               let
                   val me = ++ ir
               in
                   (++ ctr,
                    fn () =>
                    s ^ sep ^ Int.toString me)
               end)
end