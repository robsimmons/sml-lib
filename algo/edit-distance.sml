
functor EditDistanceFn(S : EDIT_DISTANCE_ARG)
 :> EDIT_DISTANCE where type ch = S.ch and type str = S.str =
struct
  open S

  exception EditDistance of string

  datatype edit =
    Insert of int * ch
  | Delete of int
  | Modify of int * ch

  fun minedit (s1, s2) =
    let
      val n1 = len s1
      val n2 = len s2

      (* each cell (x, y) stores the minimum cost and the
         minimal edit list to turn 
         prefix(s1, x) into prefix(s2, y) 
         
         we are careful about sharing so that exactly
         n^2 edits are generated, even though they share
         list tails 
         
         this array is of size (size(s1) + 1) * (size(s2) + 1)
         because we also record the zero length prefix.
         *)
      val a = Array.array((1 + n1) * (1 + n2), NONE)
      fun read (x, y) = Array.sub(a, y * (n1 + 1) + x)
      fun write (x, y, v) = Array.update(a, y * (n1 + 1) + x, v)

      (* 
              s1      --> x
                h e l l o 
              0 1 2 3 4 5 ... n1
        s2 0  
         w 1    .
       | o 2      .
       V r 3        .
         l 4        
       y d 5
           .
           :
          n2

          *)


(*
      (* this also does the top left *)
      fun top l cost x =
        if x > n1 then ()
        else (write(x, 0, SOME(cost + (DELETE_COST (sub (s1, x )), l));
              top (Delete x :: l) (x + 1))

      fun left l y =
        if y >= (n2 + 1) then ()
        else let val l = Insert (0, sub (s2, y - 1)) :: l
             in
               write(0, y, SOME(y * INSERT_COST, l));
               left l (y + 1)
             end
*)

      fun for lo hi f =
        if lo > hi then ()
        else (ignore (f lo); for (lo + 1) hi f)

      (* initialize the boundary conditions. *)
      val () = write(0, 0, SOME(0, nil))
      (* the meaning of the top row is to edit
         prefixes of s1 into the empty string; we
         can only do this by deleting the whole
         string. *)
      val () = for 1 n1
              (fn x =>
               let val (lc, lp) = valOf (read (x - 1, 0))
               in
                 write(x, 0,
                       SOME(lc + DELETE_COST (sub (s1, x - 1)),
                            Delete (x - 1) :: lp))
               end)

      (* for the left, we're creating the string out
         of the empty string, so insert chars. *)
      val () = for 1 n2
              (fn y =>
               let val (uc, up) = valOf (read (0, y - 1))
               in
                 write(0, y,
                       SOME(uc + INSERT_COST (sub (s2, y - 1)),
                            Insert (0, sub (s2, y - 1)) :: up))
               end)
(*
      fun printboard () =
          print (StringUtil.table 79
                 (* num header *)
                 ((" " :: " " :: List.tabulate (n1 + 1, (fn x => Int.toString x))) ::
                 (* string header *)
                 (" " :: " " :: "-" :: List.tabulate (n1, (fn x => implode [sub (s1, x)]))) ::
                 (* all rows *)
                 List.tabulate (n2 + 1, 
                                (fn y =>
                                 Int.toString y :: (if y = 0 then "-"
                                                    else implode [sub (s2, y - 1)]) ::
                                 List.tabulate (n1 + 1, (fn x =>
                                                         (case read (x, y) of
                                                              NONE => "?"
                                                            | SOME (d, _) => Int.toString d)))))
                  )
                 )
*)

    in
      for 1 n2
      (fn y =>
       (for 1 n1
        (fn x =>
         (* want to turn prefix(s1, x) into prefix(s2, y).
            There are a few ways to do it. 
            
            Diag: We can take the edit that turns prefix(s1, x - 1)
            into prefix(s2, y - 1) and then modify that last char from
            s1[x] to s2[y].
            
            Left: Or we can the edit that turns prefix(s1, x - 1) into
            prefix(s2, y) and start by deleting the last character; now
            it is prefix(s1, x - 1) so that edit completes the process.
            
            Up:
            *)
         
         let
             val (diag, diage) = valOf (read(x - 1, y - 1))
             val (up, upe)     = valOf (read(x, y - 1))
             val (left, lefte) = valOf (read(x - 1, y))
             val c1 = sub(s1, x - 1)
             val c2 = sub(s2, y - 1)
             val (modc, same) = if eq(c1, c2) then (0, true) else (MODIFY_COST (c1, c2), false)
             val insc = INSERT_COST c2
             val delc = DELETE_COST c1
         in
             (if diag + modc <= (up + insc) andalso 
                 diag + modc <= (left + delc)
              then write(x, y, SOME(diag + modc, if same then diage
                                                 else Modify (x - 1, c2) :: diage))
              else
                  if left + delc <= diag + modc andalso 
                     left + delc <= up + insc
                  then write(x, y, SOME(left + delc, Delete (x - 1) :: lefte))
                  else write(x, y, SOME(up + insc, Insert (x, c2) :: upe))
                 )
         end)));

      (* the whole strings *)
      (case read(n1, n2) of
         SOME(cost, edits) => (cost, edits)
       | NONE => raise EditDistance "impossible!")
    end

end

structure EditDistanceString =
   EditDistanceFn(type ch = char
                  type str = string
                  val eq = op =
                  val sub = String.sub
                  val len = size
                  fun MODIFY_COST _ = 1
                  fun INSERT_COST _ = 1
                  fun DELETE_COST _ = 1)
