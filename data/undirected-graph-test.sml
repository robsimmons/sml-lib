
structure UndirectedGraphTest =
struct

  exception TestFail of string

  (*

      A--- 3 --- C,
      |        / | \
     12 .- 5 -`  2  |
      |/         |  5
      B--- 1 --- D  |
       \         |  |
        6        1 /
         \       |/
          E-- 3 -F   G
  *)

  structure G = IntUndirectedGraph
  val _ = 0 : G.weight
  val (g : string G.graph) = G.empty ()
  val [AA, BB, CC, DD, EE, FF, GG] = map (G.add g) ["A", "B", "C", "D", "E", "F", "G"]

  val edges = [(AA, BB, 12),
               (AA, CC, 3),
               (BB, DD, 1),
               (CC, BB, 5),
               (CC, FF, 5),
               (CC, DD, 2),
               (FF, DD, 1),
               (EE, FF, 3),
               (BB, EE, 6)]

  val () = print "Original:\n"
  val () = G.app (fn n => 
                  let val name = G.get n
                  in  print ("  " ^ name ^ ".\n")
                  end) g

  val () = List.app (fn (a, b, w) => G.addedge a b w) edges
  val () = List.app (fn (a, b, w) =>
                     case (G.hasedge a b, G.hasedge b a) of
                         (NONE, _) => raise TestFail "Expected edge"
                       | (_, NONE) => raise TestFail "Expected reverse edge"
                       | (SOME w', SOME w'') =>
                             if w <> w' orelse w <> w''
                             then raise TestFail "Wrong weight"
                             else ()) edges

  val _ = Option.isSome (G.hasedge AA AA) andalso raise TestFail "Self-edges?!"
  val _ = Option.isSome (G.hasedge AA DD) andalso raise TestFail "unexpected edge"

  val { graph = sp, promote = _ } = G.shortestpaths AA
      handle G.UndirectedGraph s =>
          let in
              print ("shortestpaths err: " ^ s ^ "\n");
              raise TestFail s
          end

  fun otos f NONE = "NONE"
    | otos f (SOME z) = f z
  val () = print "Shortest distances:\n"
  val () = G.app (fn n => 
                  let val (name, dist) = G.get n
                  in  print ("  " ^ name ^ " : " ^ otos Int.toString dist ^ "\n")
                  end) sp

  val { graph = span, promote = _ } = G.spanningtree sp
      handle G.UndirectedGraph s =>
          let in
              print ("spanningtree err: " ^ s ^ "\n");
              raise TestFail s
          end
  val () = print "Spanning tree:\n"
  val () = G.app (fn n =>
                  let val G.S { a = name, dist, parent } = G.get n
                      val ppar = case parent of
                          NONE => "(no parent)"
                        | SOME x => let val G.S { a, ... } = G.get x
                                    in a
                                    end
                  in
                      print ("  " ^ name ^ " : " ^ otos Int.toString dist ^ " -> " ^
                             ppar ^ "\n")
                  end) span

  (*

      A<-- 3 --- C 
                 ^  
                 2   
                 |   
      B--- 1 --> D   
                 ^   
                 1  
                 | 
          E-- 3->F   G
  *)


  val () = print "Success!\n"
end
