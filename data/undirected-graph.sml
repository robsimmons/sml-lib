
(* Designed for sparse graphs. Could have a dense implementation
   too (adjacency matrix). *)
functor UndirectedGraphFn(A : UNDIRECTEDGRAPHARG) :>
UNDIRECTEDGRAPH where type weight = A.weight =
struct

  structure GA = GrowArray
  type weight = A.weight

  exception UndirectedGraph of string

  (* The representation of the graph is an array of node
     data. Each node is represented by its index into this
     array. Node data is a list of connected nodes, with weights.
     These are symmetric by invariant. *)
  type 'a graph = ('a * (int * weight) list) GA.growarray
  fun samegraph (a, b) = if GA.eq(a, b) then () 
                         else raise UndirectedGraph "Not from the same graph!"
  fun gph (a, _) = a
  fun idx (_, i) = i

  (* The graph it's in, plus its index. In bounds by invariant. *)
  type 'a node = 'a graph * int

  fun cmp (a, b) =
      let in
          samegraph (gph a, gph b);
          Int.compare (idx a, idx b)
      end
  fun eq (a, b) =
      let in
          samegraph (gph a, gph b);
          idx a = (idx b : int)
      end

  val empty = GA.empty
  fun add g a =
      let val i = GA.length g
      in  GA.append g (a, nil);
          (g, i)
      end

  fun hasedge a b =
      let
          val () = samegraph (gph a, gph b)
          val (_, l) = GA.sub (gph a) (idx a)
      in
          case List.find (fn (x : int, _) => x = idx b) l of
              NONE => NONE
            | SOME (_, w) => SOME w
      end

  fun addedge a b w =
      let 
          val g = gph a
          fun add (ix, iy) =
              let 
                  val (a, l) = GA.sub g ix
              in
                  GA.update g ix (a, (iy, w) :: l)
              end
      in
          (* nb. also checks same graph *)
          (case hasedge a b of
               NONE => ()
             | SOME _ => raise UndirectedGraph "Edge already exists.");
          add (idx a, idx b);
          add (idx b, idx a)
      end

  fun app f g =
      let fun loop ~1 = ()
            | loop n = f (g, n)
      in  loop (GA.length g - 1)
      end

  fun edges (g, i) =
      let 
          val (_, l) = GA.sub g i
      in  map (fn (i', w) => ((g, i'), w)) l
      end

end


structure RealUndirectedGraph = UndirectedGraphFn(type weight = Real.real
                                                  open Real)
structure IntUndirectedGraph = UndirectedGraphFn(type weight = Int.int
                                                 open Int)
