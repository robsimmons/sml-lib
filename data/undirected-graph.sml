
(* Designed for sparse graphs. Could have a dense implementation
   too (adjacency matrix). *)
functor UndirectedGraphFn(A : UNDIRECTEDGRAPHARG) :>
UNDIRECTEDGRAPH where type weight = A.weight =
struct

  structure GA = GrowArray
  type weight = A.weight

  exception UndirectedGraph of string

  fun util_for lo hi f =
      if lo > hi then ()
      else (ignore (f lo); util_for (lo + 1) hi f)


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

  fun get (g, i) =
      let val (a, _) = GA.sub g i
      in a
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
      case A.compare (w, A.zero) of
          GREATER =>
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
        | _ => raise UndirectedGraph "weights must be strictly positive"

  fun app f g =
      let fun loop ~1 = ()
            | loop n = (f (g, n); loop (n - 1))
      in  
          loop (GA.length g - 1)
      end

  fun edges (g, i) =
      let 
          val (_, l) = GA.sub g i
      in  map (fn (i', w) => ((g, i'), w)) l
      end

  structure H = HeapFn(type priority = weight
                       val compare = A.compare)

  fun shortestpaths ((g, src) : 'a node) =
      let
          (* Algorithm is Dijkstra's:
             
             - Heap contains frontier: index of points that we know a
               distance to, but haven't yet determined the final
               distance.
               
             - The minimum element in the heap is finalized. Put it
               in the final set (new graph). Add its edges to the
               frontier, repeat.
             
             *)
          (* New graph starts with same structure, but NONE in
             distance field. *)
          val newg = GA.tabulate 
              (GA.length g)
              (fn i =>
               let val (a, l) = GA.sub g i
               in ((a, NONE), l)
               end)

          datatype state = NOTYET | IN of H.hand | DONE
          (* To adjust the priority of something after the fact,
             we need a handle to it in the heap. This is a mapping
             from indices to handles. The handle only exists if the
             node is in the frontier. *)
          val handles = Array.array (GA.length g, NOTYET)

          val h = H.empty ()
          val () = Array.update (handles, src, IN (H.insert h A.zero src))

          fun loop () =
            case H.min h of
                NONE => ()
              | SOME (w, i) =>
                let
                    (* Distance is known. Finalize it. *)
                in
                  (case GA.sub newg i of
                    ((a, NONE), l) => 
                        let 
                            fun neighbor (j, dist) =
                              let val newdist = A.+(dist, w)
                              in
                                  (* Make sure it's in frontier with
                                     the right distance, if it needs
                                     to be. *)
                                  case Array.sub (handles, j) of
                                      NOTYET =>
                                          Array.update (handles, j,
                                                        IN (H.insert h newdist j))
                                    | DONE => () (* already done. *)
                                          (* could check that the final distance
                                             is indeed better than the one we
                                             computed. Maybe a good idea since
                                             if the weight argument is garbage,
                                             it would be good to detect that. *)
                                    | IN hand => 
                                      let val (olddist, _) = H.get h hand
                                      in  case A.compare (newdist, olddist) of
                                            LESS => H.adjust h hand newdist
                                          | _ => () (* no change. *)
                                      end
                              end
                        in
                            (* No longer in heap. *)
                            Array.update (handles, i, DONE);
                            GA.update newg i ((a : 'a, SOME w), l);
                            List.app neighbor l
                        end

                    | ((_, SOME _), _) => raise UndirectedGraph 
                          "Bug: Already had finalized distance.");
                    loop ()
                end
      in
          loop ();
          Array.app (fn (IN h) =>
                     raise UndirectedGraph 
                         "Bug: didn't process all nodes in frontier?"
                      | _ => ()) handles;
          { graph = newg,
            promote = (fn (g', i) => 
                       let in
                           samegraph (g, g');
                           (newg, i) 
                       end) }
      end

    datatype 'a span = S of { a : 'a, 
                              dist : weight option,
                              parent : 'a span node option }

    (* This is actually very simple. For each node with a non-NONE
       positive distance, just choose the neighbor node with the minimum
       distance as the parent. *)
    fun 'a spanningtree (g : ('a * weight option) graph) =
        let
            val newg : 'a span graph = GA.empty()
            val () =
              util_for 0 (GA.length g - 1)
              (fn i =>
               GA.update newg i
               (case GA.sub g i of
                  ((a, SOME d), l as ((h, hw) :: t)) =>
                      if A.compare (d, A.zero) = EQUAL
                      then (* root *)
                          (S { a = a, dist = SOME d, parent = NONE }, l)
                      else
                      let
                          val best = case #2 (#1 (GA.sub g h)) of
                              SOME d => A.+(d, hw)
                            | _ => raise UndirectedGraph "First distance incomplete"

                          (* Find the neighbor with the smallest distance;
                             pick it *)
                          fun m h best ((x, xw) :: t) =
                              (case #2 (#1 (GA.sub g x)) of
                                  SOME d => 
                                      let val d = A.+(d, xw)
                                      in
                                          if LESS = A.compare (d, best)
                                          then m x d t
                                          else m h best t
                                      end
                                | NONE =>
                                      raise UndirectedGraph "Distances are incomplete")
                            | m h _ nil = h
                      in
                          (S { a = a, 
                               dist = SOME d, 
                               parent = SOME (newg, m h best t) }, 
                           l)
                      end
                (* only possible if the source node is a singleton,
                   and this is it *)
                | ((a, SOME d), nil) => (S { a = a, dist = SOME d, parent = NONE }, nil)
                | ((a, NONE), l) => (S { a = a, dist = NONE, parent = NONE }, l)))
        in
            { graph = newg,
              promote = (fn (g', i) =>
                         let in
                             samegraph (g, g');
                             (newg, i)
                         end) }
        end


end


structure RealUndirectedGraph = UndirectedGraphFn(type weight = Real.real
                                                  val zero = 0.0
                                                  open Real)
structure IntUndirectedGraph = UndirectedGraphFn(type weight = Int.int
                                                 val zero = 0
                                                 open Int)
