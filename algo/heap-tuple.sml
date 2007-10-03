
functor HeapFn(type priority
               val compare : priority * priority -> order)
           :> HEAP where type priority = priority =
struct

  exception Heap of string

  (* flattened array of three elements *)
  structure Array3 :>
  sig
      type ('a, 'b, 'c) array
      val sub : ('a, 'b, 'c) array * int -> ('a * 'b * 'c)
      val update : ('a, 'b, 'c) array * int * ('a * 'b * 'c) -> unit
      val length : ('a, 'b, 'c) array -> int
      val tabulate : int * (int -> ('a * 'b * 'c)) -> ('a, 'b, 'c) array
  end =
  struct
      type ('a, 'b, 'c) array = 
          'a Array.array * 'b Array.array * 'c Array.array
      (* PERF only need to do bounds check once *)
      fun sub ((a, b, c), i) = (Array.sub(a, i),
                                Array.sub(b, i),
                                Array.sub(c, i))
      fun update ((a, b, c), i, (aa, bb, cc)) =
          let in
              Array.update(a, i, aa);
              Array.update(b, i, bb);
              Array.update(c, i, cc)
          end
      
      fun tabulate (0, _) = (Array.fromList nil,
                             Array.fromList nil,
                             Array.fromList nil)
        | tabulate (n, f) =
          let
              val (aa, bb, cc) = f 0

              val a = Array.array (n, aa)
              val b = Array.array (n, bb)
              val c = Array.array (n, cc)

              fun loop m =
                  if n = m
                  then (a, b, c)
                  else let in
                           update((a, b, c), m, f n);
                           loop (m + 1)
                       end
          in
              loop 1
          end

      fun length (a, _, _) = Array.length a
  end
  
  (* XXX necessary? *)
  type priority = priority

  (* if ~1, then the item has already been deleted *)
  type hand = int ref
  fun valid (ref ~1) = false
    | valid _ = true

  (* PERF perhaps as three arrays? *)
  type 'a heap = (int * (priority * 'a * hand) Array.array) ref


  fun empty () = ref (0, Array.fromList nil)

  (* assumes n > 0 *)
  fun removelast (heap as ref (n, a)) =
    let in
      (* don't bother shrinking the array *)
      heap := (n - 1, a);
      Array.sub(a, n - 1)
    end

  (* modify the heap to hold this element at i.
     updates the handle, but doesn't worry about
     what it's overwriting *)
  (* assumes i is in range *)
  fun setelem (ref (_, a)) i (pp, aa, hh) =
    let in
      Array.update(a, i, (pp, aa, hh));
      hh := i
    end

  (* the element i may violate the order invariant by being too high.
     swap it with children until it doesn't. *)
  fun percolate_down (heap as ref(n, a)) i =
    if 2 * i + 1 >= n then () (* end of heap *)
    else
      let
        (* get it *)
        val me = Array.sub(a, i)
          
        val li = 2 * i + 1
        val ri = 2 * i + 2

        (* compare to the two children *)
        val cl = Array.sub(a, li)

        fun swap child childi =
          let in
            setelem heap childi me;
            setelem heap i child;
            (* continue! *)
            percolate_down heap childi
          end
      in
        case compare (#1 me, #1 cl) of
          (* will swap, but with which child? *)
          GREATER =>
            if ri >= n 
            then swap cl li
            else let
                   val cr = Array.sub(a, 2 * i + 2)
                 in
                   (case compare (#1 cl, #1 cr) of
                      LESS => swap cl li
                    | _ => swap cr ri)
                 end
                      
        | _ => if ri >= n
               then () (* done -- less than left child,
                          no right child *)
               else let 
                      val cr = Array.sub(a, 2 * i + 2)
                    in
                      case compare (#1 me, #1 cr) of
                        GREATER => swap cr ri
                      | _ => () (* done -- less than both children *)
                    end
      end


  (* the element i may violate the order invariant by being too low.
     swap it with its parent until it doesn't. 


          a
        /   \
       b     c
      / \   / \
     d   e f   g

     suppose i is the index of c
     we know b<d,e, a<b, c<f,g, a<f,g
     but it may not be the case that a<c, which violates
     the invt.
       

          c
        /   \
       b     a
      / \   / \
     d   e f   g

     if we swap a and c, we fix this (perhaps introducing
     the same problem now with i=indexof(a)). 

     c < b because c < a < b.
     *)
  fun percolate_up _ 0 = () (* done -- root *)
    | percolate_up (heap as ref(_,a)) i =
    let
      val me = Array.sub(a, i)

      val pi = (i - 1) div 2

      val parent = Array.sub(a, pi)
    in
      case compare (#1 me, #1 parent) of
        LESS => (* swap *)
          let in
            setelem heap pi me;
            setelem heap i parent;
            (* continue! *)
            percolate_up heap pi
          end
      | _ => () (* done -- correct spot *)
    end

  (* inserts, using the handle h *)
  fun insertusing h heap p a =
    let
      (* ensure we have enough room for this new one *)
      val (n, arr) = 
        let val (n, arr) = !heap
        in
          if n = Array.length arr
          then 
          let
            val newsize = if n = 0 then 512
                          (* grow exponentially, 
                             but not doubling since
                             memory pressure is often
                             tight. *)
                          else n + ((3 * n) div 2)
            (* need a dummy element to grow beyond n + 1;
               just cook one up using the input (we'll never
               look at it) *)
            val dummy = (p, a, ref ~1)
            val a2 = Array.tabulate(newsize,
                                    (fn i =>
                                     if i < n
                                     then Array.sub(arr, i)
                                     else dummy))
          in
            heap := (n, a2);
            (n, a2)
          end
          else (n, arr)
        end
    in
      (* put in very last position (sets handle) *)
      setelem heap n (p, a, h);

      (* increase heap size *)
      heap := (n + 1, arr);

      (* but we may have violated the order invt, so restore it *)
      percolate_up heap n
    end

  (* PERF - check handle? or let subscript take care of it *)
  fun delete _ (ref ~1) = raise Heap "bad handle"
    (* if a handle is valid, then heap size > 0 *)
    | delete heap (h as ref i) =
    let
      (* now, remove the last element *)
      val (pl, al, hl) = removelast heap
        

      val (pold, _, _) = Array.sub(#2 (!heap), i)
    in
      (* invalidate it, since it's going away *)
      h := ~1;

      if hl = h
      then () (* done -- we just removed the last element. *)
      else
        let in
          (* overwrite this guy with last element *)
          setelem heap i (pl, al, hl);

          (* might have violated heap invariant *)
          case compare(pl, pold) of
            LESS => percolate_up heap i
          | GREATER => percolate_down heap i
          | EQUAL => ()
        end
    end
    

  fun min (ref (0, _)) = NONE
    | min (heap as ref(_, a)) = 
    let
      val (p, a, h) = Array.sub(a, 0)
    in
      delete heap h;
      SOME (p, a)
    end

  fun get (heap as ref(_, a)) (ref n) =
    let val (p, a, _) = Array.sub(a, n)
    in (p, a)
    end

  (* PERF this is simple, but we could also test whether this
     is an increase or decrease, and then percolate_up or 
     percolate_down! *)
  fun adjust heap h p =
    let val (_, a) = get heap h
    in
      delete heap h;
      insertusing h heap p a
    end

  fun insert heap p a =
    let
      val h = ref ~1
    in
      insertusing h heap p a;
      h
    end

  (* debugging stuff *)

  fun handtostring (ref n) = Int.toString n
  fun printheap atos ptos (ref (n, arr)) =
    let
      fun pchild par d i = 
        if i < n
        then 
          let 
            val (p, a, h) = Array.sub(arr, i)
          in
            print (CharVector.tabulate(d * 2, fn _ => #" "));
            if compare (p, par) = LESS then print "XXX<<<"
            else ();
            print ("p=" ^ ptos p ^ " a=" ^ atos a ^ " h=" ^ handtostring h);
            if !h <> i then print "XXX!!!"
            else ();
              print "\n";
              
              pchild p (d + 1) (i * 2 + 1);
              pchild p (d + 1) (i * 2 + 2)
          end
        else ()

    in
      print ("num elements: " ^ Int.toString n ^ "\n");
      if n > 0 then
         (* need a start priority *)
         let val (p, _, _) = Array.sub(arr, 0)
         in
           pchild p 0 0
         end
      else ()
    end

  fun size (ref (n, _)) = n

end