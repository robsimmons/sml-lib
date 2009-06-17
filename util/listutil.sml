
structure ListUtil :> LISTUTIL =
struct

    exception ListUtil

    (* Association lists *)
    structure Alist =
    struct
        exception NotFound

        fun find eq nil key = NONE
          | find eq ((a,b)::t) key =
            if eq (a, key) then SOME b
            else find eq t key

        fun extract eq al x =
          let
            fun ex _ nil = NONE
              | ex h ((a,b)::t) = if eq (a, x)
                                  then SOME (b, rev h @ t)
                                  else ex ((a,b)::h) t
          in
            ex nil al
          end

        fun get eq nil key = NONE
          | get eq ((h as (a,b))::t) key =
            if eq (a, key) then SOME h
            else get eq t key

        fun haskey f l k = Option.isSome (find f l k)

        fun update eq l a b = map (fn (aa,bb) => 
                                   (aa, if eq(aa,a) then b else bb)) l

        fun modify eq l a bf = map (fn (aa,bb) => 
                                   (aa, if eq(aa,a) then bf bb else bb)) l
                
        fun removefirst eq nil key = raise NotFound
          | removefirst eq ((a,b)::t) key =
            if eq (a, key) then t
            else (a,b) :: removefirst eq t key
                
        fun removeall eq nil key = nil
          | removeall eq ((a,b)::t) key =
            if eq (a, key) then removeall eq t key
            else (a,b) :: removeall eq t key
                
        fun bycompare f x = EQUAL = f x

        fun s (a, b) = (b, a)
        fun swap l = map s l

    end

    (* sorted lists *)

    structure Sorted =
    struct
        fun insert cmp nil a = [a]
          | insert cmp (h::t) a =
            case cmp (h, a) of
                LESS => h :: insert cmp t a
              | _ => a :: h :: t

        fun reverse c a = 
            case c a of
                LESS => GREATER
              | GREATER => LESS
              | EQUAL => EQUAL

        fun insertbest 0 _ _ _ = nil
          | insertbest max cmp nil a = [a]
          | insertbest max cmp (h::t) a =
            case cmp(h, a) of
                LESS => h :: insertbest (max - 1) cmp t a
              | _ =>    a :: insertbest (max - 1) cmp t h

    end

    fun stratify cmp nil = nil
      | stratify cmp ((aa, bb) :: t) =
      let
        val ls = stratify cmp t

        fun insert (a, b) nil = [(a, [b])]
          | insert (a, b) ((h,elems) :: rest) =
          case cmp (a, h) of
            EQUAL => (h, b :: elems) :: rest
          | LESS => (a, [b]) :: (h, elems) :: rest
          | GREATER => (h, elems) :: insert (a, b) rest
      in
        insert (aa, bb) ls
      end

    fun combinel f nil = raise ListUtil
      | combinel f (h::t) = foldl f h t

    fun combiner f [x] = x
      | combiner f (h::t) = f (h, combiner f t)
      | combiner f nil = raise ListUtil

    fun list x = [x]

    fun aslongas f nil = nil
      | aslongas f (h::t) = if f h then h :: aslongas f t else nil

    fun after f nil = nil
      | after f (l as (h::t)) = if f h then l else after f t

    fun partitionaslongas f l =
        let
            fun pala a (u as (h::t)) = if f h then pala (h::a) t
                                       else (rev a, u)
              | pala a nil = (l, nil)
        in
            pala nil l
        end

    fun position' _ _ nil = NONE
      | position' f n (h::t) = if f h then SOME n else position' f (n + 1) t
    fun position f l = position' f 0 l

    fun all2 f nil nil = true
      | all2 f (a::ta) (b::tb) = f (a, b) andalso all2 f ta tb
      | all2 f _ _ = false

    fun map3 f nil nil nil = nil
      | map3 f (a::ta) (b::tb) (c::tc) = f (a, b, c) :: map3 f ta tb tc
      | map3 _ _ _ _ = raise ListUtil

    fun app3 f nil nil nil = ()
      | app3 f (a::ta) (b::tb) (c::tc) = (ignore (f (a, b, c)); app3 f ta tb tc)
      | app3 _ _ _ _ = raise ListUtil

    fun foldl3 f acc nil nil nil = acc
      | foldl3 f acc (a::ta) (b::tb) (c::tc) = 
        foldl3 f (f((a, b, c), acc)) ta tb tc
      | foldl3 _ _ _ _ _ = raise ListUtil

    fun foldli f b l =
      let
        fun go (acc, _, nil) = acc
          | go (acc, n, h :: t) = go (f(n, h, acc), n + 1, t)
      in
        go (b, 0, l)
      end
  
    fun foldri f b l =
      let
        fun go (_, nil) = b
          | go (n, h :: t) = f(n, h, go(n + 1, t))
      in
        go (0, l)
      end

    fun mapsecond f nil = nil 
      | mapsecond f ((a,b)::t) = (a,f b) :: mapsecond f t

    fun mapfirst  f nil = nil
      | mapfirst  f ((a,b)::t) = (f a, b) :: mapfirst f t

    fun appsecond f nil = ()
      | appsecond f ((_,b)::t) = (ignore (f b); appsecond f t)

    fun appfirst  f nil = ()
      | appfirst  f ((a,_)::t) = (ignore (f a); appfirst f t)

    fun existfirst f nil = false
      | existfirst f ((a,b)::t) = f a orelse existfirst f t

    fun existsecond f nil = false
      | existsecond f ((a,b)::t) = f b orelse existsecond f t

    fun mapi f l =
        let fun mm n nil = nil
              | mm n (h::t) = f (h, n) :: mm (n + 1) t
        in
            mm 0 l
        end

    fun appi f l =
        let fun mm n nil = ()
              | mm n (h::t) = (ignore (f (h, n)); mm (n + 1) t)
        in
            mm 0 l
        end

    (* should be optimized to do less consing. (split mainly) *)
    fun sort cmp l =
        let
            fun split l =
                let fun s a1 a2 nil = (a1, a2)
                      | s a1 a2 (h::t) = s a2 (h::a1) t
                in s nil nil l
                end

            fun merge a nil = a
              | merge nil b = b
              | merge (aa as (a::ta)) (bb as (b::tb)) =
                case cmp (a, b) of
                    EQUAL => (a :: b :: merge ta tb)
                  | LESS => (a :: merge ta bb)
                  | GREATER => (b :: merge aa tb)

            fun ms nil = nil
              | ms [s] = [s]
              | ms [a,b] = merge [a] [b]
              | ms ll = 
                let val (a,b) = split ll
                in merge (ms a) (ms b)
                end
        in ms l
        end

    (* stability requires indices for merge phase,
       so this is a little slower *)
    fun stablesort cmp l =
        let
            val tagged = mapi (fn x => x) l

            fun split l =
                let fun s a1 a2 nil = (a1, a2)
                      | s a1 a2 (h::t) = s a2 (h::a1) t
                in s nil nil l
                end

            fun merge a nil = a
              | merge nil b = b
              | merge (aa as ((ah as (a,ai))::ta)) 
                      (bb as ((bh as (b,bi))::tb)) =
                case cmp (a, b) of
                    EQUAL => 
                        (case Int.compare (ai, bi) of
                             LESS => (ah :: merge ta bb)
                           | GREATER => (bh :: merge aa tb)
                           | EQUAL => raise ListUtil)
                  | LESS => (ah :: merge ta bb)
                  | GREATER => (bh :: merge aa tb)

            fun ms nil = nil
              | ms [s] = [s]
              | ms ll = 
                let val (a,b) = split ll
                in merge (ms a) (ms b)
                end

        in map #1 (ms tagged)
        end

    fun min f l =
        let
            fun m nil a = a
              | m (h::t) a =
                case f(h, a) of
                    LESS => m t h
                  | _ => m t a
        in
            m (tl l) (hd l)
        end

    fun byfirst f ((a,b),(aa,bb)) = f (a, aa)
    fun bysecond f ((a,b),(aa,bb)) = f (b, bb)

    fun allfirst f ((a,b)::t) = f a andalso allfirst f t
      | allfirst f nil = true

    fun allsecond f ((a,b)::t) = f b andalso allsecond f t
      | allsecond f nil = true

    fun alladjacent f nil = true
      | alladjacent f [_] = true
      | alladjacent f (a::(l as (b::_))) = f (a, b) andalso alladjacent f l

    (* assumes f reflexive *)
    fun allpairs f l =
        let 
            fun apa _ nil = true
              | apa a (b::t) = f (a, b) andalso f (b, a) andalso apa a t

            fun ap nil = true
              | ap [_] = true
              | ap (h::t) = apa h t andalso ap t
        in
            ap l
        end

    (* if f is symmetric *)
    fun allpairssym f l = 
        let 
            fun apa _ nil = true
              | apa a (b::t) = f (a, b) andalso apa a t

            fun ap nil = true
              | ap [_] = true
              | ap (h::t) = apa h t andalso ap t
        in
            ap l
        end

    fun apppairssym f l = 
        let 
            fun apa _ nil = ()
              | apa a (b::t) = (ignore (f (a, b)); apa a t)

            fun ap nil = ()
              | ap [_] = ()
              | ap (h::t) = (apa h t; ap t)
        in
            ap l
        end

   fun count f l =
        let
            fun c nil a = a
              | c (h::t) a = c t (if f h then a + 1
                                  else a)
        in
            c l 0
        end

   fun transpose nil = nil
     | transpose (nil::_) = nil
     | transpose lists = let fun split nil = raise ListUtil
                               | split (a::b) = (a,b)
                             val pairs = map split lists
                         in (map #1 pairs) :: (transpose (map #2 pairs))
                         end

   fun example f nil = NONE
     | example f (h::t) = if f h then SOME h else example f t

   fun extract f l =
     let
       (* PERF revappend? *)
       fun ex (l, h :: r) = if f h then SOME (h, rev l @ r)
                            else ex (h :: l, r)
         | ex (l, nil) = NONE
     in
       ex (nil, l)
     end

   fun findpartial f nil = NONE
     | findpartial f (h::t) = 
     case f h of 
       NONE => findpartial f t 
     | yes => yes

   fun unzip3 abc_list =
       let fun unzip3_loop [] (aa,bb,cc) = (rev aa, rev bb, rev cc)
             | unzip3_loop ((a,b,c)::rest) (aa,bb,cc) = unzip3_loop rest (a::aa,b::bb,c::cc)
       in unzip3_loop abc_list ([],[],[])
       end

   fun zip3 a b c = map (fn ((a,b),c) => (a,b,c)) 
                          (ListPair.zip (ListPair.zip (a, b), c))

   (* a wedding is defined as being between an 'a and a 'b *)
   fun wed nil nil = nil
     | wed nil _ = raise ListUtil
     | wed _ nil = raise ListUtil
     | wed (a::t) (b::u) = (a, b) :: wed t u

   fun unzip4 abcd_list =
     let fun unzip4_loop nil (aa, bb, cc, dd) = (rev aa, rev bb, rev cc, rev dd)
           | unzip4_loop ((a,b,c,d)::rest) (aa,bb,cc,dd) =
                unzip4_loop rest (a::aa, b::bb, c::cc, d::dd)
     in
       unzip4_loop abcd_list (nil, nil, nil, nil)
     end

   fun zip4 a b c d = map (fn ((a, b, c), d) => (a, b, c, d))
                           (ListPair.zip (zip3 a b c, d))

   fun choosemap f l =
     let 
       fun cm bef (h::aft) = f(h, (rev bef)@aft) :: cm (h::bef) aft
         | cm _ nil = nil
     in
       cm nil l
     end

   fun choosek k l =
       let
           (* keep track of the length of l, so that we can stop early. *)

           (* We can always choose 0 out of any list. *)
           fun ck (0, _, _) = [nil]
             (* but we need to have at least as many elements otherwise... *)
             | ck (k, n, l) =
               if k > n
               then nil
               else
                   (* must succeed; k is <= n, and k is nonzero *)
                   let val h = hd l
                       val t = tl l

                       (* we can take this one or not. *)
                       val take = map (fn s => (h :: s)) (ck (k - 1, n - 1, t))
                       val don't = ck (k, n - 1, t)
                   in
                       take @ don't
                   end
       in
           ck (k, length l, l)
       end
       

   fun mapto f nil = nil
     | mapto f (h :: t) = (h, f h) :: mapto f t

   fun cleave' 0 l acc = (rev acc,l)
     | cleave' n [] acc = raise Subscript
     | cleave' n (h::t) acc = cleave' (n-1) t (h::acc)

   fun cleave n l = if n < 0 then raise Subscript else cleave' n l []

   fun permutations nil = [[]]
     | permutations (h::t) =
       let
           val pt = permutations t
               
           (* each_pos (x : 'a) (l : 'a list)
              returns 'a list list where 
              x has been inserted in each possible
              position within l *)
           fun each_pos x nil = [[x]]
             | each_pos x (hh::tt) =
               (x :: hh :: tt) ::
               (map (fn l => hh :: l) (each_pos x tt))
       in
           List.concat
           (map 
            (fn one_perm =>
             each_pos h one_perm) pt)
       end

    fun max f l = min (Sorted.reverse f) l

    (* cuter with folds! *)
    fun power l =
      foldl (fn (elt, sofar) => 
             foldl (fn (lis, sets) =>
                    (elt :: lis) :: sets) sofar sofar
             ) [nil] l

end
