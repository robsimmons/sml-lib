
functor TrieFn(A : TRIEARG) :> TRIE where type char = A.char and type string = A.string =
struct
    type char = A.char
    type string = A.string

    (* XXX This could probably be a general-purpose OffsetVector utility... *)
    (* Functional association from character index to value. Constant time access once built. *)
    structure V :
    sig
        type 't vec
        val empty : unit -> 't vec
        val sub : 't vec * int -> 't option
        val update : 't vec * int * 't -> 't vec
        val null : 't vec -> bool
        val appi : (int * 't -> unit) -> 't vec -> unit
        val foldli : (int * 't * 'b -> 'b) -> 'b -> 't vec -> 'b
        val foldri : (int * 't * 'b -> 'b) -> 'b -> 't vec -> 'b
        val tolist : 't vec -> (int * 't) list
    end =
    struct
        (* PERF should probably use better sparse encoding when data are sparse. *)
        (* Representation invariant: Always uses the minimum space, so that first
           and last elements of the vector are SOME if the vector has any elements
           at all. *)
        type 't vec = 
            { off : int, (* index of lowest child actually in index *)
              ch : 't option Vector.vector }
        fun empty () = { off = 0, ch = Vector.fromList nil }
        fun sub ({ off, ch }, i) =
            (* Triggering exception is normal *)
            Vector.sub(ch, i - off) handle Subscript => NONE

        fun null { ch, off = _ } = Vector.length ch = 0 
        fun appi f { ch, off } =
            Vector.appi (fn (i, SOME v) => f (i + off, v) | _ => ()) ch
        fun foldli f b { ch, off } =
            Vector.foldli (fn (i, SOME v, b) => f(i + off, v, b) | (_, NONE, b) => b) b ch
        fun foldri f b { ch, off } =
            Vector.foldri (fn (i, SOME v, b) => f(i + off, v, b) | (_, NONE, b) => b) b ch
            
        fun tolist v = foldri (fn (i, x, r) => (i, x) :: r) nil v

        fun update (old as { off, ch }, i, v) =
            let 
                (* need to be able to store old smallest element, and new element *)
                val nmin = if null old
                           then i
                           else Int.min(i, off)
                val nmax = Int.max(off + Vector.length ch - 1, i)
            in
                { off = nmin,
                  ch =
                    Vector.tabulate(nmax - nmin + 1,
                                    (fn x =>
                                     let val xx = x + nmin
                                     in
                                         if xx = i then SOME v
                                         (* PERF could inline, but there
                                            are several cases.. *)
                                         else sub(old, xx)
                                     end)) }
            end
    end

    (* Indexed vector of children, value for this node if accepting *)
    datatype 'a trie = T of ('a trie V.vec * 'a option) ref

    fun new () = T(ref (V.empty (), NONE))

    fun insertwith comb t s nv =
        let
            val T(target as ref (ch, v)) =
                (* fold, passing along the tree we'd insert into
                   if the string were empty *)
                A.fold (fn (c, T(target as ref (ch, v))) =>
                        let val oc = A.ord c
                        in case V.sub(ch, oc) of
                            NONE => 
                                let val n = new()
                                in target := (V.update(ch, oc, n), v);
                                   n
                                end
                          | SOME target => target
                        end) t s
        in
            case v of
                NONE => target := (ch, SOME nv)
              | SOME ov => target := (ch, SOME(comb(ov, nv)))
        end

    fun insert t s v = insertwith #2 t s v

    fun value (T(ref (_, vo))) = vo
    fun haschildren (T(ref (ch, _))) = not(V.null ch)

    fun child (T(ref (ch, _))) c = V.sub(ch, A.ord c)

    fun children (T(ref (ch, _))) = map (fn (i, t) => (A.chr i, t)) (V.tolist ch)

    fun appchildren (T(ref (ch, _))) f = V.appi (fn (i, v) => f(A.chr i, v)) ch
    fun foldchildren (T(ref (ch, _))) b f =
        V.foldli (fn (i, v, b) => f(A.chr i, v, b)) b ch

end

(* PERF can possibly get better space performance by writing chr and
   ord to go through a transposition table that puts character classes
   near one another (eg. starts with aeiou) *)
structure Trie = TrieFn(type char = Char.char
                        (* same as String.string *)
                        type string = CharVector.vector
                        val chr = Char.chr
                        val ord = Char.ord
                        val fold = CharVector.foldl)