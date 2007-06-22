
structure SimpleStream :> SIMPLESTREAM =
struct

  type 'a stream = unit -> 'a option

  fun ss_app f s =
    case s () of
      NONE => ()
    | SOME x => (f x; ss_app f s)

  fun fromstring s =
    let
      val i = ref 0
    in
      (fn () =>
       if !i >= size s
       then NONE
       else SOME (String.sub(s, !i)
                  before i := !i + 1))
    end

  fun fromvector s =
    let
      val i = ref 0
    in
      (fn () =>
       if !i >= Vector.length s
       then NONE
       else SOME (Vector.sub(s, !i)
                  before i := !i + 1))
    end

  fun fromarray s =
    let
      val i = ref 0
    in
      (fn () =>
       if !i >= Array.length s
       then NONE
       else SOME (Array.sub(s, !i)
                  before i := !i + 1))
    end

  fun fromfile s = 
    let
      val f = ref (SOME (BinIO.openIn s))
    in
      (fn () =>
       case !f of
         NONE => NONE
       | SOME ff => 
           case BinIO.input1 ff of
             NONE => (BinIO.closeIn ff; f := NONE; NONE)
           | SOME c => SOME c)
    end

  fun fromlist l =
    let
      val lr = ref l
    in
      (fn () =>
       case !lr of
         nil => NONE
       | h :: t => 
           let in
             lr := t;
             SOME h
           end)
    end

  fun tolist f =
    (case f () of 
       NONE => nil
     | SOME c => c :: tolist f)

  fun map f s () =
    (case s () of
       NONE => NONE
     | SOME c => SOME (f c))

  fun toarrayslice f =
    case f () of
       SOME first =>
         let
           val a = ref (Array.array(256, first))
           val n = ref 1

           fun add e =
             if !n < (Array.length (!a) - 1)
             then (Array.update(!a, !n, e);
                   n := !n + 1)
             else
               (a := Array.tabulate(Array.length (!a) * 2,
                                    (fn x =>
                                     if x < Array.length (!a)
                                     then Array.sub(!a, x)
                                     else first));
                add e)

           fun go () =
             case f () of
               SOME e => (add e; go ())
             | NONE => ()
         in
           go ();
           ArraySlice.slice(!a, 0, SOME (!n))
         end
     | NONE => ArraySlice.slice (Array.fromList nil, 0, SOME 0)

  fun tobinfile outfile bs =
    let
      val f = BinIO.openOut outfile
    in
      ss_app (fn b => BinIO.output1(f, b)) bs;
      BinIO.closeOut f
    end

  fun tofile outfile bs =
    let
      val f = TextIO.openOut outfile
    in
      ss_app (fn b => TextIO.output1(f, b)) bs;
      TextIO.closeOut f
    end

  fun tovector f = ArraySlice.vector (toarrayslice f)

  fun flatten f =
    let
      val cur = ref (fn _ => NONE)
        
      fun rd () =
        case !cur () of
          NONE => 
            (* get new cur? *)
            (case f () of
               SOME f' => (cur := f'; rd())
             | NONE => (* done! *) NONE)
        | SOME i => SOME i
    in
      rd
    end

  val app = ss_app

end