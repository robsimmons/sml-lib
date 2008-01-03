
(* mutable collection of flags and string constants.
   see params-sig.sml.
*)

structure Params :> PARAMS =
struct

  exception BadOption of string

  val flags = (ref nil) 
              : (bool ref * bool * (string * string * unit) option * 
                 string * unit) list ref

  val params = (ref nil)
               : (string ref * string * (string * string * unit) option * 
                  string * unit) list ref

  (* final bool: true if accumulator *)
  val paramlists = (ref nil)
               : (string list ref * string list * (string * string * char) option *
                  string * bool) list ref

  fun I x = x

  fun get lr (name : string) =
    let
      fun f nil = NONE
        | f ((r, _, _, n, _)::t) = 
          if name = n 
          then SOME r
          else f t 
    in
      f (!lr)
    end

  fun argget selector lr (name : string) =
    let
      fun f nil = NONE
        | f ((h as (r, _, SOME(n, _, _), _, _))::t) = 
          if name = n 
          then SOME (selector h)
          else f t 
        | f (_::t) = f t
    in
      f (!lr)
    end

  fun make (f  : 'spec option -> (string * string * 'extra) option) 
           (lr : ('output ref * 'output * (string * string * 'extra) option * 
                  string * 'flag) list ref)
           (flags : 'flag)
           (default : 'output)
           (cmd : 'spec option)
           (name : string)
           : 'output ref =
    case get lr name of
      NONE => 
        let 
          val h = ref default
        in
          lr := ((h,default,f cmd,name, flags) :: !lr);
          h
        end
    (* XXX for paramacc, perhaps should merge the two defaults *)
    | SOME r => r

  fun twotothree NONE = NONE
    | twotothree (SOME (a,b)) = SOME(a,b,())

  val getflag = get flags
  val flag = make twotothree flags ()

  val getparam = get params
  val param = make twotothree params ()

  val getparamlist = get paramlists
  val paramlist = make I paramlists false

  val getparamacc = get paramlists
  val paramacc = make I paramlists true

  fun plarg (r, _, SOME(_, _, c), _, b) = (r, c, b)
    | plarg _ = raise BadOption "impossible"

  fun asint def (ref a) = getOpt (Int.fromString a, def)

  val table = StringUtil.table 75

  fun usage () =
    let
        fun smp ((cl, doc, _)) s = [cl, s, doc]
        fun pl ((cl, doc, c)) s = [cl, str c, s, doc]
        fun f ml s ts l = rev (foldr (fn ((_, d, SOME xx, _, _), b) =>
                                   ml xx (ts d) :: b 
                                   | (_, b) => b) [s] l)
        fun bts d = (if d then "(true)" else "(false)")
    in
      (case !flags of
         nil => ""
       | l => "The following flags are supported (specify to toggle):\n" ^
              (table (f smp ["flag", "default", "description"] bts l))) ^
      (case !params of
         nil => ""
       | l => "\nThe following parameters are supported (specify,\n" ^
              "followed by a string, to change them):\n" ^
              (table (f smp ["param", "default", "description"] I l))) ^
           (* XXX should distinguish accumulator from overwrite *)
           (* XXX should delimit with actual separator char *)
      (case !paramlists of
         nil => ""
       | l => "\nThe following parameters take a list of strings.\n" ^
              "Specify them followed by a list of arguments separated\n" ^
              "by the given separator character.\n" ^
              (table (f pl ["param", "sep", "default", "description"] 
                           (StringUtil.delimit ",") l)))
    end

  fun docommandline () = 
    let
      fun f nil l = rev l
        | f (h::t) l =
        case argget #1 flags h of
          NONE =>
            (case argget #1 params h of
               NONE =>
                (case argget plarg paramlists h of
                     NONE => f t (h::l)
                   | SOME (pr, ch, acc) =>
                      (case t of
                           nil => raise BadOption
                               (h ^ " must be followed by a list")
                         | v::rest => 
                               let
                                   val toks = 
                                       String.fields (StringUtil.ischar ch) v
                               in
                                   pr := (if acc then toks @ !pr else toks);
                                   f rest l
                               end))
             | SOME sr => 
                 (case t of 
                    nil => raise BadOption 
                          (h ^ " must be followed by a value")
                  | v::rest => 
                          let in
                            sr := v;
                            f rest l
                          end))
        | SOME br => let in
                       br := (not (!br));
                       f t l
                     end
    in
      f (CommandLine.arguments()) nil
    end

end
