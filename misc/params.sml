
(* mutable collection of flags and string constants.
   see params-sig.sml.

   I use this code all the time but it is poop city.
   I should rewrite this and provide a legacy interface.
*)

structure Params :> PARAMS =
struct

  exception BadOption of string
  exception Params of string

  (* If we use_ it before we declare it, then it goes in
     ForwardDeclared state and we fail in docommandline
     if we see one of these. *)
  datatype 'a state =
    ForwardDeclared
  | Declared of 'a

  val flags = ref nil 
               (* cell      default   (commandlinename, docstring, nothing) *)
              : (bool ref * (bool * (string * string * unit) option) state * 
                (* name nothing *)
                 string * unit) list ref

  val params = ref nil
               : (string ref * (string * (string * string * unit) option) state * 
                  string * unit) list ref

  (* final bool: true if accumulator *)
  val paramlists = ref nil
               : (string list ref * (string list * (string * string * char) option) state *
                  string * bool) list ref

  fun I x = x

  fun get lr (name : string) =
    let
      fun f nil = NONE
        | f ((r, _, n, _)::t) = 
          if name = n 
          then SOME r
          else f t 
    in
      f (!lr)
    end

  fun argget selector lr (name : string) =
    let
      fun f nil = NONE
        | f ((h as (r, Declared(_, SOME(n, _, _)), _, _))::t) = 
          if name = n 
          then SOME (selector h)
          else f t 
        | f ((_, ForwardDeclared, name, _) :: t) = 
            raise Params ("Arg " ^ name ^ " was only forward declared!")
        | f (_::t) = f t
    in
      f (!lr)
    end

  fun make (f  : 'spec option -> (string * string * 'extra) option) 
           (lr : ('output ref * ('output * (string * string * 'extra) option) state * 
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
          lr := ((h, Declared(default, f cmd), name, flags) :: !lr);
          h
        end
    (* XXX for paramacc, perhaps should merge the two defaults? *)
    | SOME (r : 'output ref) =>
        let
          (* If it's there as ForwardDeclared, upgrade. *)
          fun g b nil = r
            | g b ((h as (rr, ForwardDeclared, n, fl)) :: t) =
            if name = n
            then
              let in
                (* because get returned r for this name *)
                if rr <> r then raise Params "impossible" else ();
                lr := rev b @ ((rr, Declared(default, f cmd), name, flags) :: t);
                r
              end
            else g (h :: b) t
            | g b (h :: t) = g (h :: b) t
        in
          g nil (!lr)
        end

  (* Same as make, but forward declares if it's not there. *)
  fun use_ (lr : ('output ref * ('output * (string * string * 'extra) option) state * 
                  string * 'flag) list ref)
           (flags : 'flag)
           (default : 'output)  (* this is the default for the type of arg.
                                   in idiomatic uses it is never accessed. *)
           (name : string)
           : 'output ref =
    case get lr name of
      NONE =>
        let
          val h = ref default 
        in
          lr := ((h, ForwardDeclared, name, flags) :: !lr);
          h
        end
    | SOME r => r

  fun twotothree NONE = NONE
    | twotothree (SOME (a,b)) = SOME(a,b,())

  val getflag = get flags
  val flag = make twotothree flags ()
  val use_flag = use_ flags () false

  val getparam = get params
  val param = make twotothree params ()
  val use_param = use_ params () ""

  val getparamlist = get paramlists
  val paramlist = make I paramlists false
  val use_paramlist = use_ paramlists false nil

  val getparamacc = get paramlists
  val paramacc = make I paramlists true
  val use_paramacc = use_ paramlists true nil

  fun plarg (r, Declared(_, SOME(_, _, c)), _, b) = (r, c, b)
    | plarg _ = raise BadOption "impossible"

  fun asint def (ref a) = getOpt (Int.fromString a, def)

  val table = StringUtil.table 75

  fun usage () =
    let
        fun smp ((cl, doc, _)) s = [cl, s, doc]
        fun pl ((cl, doc, c)) s = [cl, str c, s, doc]
        (* XXX fail for undeclared flags..? *)
        fun f ml s ts l = rev (foldr (fn ((_, Declared(d, SOME xx), _, _), b) =>
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

  datatype res =
      ExactArgs of string list
    | Bad of string

  fun require n =
      let
          val l = docommandline ()
      in
          if length l = n
          then ExactArgs l
          else Bad ("Expected " ^ Int.toString n ^ " args; got " ^
                    Int.toString (length l))
      end handle BadOption s => Bad s

  fun nope u s =
      let in
          TextIO.output(TextIO.stdErr, s ^ "\n");
          TextIO.output(TextIO.stdErr, "Usage: " ^ u ^ "\n\n");
          TextIO.output(TextIO.stdErr, usage ())
      end
          
  fun main0 u go =
      case require 0 of
        ExactArgs _ => ignore (go ())
      | Bad s => nope u s

  exception Impossible
  fun main1 u go =
      case require 1 of
        ExactArgs [a] => ignore (go a)
      | ExactArgs _ => raise Impossible
      | Bad s => nope u s

  fun main2 u go =
      case require 2 of
        ExactArgs [a, b] => ignore (go (a, b))
      | ExactArgs _ => raise Impossible
      | Bad s => nope u s

  fun main3 u go =
      case require 3 of
        ExactArgs [a, b, c] => ignore (go (a, b, c))
      | ExactArgs _ => raise Impossible
      | Bad s => nope u s

  fun main4 u go =
      case require 4 of
        ExactArgs [a, b, c, d] => ignore (go (a, b, c, d))
      | ExactArgs _ => raise Impossible
      | Bad s => nope u s

  fun main5 u go =
      case require 5 of
        ExactArgs [a, b, c, d, e] => ignore (go (a, b, c, d, e))
      | ExactArgs _ => raise Impossible
      | Bad s => nope u s

  fun main u go =
      ignore (go (docommandline ())) handle BadOption s => nope u s

end
