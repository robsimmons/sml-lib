(* Needs StringUtil. *)

(* The spec for NetPBM formats is pretty fly-by-night, particularly
   with regard to comments. It encourages the reader to be as lenient
   as possible. *)
structure NetPBM :> NETPBM =
struct

    exception NetPBM of string
    structure R = Reader

    type ('radix, 'radixrange) graphic = 
        { width : int,
          height : int,
          max : 'radixrange,
          data : 'radix Array.array }

    val trim = StringUtil.losespecl StringUtil.whitespec o 
               StringUtil.losespecr StringUtil.whitespec

    fun tok r = 
        case R.token StringUtil.whitespec r of
            SOME "#" =>
            let in
                ignore (R.line r);
                tok r
            end
          | t => t

    fun int r : int =
        case Option.map Int.fromString (tok r) of
            NONE => raise NetPBM "expected int."
          | SOME NONE => raise NetPBM "bad int."
          | SOME (SOME i) => i

    fun readpgm r =
        case tok r of
            SOME "P5" => 
                (* would be easy, but I'm not using it now *)
                raise NetPBM "Sorry, P5 not implemented yet"
          | SOME "P2" =>
             let
                 val w = int r
                 val h = int r
                 val g = int r
             in
                 { width = w,
                   height = h,
                   max = g,
                   data = Array.tabulate (w * h, fn _ => int r) }
             end
          | _ => raise NetPBM "For PGM, should be P2 or P5"


end
