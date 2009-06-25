
(* simple tokenizer for text files. attempts to do the "right" thing
   with as little work as possible. If it's not right, you'll have to
   write your own... *)

(* The tokenizer makes no attempt to report that your language is ambiguous,
   useless, etc. 

 *)

signature SIMPLETOK =
sig

  type 'a tokenizer
    
  (* Allow decimal integers, 0xHEX and 0bBINARY *)
  datatype intstyle =
      ISStandard
    | ISHex 
    | ISBinary

  (* Can string literals span multiple lines? *)
  datatype stringstyle =
      SSStandard
    | SSMultiline

  (* Comments bracketed or terminated by newlines. *)
  datatype commentstyle =
      CSBracketed of string * string
    | CSLine of string


  (* Only one way to parse floating point constants. 
     XXX It's not implemented yet. *)
  datatype floatstyle =
      FSStandard

  (* XXX not implemented. *)
  datatype charstyle =
      CHSPrefix of char
    | CHSQuoted of char 
    | CHSBracketed of string * string 

  exception SimpleTok of string

  val empty : unit -> 'a tokenizer

  (* default other ints strings

     where int tokens are passed to 'ints',
     "string tokens" are passed to 'strings',
     and everything else is passed to 'other'.
     *)
  val default : (string -> 'a) -> (int -> 'a) -> (string -> 'a) -> 'a tokenizer


  (* setint intmaker styles negation-character

     Take a style and change the way it tokenizes ints. *)
  val setint : 'a tokenizer -> (int -> 'a) -> 
               intstyle list -> char option -> 
                  'a tokenizer
  
  val settokens : 'a tokenizer -> (string * 'a) list -> 'a tokenizer

  val setother : 'a tokenizer -> (string -> 'a) -> 'a tokenizer

  val setstring : 'a tokenizer -> (string -> 'a) -> stringstyle list -> 'a tokenizer

  val setchar : 'a tokenizer -> (char -> 'a) -> charstyle list -> 'a tokenizer

  val setfloat : 'a tokenizer -> (real -> 'a) -> floatstyle list -> 'a tokenizer

  val setcomment : 'a tokenizer -> commentstyle list -> 'a tokenizer

  val setsep : 'a tokenizer -> (char -> bool) -> 'a tokenizer

  (* Simple streams. *)
  val stringstream : string -> char Stream.stream
  val filestream   : string -> char Stream.stream

  (* XXX val linestream : char Stream.stream -> char Stream.stream Stream.stream *)

  val parser : 'a tokenizer -> ('a, char) Parsing.parser

  val tokenize : 'a tokenizer -> char Stream.stream -> 'a list

end