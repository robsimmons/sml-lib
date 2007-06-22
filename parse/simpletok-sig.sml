
(* simple tokenizer for text files. attempts to do the "right" thing
   with as little work as possible. If it's not right, you'll have to
   write your own... *)

(* The tokenizer makes no attempt to report that your language is ambiguous,
   useless, etc. 

 *)

signature SIMPLETOK =
sig

  type 'a tokenizer
    
  datatype charstyle =
      CHSPrefix of char
    | CHSQuoted of char 
    | CHSBracketed of string * string 
    
  datatype intstyle =
      ISStandard
    | ISHex 
    | ISBinary

  datatype floatstyle =
      FSStandard

  datatype stringstyle =
      SSStandard
    | SSMultiline

  datatype commentstyle =
      CSBracketed of string * string
    | CSLine of string

  exception SimpleTok of string

  val empty : unit -> 'a tokenizer

  (* default other ints strings

     where int tokens are passed to 'ints',
     "string tokens" are passed to 'strings',
     and everything else is passed to 'other'.
     *)
  val default : (string -> 'a) -> (int -> 'a) -> (string -> 'a) -> 'a tokenizer


  (* setint intmaker styles negator 
     *)
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

  val stringstream : string -> char Stream.stream
  val filestream   : string -> char Stream.stream

  (* XXX val linestream : char Stream.stream -> char Stream.stream Stream.stream *)

  val parser : 'a tokenizer -> ('a, char) Parsing.parser

  val tokenize : 'a tokenizer -> char Stream.stream -> 'a list

end