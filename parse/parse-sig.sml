
signature BASIC_PARSING =
sig

    (* Parser with token type 't, result type 'a *)
    type ('a,'t) parser

    (* succeed with given value *)
    val succeed : 'a -> ('a,'t) parser
    (* fail immediately *)
    val fail : ('a,'t) parser

    (* check for end of input *)
    val done : 'a -> ('a,'t) parser
    (* admit anything, provided there's something on the input *)
    val any : ('t,'t) parser

    (* sequential successful composition of parsers *)
    val -- : ('a,'t) parser * ('a -> ('b,'t) parser) -> ('b,'t) parser
    (* sequential failing composition of parsers *)
    val ## : ('a,'t) parser * (Pos.pos -> ('a,'t) parser) -> 
              ('a,'t) parser

    (* grab position *)
    val !! : ('a,'t) parser  -> ('a * Pos.pos,'t) parser

    (* get position *)
    val get : (Pos.pos -> ('a, 't) parser) -> ('a, 't) parser

    (* to handle mutually-recursive parsers *)
    val $ : (unit -> ('a,'t) parser) -> ('a,'t) parser

    (* to construct a recursive parser *)
    val fix : (('a,'t) parser -> ('a,'t) parser) -> ('a,'t) parser

    (* re-parse same input, given result of first parse *)
    val lookahead : ('a,'t) parser -> ('a -> ('b,'t) parser) -> 
                      ('b,'t) parser

    (* parse this stream before reading any other input *)
    val push : ('t * Pos.pos) Stream.stream -> 
                ('a,'t) parser -> ('a, 't) parser

    (* parse a stream *)
    val parse : ('a,'t) parser -> ('t * Pos.pos) Stream.stream -> 
                 'a option

    (* transform p s

       parses consecutive maximal prefixes of s with p as many times
       as possible, outputting the results as a stream *)
    val transform : ('a,'t) parser -> ('t * Pos.pos) Stream.stream -> 
                     'a Stream.stream

end

signature PARSING =
sig

  include BASIC_PARSING

  (*
    infixr 4 << >>
    infixr 3 &&
    infix  2 -- ##
    infix  2 wth suchthat return guard when
    infixr 1 ||
  *)

  (* sequential composition *)
  val &&       : ('a,'t) parser * ('b,'t) parser -> ('a * 'b,'t) parser 
  (* alternation *)
  val ||       : ('a,'t) parser * ('a,'t) parser -> ('a,'t) parser 

  (* apply function to success value *)
  val wth      : ('a,'t) parser * ('a -> 'b) -> ('b,'t) parser 
  (* succeed only if check on successful is true *)
  val suchthat : ('a,'t) parser * ('a -> bool) -> ('a,'t) parser
  (* specify success value *)
  val return   : ('b,'t) parser * 'a -> ('a,'t) parser

  (* apply function to failure position *)
  val guard    : ('a,'t) parser * (Pos.pos -> 'b) -> ('a,'t) parser

  (* n-ary sequential composition *)
  val seq      : ('a,'t) parser list -> ('a list,'t) parser
  (* n-ary alternation *)
  val alt      : ('a,'t) parser list -> ('a,'t) parser

  (* ensure that next token satisfies condition, yielding that token *)
  val satisfy  : ('t -> bool) -> ('t,'t) parser 

  (* succeed only if function returns SOME a *)
  val maybe    : ('t -> 'a option) -> ('a, 't) parser

  (* succeed with mapped result if SOME, otherwise fail. *)
  val when     : ('a, 't) parser * ('a -> 'b option) -> ('b, 't) parser

  (* XXX these require equality on tokens; yech! *)

  (* check for a given token *)
  val literal  : ''t -> (''t,''t) parser
  (* check for a given list of tokens *)
  val string   : ''t list -> (''t list,''t) parser
  (* check for one of a list of tokens *)
  val oneof    : ''t list -> (''t,''t) parser

  (* optional parse, yielding an optional result *)
  val opt      : ('a,'t) parser -> ('a option,'t) parser 
  (* optional parse, with given action on success *)
  val optional : ('a -> 'b) -> 'b -> ('a,'t) parser -> ('b,'t) parser

  (* zero or more copies *)
  val repeat   : ('a,'t) parser -> ('a list,'t) parser 
  (* one or more *)
  val repeat1  : ('a,'t) parser -> ('a list,'t) parser 
  (* exact number *)
  val repeatn  : int -> ('a, 't) parser -> (unit, 't) parser
  (* avoid building result *)
  val repeati  : ('a, 't) parser -> (unit, 't) parser

  (* parse two things, yielding value of first *)
  val first    : ('a,'t) parser -> ('b,'t) parser -> ('a,'t) parser 
  val <<       : ('a,'t) parser * ('b,'t) parser -> ('a,'t) parser 
  (* ... second *)
  val second   : ('a,'t) parser -> ('b,'t) parser -> ('b,'t) parser 
  val >>       : ('a,'t) parser * ('b,'t) parser -> ('b,'t) parser 
  (* .... middle of three *)
  val middle   : ('a,'t) parser -> ('b,'t) parser -> ('c,'t) parser 
                 -> ('b,'t) parser 

  (* parse one or more, with given separator between items *)
  val separate : ('a,'t) parser -> ('b,'t) parser -> ('a list,'t) parser 
  (* ... zero or more *)
  val separate0: ('a,'t) parser -> ('b,'t) parser -> ('a list,'t) parser 
  (* one or more, allowing trailing separator *)
  val separate': ('a,'t) parser -> ('b,'t) parser -> ('a list,'t) parser 

  (* nested parsers *)
  val join     : (('a,'t) parser,'t) parser -> ('a,'t) parser

  (***** Pre/In/Post-fix utilities *****)

  datatype associativity = Left | Right | Non

  datatype 'a opr =
      Prefix of int * ('a -> 'a)
    | Infix of associativity * int * ('a * 'a -> 'a)
    | Postfix of int * ('a -> 'a)

  datatype 'a fixityitem =
      Atm of 'a
    | Opr of 'a opr

  val parsefixity : ('a fixityitem, 't) parser -> ('a,'t) parser

  (* Same, but also look for adjacent tokens, combining them with
     the supplied function and associativity. *)
  val parsefixityadj : ('a fixityitem, 't) parser -> 
                       associativity -> ('a * 'a -> 'a) -> ('a,'t) parser

  (* Utilities for manipulating intermediate results,

     ie. (IF >> $exp) && (THEN >> $exp) && (ELSE >> $exp) wth If o flat3
     *)

  val flat3 : 'a * ('b * 'c) -> 'a * 'b * 'c
  val flat4 : 'a * ('b * ('c * 'd)) -> 'a * 'b * 'c * 'd
  val flat5 : 'a * ('b * ('c * ('d * 'e))) -> 'a * 'b * 'c * 'd * 'e

end
