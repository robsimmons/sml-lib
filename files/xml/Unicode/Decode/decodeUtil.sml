(*
require "basis.__word";
require "basis.__word8";

require "chars";
require "decodeBasic";
require "decodeError";
*)

(*--------------------------------------------------------------------------*)
(* Structure: DecodeUtil                                                    *)
(*                                                                          *)
(* Exceptions raised by functions in this structure:                        *)
(*   combineSurrogates        : none                                        *)
(*   combineUcs4big           : none                                        *)
(*   combineUcs4little        : none                                        *)
(*   combineUcs4strangeBig    : none                                        *)
(*   combineUcs4strangeLittle : none                                        *)
(*   combineUtf16big          : none                                        *)
(*   combineUtf16little       : none                                        *)
(*   isLowSurrogate           : none                                        *)
(*   isHighSurrogate          : none                                        *)
(*   isSurrogate              : none                                        *)
(*--------------------------------------------------------------------------*)
signature DecodeUtil =
   sig
      val isSurrogate       : UniChar.Char -> bool
      val isLowSurrogate    : UniChar.Char -> bool
      val isHighSurrogate   : UniChar.Char -> bool
      val combineSurrogates : UniChar.Char * UniChar.Char -> UniChar.Char
   end

structure DecodeUtil : DecodeUtil =
   struct
      open UniChar DecodeFile DecodeError

      fun isSurrogate c = Chars.orb(c,0wx7FF)=0wxDFFF
      fun isLowSurrogate c = Chars.orb(c,0wx3FF)=0wxDFFF
      fun isHighSurrogate c = Chars.orb(c,0wx3FF)=0wxDBFF
      fun combineSurrogates(hi,lo) = (hi-0wxD800)*0wx400+lo+0wx2400 : Char
   end
