


(*--------------------------------------------------------------------------*)
(* In order to be used as a dictinary/symbol table key, a type must have a  *)
(* null value, hash to words, must be comparable and printable.             *)
(*--------------------------------------------------------------------------*)
signature Key = 
   sig
      type Key 

      val null     : Key
      val hash     : Key -> word
      val compare  : Key * Key -> order
      val toString : Key -> string
   end
