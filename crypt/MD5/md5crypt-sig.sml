
(* Implementation of the PAM MD5 crypt function.
   These passwords look like this in the shadow file:

   $1$abcdefgh$WBDZcnKCuiIJlhjvL0dyI.

   Where "abcdefgh" is the salt, and "WBDZcnKCuiIJlhjvL0dyI."
   is the MD5-based hash.

   I did not invent this crazy algorithm!
   This interface and its implementation are in the public domain.
      - Tom 7                13 Jun 2001
*)   
   

signature MD5CRYPT =
sig

  exception Error of string
  
  (* $1$salt$hash to (salt, hash).
     Verifies that it is in the proper format. *)
  val getfields : string -> string * string

  (* crypt salt password 

     returns the MD5 hashed password using the given 
     salt (typically 8 chars). *)

  val crypt : string -> string -> string


end
