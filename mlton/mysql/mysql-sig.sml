
(* Minimal client interface for mysql 
   (http://mysql.org/) *)
signature MYSQL =
sig

  exception MySQL of string

  type mysql
  type result

  datatype entry =
    Int of int
  | BigInt of IntInf.int
  (* used for  *)
  | String of string
  (* XXX others like double, time -- add 'em! *)
  | Unknown

  (* Returns a non-empty string describing the current error, or NONE if
     none. Most functions simply raise MySQL with this error message.
     The error message is reset by most calls to the backend. *)
  val error : mysql -> string option

  (* connect user password *)
  val connect : string -> string option -> mysql

  (* connectex host user password port unixsocketname *)
  val connectex : string -> string -> string -> int -> string -> mysql

  (* must call this to avoid leaking resources. 
     after calling it, should discard the argument and all aliases. *)
  val close : mysql -> unit

  (* query m q *)
  val query : mysql -> string -> result option

  (* you must free the results of queries *)
  val free : result -> unit

  (* readone m r 
     reads one row from the result set. *)
  val readone : mysql -> result -> entry option list option

  (* readall m r
     read the entire matrix of results *)
  val readall : mysql -> result -> entry option list list
    
  (* TO-DO: a combinator language for expressing the types of
     columns, so that read can return a list of records directly *)
end