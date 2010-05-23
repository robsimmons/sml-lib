
(* ref: http://dev.mysql.com/doc/mysql/en/c-api-functions.html *)

structure MySQL :> MYSQL =
struct

  exception MySQL of string

  type ptr = MLton.Pointer.t
  val null = MLton.Pointer.null

  (* C pointers to mysql, mysql_result, *)
  type mptr = ptr
  type rptr = ptr

  type mysql = mptr option ref
  type result = rptr option ref

  fun protect f (ref (SOME m)) = f m
    | protect _ (ref NONE) = raise MySQL "protected function called on closed connection or freed result"

  val mysql_close = _import "mysql_close" : ptr -> unit ;
  val mysql_error = _import "mysql_error" : mptr -> ptr ;

  (* Read a null-terminated C string into an ML string.
     Linear time. Unsafe, obviously! *)
  fun fromcstring (ptr : ptr) : string =
    let
      fun getsize n = if MLton.Pointer.getInt8 (ptr, n) = 0
                      then n
                      else getsize (n + 1)
    in
      CharVector.tabulate (getsize 0, fn x => chr (Word8.toInt (MLton.Pointer.getWord8 (ptr, x))))
    end

  fun error' (m : mptr) =
    (case fromcstring (mysql_error m) of
       "" => NONE
     | s => SOME s)
      
  val error = protect error'

  fun getError (m : mptr) =
    case error' m of
      NONE => "(no error?)"
    | SOME s => s

  fun a2v a = CharVector.tabulate (CharArray.length a, fn x => CharArray.sub(a, x))

  datatype entry =
    Int of int
  | BigInt of IntInf.int
  (* used for  *)
  | String of string
  (* XXX others like double, time -- add 'em! *)
  | Unknown

  fun close (r as ref (SOME m)) =
    let in
      mysql_close m;
      r := NONE
    end
    | close _ = raise MySQL "already closed"

  fun free (rr as ref (SOME r)) =
    let 
      val mysql_free_result = _import "mysql_free_result" : rptr -> unit ;
    in
      mysql_free_result r;
      rr := NONE
    end
    | free _ = raise MySQL "already freed"

  fun connectex host user passwd port unixsocket =
    let
      val mysql_init = _import "mysql_init" : ptr -> mptr ;
      val mysql_real_connect = _import "mysql_real_connect"
        (* conn       host   user    password    db(null)      port  unixsocketname  flags *)
         : mptr * string * string * string * ptr * int * string *        int ->
            SysWord.word ;

      (* allocates *)
      val m = mysql_init null
    in
      case mysql_real_connect (m, String.toCString host, 
                               String.toCString user, 
                               String.toCString passwd, 
                               null, port, 
                               String.toCString unixsocket, 0) of
        0w0 =>
          let 
            val err = ("Error connecting: " ^ getError m)
          in
            (* ok to try to close? we need to deallocate it *)
            mysql_close m;
            raise MySQL err
          end
      | _ => ref (SOME m)
    end

  fun connect user password =
    let
      val mysql_init = _import "mysql_init" : ptr -> mptr ;

      (* there's no easy, type safe way to tell mlton that each of these
         parameters is a string-or-null. So I import two versions
         here, and hope you don't have a desire to make a call with a
         weird combination of null and non-null arguments. *)
      val mysql_real_connect_np = _import "mysql_real_connect"
        : mptr * ptr * string * ptr * ptr * int * ptr * int ->
        SysWord.word ;
      val mysql_real_connect = _import "mysql_real_connect"
        : mptr * ptr * string * string * ptr * int * ptr * int ->
        SysWord.word ;
      val m = mysql_init null
    in
      case (case password of
              NONE => mysql_real_connect_np (m, null,
                                             String.toCString user, 
                                             null, 
                                             null, 0, 
                                             null, 0)
            | SOME pw => mysql_real_connect (m, null,
                                             String.toCString user, 
                                             String.toCString pw, 
                                             null, 0, 
                                             null, 0)) of
        0w0 =>
          let 
            val err = "Error connecting: " ^ getError m
          in
            (* ok to try to close? we need to deallocate it *)
            mysql_close m;
            raise MySQL err
          end
      | _ => ref (SOME m)
    end

  fun query' m q =
    let
      val mysql_real_query = _import "mysql_real_query" : mptr * string * int -> int ;
    in
      case mysql_real_query (m, q, size q) of
        0 =>
          (* ok, so see if we have a result set. *)
          let
            val mysql_store_result = _import "mysql_store_result" : mptr -> rptr ;
            val r = mysql_store_result m
          in
            if r = null
            then NONE (* XXX could check errors; call mysql_field_count *)
            else SOME(ref (SOME r))
          end
      (* unfortunately hard to check error codes here, as they are #defines in 
         mysql.h. *)
      | _ => raise MySQL ("error in query: " ^ getError m)
    end

  (* Convert from a string to these various types *)
  fun expect_some f x =
    case f x of
      NONE => raise MySQL "malformed data from server"
    | SOME t => t

  val readint = Int o expect_some Int.fromString
  val readbig = BigInt o expect_some IntInf.fromString
  val readstring = String
  fun readunknown _ = Unknown

  fun readone' _ =
    protect (fn r =>
             let
               val mysql_num_fields = _import "mysql_num_fields" : rptr -> int ;
               val mysql_fetch_row = _import "mysql_fetch_row" : rptr -> ptr ;
               val mysql_fetch_lengths = _import "mysql_fetch_lengths" 
                 : rptr -> ptr ;

               val n = mysql_num_fields r


               val get_length_i = _import "mlton_mysql_get_length_i" 
                 : ptr * int -> int ;

               val get_data_i = _import "mlton_mysql_get_data_i"
                 (* row  idx   dest               len *)
                 : ptr * int * CharArray.array * int -> unit ;

               val is_null_i = _import "mlton_mysql_is_null_i"
                 : ptr * int -> bool ;

               val row = mysql_fetch_row r
             in
               if null = row
               then NONE
               else
                 let
                   val lengths = mysql_fetch_lengths r

                   fun getdata row i =
                     if is_null_i (row, i)
                     then NONE
                     else let
                            val len = get_length_i (lengths, i)
                            val a = CharArray.array(len, #"*")
                          in
                            get_data_i (row, i, a, len);
                            SOME (a2v a)
                          end

                   fun tag_data i =
                     let
                       val mysql_get_fieldtype_i =
                         _import "mlton_mysql_get_fieldtype_i"
                         : rptr * int -> int ;
                         
                       val rf =
                         (* these numbers are just constants
                            cooked up for interfacing with
                            this C stub -- see mysql.c *)
                         case mysql_get_fieldtype_i (r, i) of
                           0 => raise MySQL "bad fieldtype"
                         | 1 => readint (* tiny int *)
                         | 2 => readint (* short int *)
                         | 3 => readint (* long int *)
                         | 4 => readint (* int24 *)
                         | 5 => readbig (* longlong *)
                             
                         | 14 => readstring (* string *)
                         | 15 => readstring (* var_string *)
                         | 16 => readstring (* blob *)
                         | 17 => readstring (* enum *)
                             
                         | 20 => readint (* char *)
                             
                         | _ => readunknown (* anything else *)
                     in
                       rf
                     end
                   
                   fun read i =
                     if i = n then nil
                     else let
                            val so = getdata row i
                            val s = Option.map (tag_data i) so
                          in
                            s :: read (i + 1)
                          end
                 in
                   SOME (read 0)
                 end
             end)

  val query = protect query'
  val readone = protect readone'

  (* PERF much of the work done in readone can be done just once
     and reused *)
  fun readall m r =
    case readone m r of
      NONE => nil
    | SOME l => l :: readall m r

end