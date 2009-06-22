
(* This library is for declaring and parsing command-line parameters.
   Parameters are identified by name and can be shared by several
   structures. There are a few different standard kinds of parameters.

   Typically, you use this by writing at the top of each structure
   that needs to refer to myflag:

   val myflag = Params.flag false NONE "myflag" 

   or, if you want this parameter to be settable from the command line,

   val myflag = Params.flag false (SOME ("-myflag", 
                                         "Turns on my flag.")) "myflag"

   Now you can test the flag during the execution of your program:

   if !myflag then ...
              else ...

   (of course, !myflag means to dereference the ref cell, not logical
   not.)

   The use_ variants get a reference to the flag data by its name
   without providing a default. If no other code calls the standard
   version to give it a default value and docstring, then docommandline
   will fail.
*)

signature PARAMS =
sig

  exception BadOption of string
  (* Raised when the program is buggy (uses but does not define flags) *)
  exception Params of string

  (* flag default commandline-spec name

     adds a new flag to the collection (or retrieves
     a pre-existing one with that name) and returns
     a reference to its value (initialized to the default).

     If commandline-spec is SOME(s1, s2) then specifying
     s1 on the command line sets the flag to the opposite
     of its default. s2 is used as help text in the usage message.
  *)
  val flag : bool -> (string * string) option -> string -> bool ref
  val use_flag : string -> bool ref

  (* param default commandline-spec name

     similar as above, but for string parameters.
     
     If commandline-spec is SOME(s1, s2) then specifying

     s1 new-value

     on the command line sets the param to new-value.
  *)
  val param : string -> (string * string) option -> string -> string ref
  val use_param : string -> string ref

  (* paramlist default commandline-spec name 
     
     Like param, but for lists of strings.
     If commandline-spec is SOME(s1, s2, c) then specifying
     
     s1 valuelist
     
     on the command line sets s1 to the list of elements within valuelist.
     (valuelist is a string with elements of the list separated by the
      separator char c (as String.fields).)

     For instance:

     val users = paramlist nil (SOME ("-users", 
                                      "a comma-separated lists of users", #",")) "users"
  
     Allows the user to invoke the program like this:

     ./program -users root,tom,metroid

   *)
  val paramlist : string list -> (string * string * char) option -> string -> string list ref
  val use_paramlist : string -> string list ref

  (* paramlist default commandline-spec name

     Like paramlist, but each time the parameter appears on the command line, the
     arguments are pushed onto the list instead of replacing it.

     ./hemlock -I .,/usr/include/hemlock

     *)
  val paramacc : string list -> (string * string * char) option -> string -> string list ref
  val use_paramacc : string -> string list ref

(* XXX to implement this I need to use exn trick, I think.
  (* general f init commandline-spec name

     This is a generalized version of the above. It allows an arbitrary computation to be
     performed on the incoming string (and some arbitrary item 'b). 
  *)
  val general : (string * 'b -> 'a option) -> 'a -> (string * string * 'b) option -> string -> 'a ref
*)

  (* get a reference to the flag or parameter, if it exists. *)
  val getflag : string -> bool ref option
  val getparam : string -> string ref option
  val getparamlist : string -> string list ref option

  (* get a parameter as an integer. Supply a default in case it
     is malformed. *)
  val asint : int -> string ref -> int

  (* returns usage information as a string, listing all public
     flags/parameters and their documentation string *)
  val usage : unit -> string

  (* process the command line, setting flags and parameters.
     returns strings that aren't flags or parameters.
   
     raises BadOption "-s needs a parameter" if given a parameter
     -s as the last argument (but no corresponding value).
   *)
  val docommandline : unit -> string list

  (* main0 usage go
     Main function taking no arguments. Parses command line arguments
     with docommandline. If there are too many, prints usage string and 
     exits. Otherwise, calls the go function. *)
  val main0 : string -> (unit -> 'a) -> unit

  (* main1 usage go
     Same, for a function taking a single command-line argument. *)
  val main1 : string -> (string -> 'a) -> unit
  (* etc. *)
  val main2 : string -> (string * string -> 'a) -> unit
  val main3 : string -> (string * string * string -> 'a) -> unit
  val main4 : string -> (string * string * string * string -> 'a) -> unit
  val main5 : string -> (string * string * string * string * string -> 'a) -> unit

  (* Takes any number of arguments; never fails unless parsing fails *)
  val main : string -> (string list -> 'a) -> unit

end
