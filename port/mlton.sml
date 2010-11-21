
(* the Port structure for mlton *)
structure Port =
struct 

  (* Have to compile with argument
      -const 'Exn.keepHistory true' 
     in order for this to return anything useful. *)
  val exnhistory = MLton.Exn.history

  val fast_eq = MLton.eq

  val defaultTopLevelHandler = MLton.Exn.defaultTopLevelHandler
  val setTopLevelHandler = MLton.Exn.setTopLevelHandler

end