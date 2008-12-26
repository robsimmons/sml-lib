
(* the Port structure for mlton *)
structure Port =
struct 

  val exnhistory = MLton.Exn.history

  val fast_eq = MLton.eq

  val defaultTopLevelHandler = MLton.Exn.defaultTopLevelHandler
  val setTopLevelHandler = MLton.Exn.setTopLevelHandler

end