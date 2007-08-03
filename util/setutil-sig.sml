
signature SETUTIL =
sig

  type item
  type set

  val fromlist : item list -> set
  val tolist : set -> item list

  val mappartial : (item -> item option) -> set -> set

end
