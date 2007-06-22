signature EDIT_DISTANCE_ARG =
sig
  type ch
  type str (* vector of ch *)
  val eq : ch * ch -> bool
  val len : str -> int
  val sub : str * int -> ch
    
               (* source, dest *)
  val MODIFY_COST : ch * ch -> int
  val INSERT_COST : ch -> int
  val DELETE_COST : ch -> int
end

signature EDIT_DISTANCE =
sig
  type ch
  type str
  datatype edit =
    Insert of int * ch
  | Delete of int
  | Modify of int * ch

  (* minedit s1 s2
     return the minimum edit cost and a minimum edit from s1 to s2 *)
  val minedit : str * str -> int * edit list

end