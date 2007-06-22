val _ =
  let 
    val m = MySQL.connect "root" NONE
  in
    (* queries ... *)

    MySQL.close m
  end
