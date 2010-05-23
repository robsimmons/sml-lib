val _ =
  let 
    val m = MySQL.connect "root" NONE
      handle e as MySQL.MySQL s => (print ("error: " ^ s ^ "\n"); raise e)
  in
    (* queries ... *)

    MySQL.close m
  end
