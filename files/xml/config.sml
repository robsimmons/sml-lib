structure Config =
   struct
      (*---------------------------------------------------------------------*)
      (* The OS command for retrieving a URI from the internet and storing   *)
      (* it in a local file, where                                           *)
      (*   %1 is replaced by the URI.                                        *)
      (*   %2 is replaced by the local filename.                             *)
      (* It is recommended that the command exits with failure in case the   *)
      (* URI cannot be retrieved. If the command generates a HTML error      *)
      (* message instead (like, e.g., lynx), this HTML file is considered    *)
      (* to be XML and will probably cause a mess of parsing errors. If you  *)
      (* don't need URI retrieval, use "exit 1" which always fails.          *)
      (* Sensible values are, e.g.:                                          *)
      (* val retrieveCommand = "wget -qO %2 %1"                              *)
      (* val retrieveCommand = "got_it -o %2 %1"                             *)
      (* val retrieveCommand = "urlget -s -o %2 %1"                          *)
      (*---------------------------------------------------------------------*)
      val retrieveCommand = "wget -qO %2 %1"                            
   end
