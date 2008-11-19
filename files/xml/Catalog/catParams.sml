




signature CatParams =
   sig
      val O_CATALOG_FILES  : Uri.Uri list ref
      val O_PREFER_SOCAT   : bool ref
      val O_PREFER_SYSID   : bool ref
      val O_PREFER_CATALOG : bool ref
      val O_SUPPORT_REMAP  : bool ref
      val O_CATALOG_ENC    : Encoding.Encoding ref

      val catError : CatError.Position * CatError.CatError -> unit
   end

