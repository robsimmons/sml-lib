signature CatDtd = 
   sig
      type Dtd

      val baseIdx     : int
      val delegateIdx : int
      val extendIdx   : int
      val mapIdx      : int
      val remapIdx    : int

      val hrefIdx     : int
      val pubidIdx    : int
      val sysidIdx    : int

      val Index2AttNot  : Dtd -> int -> UniChar.Data
      val Index2Element : Dtd -> int -> UniChar.Data
   end

structure CatDtd =  
   struct
      open Dtd
	 
      val baseGi     = UniChar.String2Data "Base"
      val delegateGi = UniChar.String2Data "Delegate"
      val extendGi   = UniChar.String2Data "Extend"
      val mapGi      = UniChar.String2Data "Map"
      val remapGi    = UniChar.String2Data "Remap"

      val hrefAtt    = UniChar.String2Data "HRef"
      val pubidAtt   = UniChar.String2Data "PublicId"
      val sysidAtt   = UniChar.String2Data "SystemId"

      fun initDtdTables () = 
	 let 
	    val dtd = Dtd.initDtdTables()
	    val _ = app (ignore o (Element2Index dtd)) [baseGi,delegateGi,extendGi,mapGi,remapGi]
	    val _ = app (ignore o (AttNot2Index dtd)) [hrefAtt,pubidAtt,sysidAtt]
	 in dtd
	 end

      local 
	 val dtd = initDtdTables()
      in 
	 val baseIdx     = Element2Index dtd baseGi
	 val delegateIdx = Element2Index dtd delegateGi
	 val extendIdx   = Element2Index dtd extendGi
	 val mapIdx      = Element2Index dtd mapGi
	 val remapIdx    = Element2Index dtd remapGi

	 val hrefIdx     = AttNot2Index dtd hrefAtt
	 val pubidIdx    = AttNot2Index dtd pubidAtt
	 val sysidIdx    = AttNot2Index dtd sysidAtt
      end
   end
