(* Copyright (c) 2010 Tom Murphy VII.
   This file only: Distribute freely.

   Like IgnoreHooks, but prints a trace message instead of doing nothing. *)
structure TraceHooks =
struct
  type AppData = unit
  type AppFinal = unit

  fun hookXml(a,_) = (print "hookXML\n"; a)
  fun hookFinish a = (print "hookFinish\n"; a)

  fun hookError(a, (_, err)) = 
      (print "hookError:\n"; 
       app (fn x => print (" .. " ^ x ^ "\n")) (ErrorMessage.errorMessage err);
       a)

  fun hookWarning(a, (_, err)) = 
      (print "hookWarning:\n"; 
       app (fn x => print (" .. " ^ x ^ "\n")) (ErrorMessage.warningMessage err);
       a)

  fun hookProcInst(a,_) = (print "hookProcInst\n"; a)
  fun hookComment(a,_) = (print "hookComment\n"; a)
  fun hookWhite(a,_) = (print "hookWhite\n"; a)
  fun hookDecl (a,_) = (print "hookDecl\n"; a)

  fun hookStartTag(a,_) = (print "hookStartTag\n"; a)
  fun hookEndTag(a,_) = (print "hookEndTag\n"; a)
  fun hookCData(a,_) = (print "hookCData\n"; a)
  fun hookData(a,_) = (print "hookData\n"; a)

  fun hookCharRef(a,_) = (print "hookCharRef\n"; a)
  fun hookGenRef(a,_) = (print "hookGenRef\n"; a)
  fun hookParRef(a,_) = (print "hookParRef\n"; a)
  fun hookEntEnd(a,_) = (print "hookEntEnd\n"; a)

  fun hookDocType(a,_) = (print "hookDocType\n"; a)
  fun hookSubset(a,_) = (print "hookSubset\n"; a)
  fun hookExtSubset(a,_) = (print "hookExtSubset\n"; a)
  fun hookEndDtd(a,_) = (print "hookEndDtd\n"; a)

end