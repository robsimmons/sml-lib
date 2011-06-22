(* XXX: Lots more tests! *)
structure StringUtilTest =
struct

  exception Nope

  val () =
  let
      val _ = "asdf 1234" = StringUtil.lcase "AsDf 1234" orelse raise Nope
      val _ = "1234 ASDF" = StringUtil.ucase "1234 asDF" orelse raise Nope
      val _ = StringUtil.whitespec #" " orelse raise Nope
      val _ = StringUtil.whitespec #"\n" orelse raise Nope
      val _ = StringUtil.whitespec #"z" andalso raise Nope
      val _ = StringUtil.whitespec #"." andalso raise Nope
          
      val _ = "a.bb.ccc" = StringUtil.delimit "." ["a", "bb", "ccc"] orelse raise Nope
      val _ = "" = StringUtil.delimit "-" nil orelse raise Nope
      val _ = StringUtil.charspec "a-z" #"s" orelse raise Nope
      val _ = StringUtil.charspec "a-z" #" " andalso raise Nope
  in
      ()
  end handle e => (print (exnName e ^ ": " ^ exnMessage e ^ "\n"); raise e)




  val () = print "StringUtil tests passed!\n"
end
