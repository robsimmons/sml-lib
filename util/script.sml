structure Script =
struct

    fun linesfromfile f =
        let val s = StringUtil.readfile f
            val l = String.tokens (fn #"\n" => true
                                    | #"\r" => true
                                    | _ => false) s
        in
            List.mapPartial (fn s =>
                             case StringUtil.losespecl StringUtil.whitespec
                                 (StringUtil.losespecr StringUtil.whitespec s) of
                                 "" => NONE
                               | w => SOME w) l
        end

    

end