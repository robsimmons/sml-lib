structure RSS :> RSS =
struct

    exception RSS of string

    datatype tree = datatype XML.tree

    fun getfirsttagl tag nil = NONE
      | getfirsttagl tag (Text _ :: rest) = getfirsttagl tag rest
      | getfirsttagl tag (Elem ((t, _), trees) :: rest) =
        if tag = t then SOME trees
        else getfirsttagl tag rest

    fun getfirst tag trees =
        case getfirsttagl tag trees of
            SOME [Text s] => SOME s
          | _ => NONE

    fun parsedate s =
        (* "Sat, 16 Jan 2010 19:20:35 -0500" *)
        case String.tokens (fn #" " => true | _ => false) s of
            [weekday_with_comma, day, mon, year, time, offset] =>

                let 
                    val weekday = 
                        if size weekday_with_comma = 4
                           andalso String.sub(weekday_with_comma, 3) = #","
                        then String.substring(weekday_with_comma, 0, 3)
                        else weekday_with_comma (* probably will fail *)

                    fun twodigit (a, aa) =
                        if a > #"9" orelse aa > #"9"
                           orelse a < #"0" orelse aa < #"0"
                        then NONE
                        else SOME ((ord a - ord #"0") * 10 +
                                   (ord aa - ord #"0"))

                in
                    case (explode offset,
                           Date.fromString (String.concat [weekday, " ",
                                                           mon, " ",
                                                           day, " ",
                                                           time, " ",
                                                           year])) of
                         ([tz_dir, h, hh, m, mm], SOME d) =>
                             (case (tz_dir <> #"+" andalso tz_dir <> #"-",
                                    twodigit (h, hh),
                                    twodigit (m, mm)) of
                                (false, SOME hours, SOME mins) =>
                                  let
                                      val offset_time =
                                          Time.fromSeconds 
                                          (LargeInt.fromInt (hours * 3600 + mins * 60))
                                  in
                                      SOME
                                      (Date.date
                                       { year = Date.year d, 
                                         month = Date.month d, 
                                         day = Date.day d,
                                         hour = Date.hour d,
                                         minute = Date.minute d, 
                                         second = Date.second d,
                                         offset = SOME offset_time })
                                  end
                              | _ => NONE)
                       | _ => NONE
                end
          | _ => NONE

    fun items (Elem (("rss", _), channels)) =
        (case getfirsttagl "channel" channels of
           SOME channel =>
              let
                  fun oneitem trees =
                      (case (getfirst "title" trees, getfirst "pubDate" trees) of
                           (SOME title, SOME pubdate) =>
                               let
                                   val description = getfirst "description" trees
                                   val link = getfirst "link" trees
                                   val guid = getfirst "guid" trees

                                   val description = getOpt (description, title)
                                   val guid =
                                       case (guid, link) of
                                           (NONE, NONE) => raise RSS "need either <guid> or <link>"
                                         | (SOME g, _) => g
                                         (* Fake it. *)
                                         | (NONE, SOME l) => l ^ ":" ^ pubdate
                               in
                                   case parsedate pubdate of
                                       NONE => raise RSS ("invalid pubDate " ^ pubdate)
                                     | SOME date => 
                                           { title = title,
                                             description = description,
                                             guid = guid,
                                             body = trees,
                                             link = link,
                                             date = date }
                               end
                         | _ => raise RSS "<title> and <pubDate> tags are absolutely required")

                  fun getitems nil = nil
                    | getitems (Elem(("item", _), trees) :: rest) = oneitem trees :: getitems rest
                    | getitems (_ :: rest) = getitems rest
              in
                  getitems channel
              end

         | NONE => raise RSS ("Expected channel tag somewhere"))
      | items _ = raise RSS "Expected document to be <rss> tag."

end