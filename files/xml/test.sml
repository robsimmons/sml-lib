
structure Test =
struct

    val text =
    "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n" ^
    "<test attribute=\"Standards!\">They make programming <b>better</b>!</test>\n"

    val xml = XML.parsestring text

    val () = print (XML.tostring xml ^ "\n")
end