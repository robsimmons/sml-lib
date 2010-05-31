signature RSS =
sig

    exception RSS of string

    (* Get the list of items in the tree. Can raise RSS if the XML
       does not denote an RSS document, but it's pretty permissive
       since RSS implementations differ widely. If a guid is not
       present, tries to cook one up from the link and date. If
       the description is not present, copies it from the title. *)
    val items : XML.tree -> { guid : string,
                              link : string option,
                              title : string,
                              description : string,
                              date : Date.date,
                              body : XML.tree list } list

    (* Parses RSS standard date string, given in RFC 822.
       It's the strftime format "%a, %d %b %Y %H:%M:%S %z". *)
    val parsedate : string -> Date.date option

    (* Find the first tree of the form <elt>text</elt>,
       where elt matches the first argument, returning SOME text.
       If no such tree, return NONE. *)
    val getfirst : string -> XML.tree list -> string option

end