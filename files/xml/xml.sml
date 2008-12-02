
(* Simple parser and pretty-printer for XML. *)
structure XML =
struct

  exception XML of string
  (* ?? *)

  (* XXX get rid of HookData *)
  type tag = string * HookData.AttSpecList

  datatype tree = 
      Text of string
    | Elem of tag * tree list

  val d = Dtd.initDtdTables()

  structure Hooks =
  struct
      open IgnoreHooks
          
      type AppData = tree list * (tag * tree list) list
      type AppFinal = tree
          
      val appstart = (nil, nil)
          
      fun hookStartTag ((content, stack),
                        (dtd, id, atts, _, empty)) =
          let val t = UniChar.Data2String (Dtd.Index2Element d id)
          in
              if empty 
              then (Elem ((t, atts), nil) :: content, stack)
              else (nil, ((t, atts), content) :: stack)
          end

      (* XXX should have message *)
      fun hookEndTag ((_, nil), _) = raise XML "ill-formed: no tag open"
        | hookEndTag ((content, (tag, content') :: stack), _) =
          (Elem (tag, rev content) :: content', stack)

      fun hookData ((content, stack), (_, vec, _)) =
          (Text (UniChar.Vector2String vec) :: content, stack)

      fun hookCData ((content, stack), (_, vec)) =
          (Text (UniChar.Vector2String vec) :: content, stack)

      fun hookCharRef ((content, stack), (_, c, _)) =
          (Text (UniChar.Data2String [c]) :: content, stack)

      fun hookFinish ([tree], nil) = tree
        | hookFinish _ = raise XML "ill-formed: multiple trees?"
  end

  structure Parser = Parse(structure Dtd = Dtd
                           structure Hooks = Hooks
                           structure ParserOptions = ParserOptions ()
                           structure Resolve = ResolveNull)

  fun parsefile file = 
      (Parser.parseDocument (SOME (Uri.String2Uri ((* "file://" ^ *) file))) 
          (SOME d) Hooks.appstart)

end