
(* Simple parser and pretty-printer for XML. *)
structure XML =
struct

  exception XML of string
  (* ?? *)
  type tag = int * HookData.AttSpecList

  datatype tree = 
      Text of string
    | Elem of tag * tree list

  structure Hooks =
  struct
      open IgnoreHooks
          
      type AppData = tree list * (tag * tree list) list
      type AppFinal = tree
          
      val appstart = (nil, nil)
          
      fun hookStartTag ((content, stack),
                        (_, elem, atts, _, empty)) =
          if empty 
          then (Elem ((elem, atts), nil) :: content, stack)
          else (nil, ((elem, atts), content) :: stack)

      (* XXX should have message *)
      fun hookEndTag ((_, nil), _) = raise XML "ill-formed!"
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

  (* Parse, no DTD. *)
  (* fun parsefile file = Parser.parseDocument "?" NONE Hooks.appstart *)

end