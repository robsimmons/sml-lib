
(* Simple parser and pretty-printer for XML. *)
structure XML =
struct

  exception XML of string
  (* ?? *)
  type pretag = string * HookData.AttSpecList

  datatype pretree = 
      PreText of string
    | PreElem of pretag * pretree list

  val d = Dtd.initDtdTables()

  structure Hooks =
  struct
      open IgnoreHooks
          
      type AppData = pretree list * (pretag * pretree list) list
      type AppFinal = pretree
          
      val appstart = (nil, nil)
          
      fun hookStartTag ((content, stack),
                        (dtd, id, atts, _, empty)) =
          let val t = UniChar.Data2String (Dtd.Index2Element d id)
              
          in
              if empty 
              then (PreElem ((t, atts), nil) :: content, stack)
              else (nil, ((t, atts), content) :: stack)
          end

      (* XXX should have message *)
      fun hookEndTag ((_, nil), _) = raise XML "ill-formed: no tag open"
        | hookEndTag ((content, (tag, content') :: stack), _) =
          (PreElem (tag, rev content) :: content', stack)

      fun hookData ((content, stack), (_, vec, _)) =
          (PreText (UniChar.Vector2String vec) :: content, stack)

      fun hookCData ((content, stack), (_, vec)) =
          (PreText (UniChar.Vector2String vec) :: content, stack)

      fun hookCharRef ((content, stack), (_, c, _)) =
          (PreText (UniChar.Data2String [c]) :: content, stack)

      fun hookFinish ([tree], nil) = tree
        | hookFinish _ = raise XML "ill-formed: multiple trees?"
  end

  structure Parser = Parse(structure Dtd = Dtd
                           structure Hooks = Hooks
                           structure ParserOptions = ParserOptions ()
                           structure Resolve = ResolveNull)

  type tag = string * HookData.AttSpecList

  datatype tree = 
      Text of string
    | Elem of tag * tree list

  fun maketree (PreText s) = Text s
    | maketree (PreElem ((id, a), tl)) = Elem((id, a), map maketree tl)
(*      Elem((UniChar.Data2String (Dtd.Index2Element d id),
                                               a), map maketree tl)*)

  fun parsefile file = 
      maketree 
      (Parser.parseDocument (SOME (Uri.String2Uri ((* "file://" ^ *) file))) 
          (SOME d) Hooks.appstart)

end