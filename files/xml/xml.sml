
(* Simple parser and pretty-printer for XML. *)
structure XML =
struct

  exception XML of string
  (* ?? *)

  (* XXX get rid of HookData *)
  type attribute = string * string option
  type tag = string * attribute list

  datatype tree = 
      Text of string
    | Elem of tag * tree list

  val d = Dtd.initDtdTables()

  fun vectortoutf8 v =
      let
          val BUFFER_SIZE = 1024
              
          fun write ((s, arr, sz), w) =
              let in
                  CharArray.update(arr, sz, chr (Word8.toInt w));
                  (s, arr, sz + 1)
              end handle Subscript =>
                  write ((CharArraySlice.vector
                            (CharArraySlice.slice(arr, 0, NONE)) :: s, 
                          arr, 0), w)

          fun finalize (s, arr, sz) = 
              String.concat(rev (CharArraySlice.vector
                                 (CharArraySlice.slice(arr, 0, SOME sz)) :: s))

          val start = (nil, CharArray.array(BUFFER_SIZE, chr 0), 0)

          val finish = Vector.foldl (EncodeUTF8.writeCharUtf8 write) start v
      in
          finalize finish
      end

  (* PERF *)
  fun datatoutf8 l = vectortoutf8 (Vector.fromList l)

  (* XXX Don't know what most of these are... *)
  fun getvalue (a : Base.AttValue) =
      case a of
      Base.AV_CDATA (u : UniChar.Vector) => raise XML "Unimplemented AV_CDATA"
    | Base.AV_NMTOKEN (u : UniChar.Data) => raise XML "Unimplemented AV_NMTOKEN"
    | Base.AV_NMTOKENS (ul : UniChar.Data list) => raise XML "Unimplemented AV_NMTOKENS"
    | Base.AV_ID (i : int) => raise XML "Unimplemented AV_ID"
    | Base.AV_IDREF (i : int) => raise XML "Unimplemented AV_IDREF" 
    | Base.AV_IDREFS (il : int list) => raise XML "Unimplemented AV_IDREFS"
    | Base.AV_ENTITY (i : int) => raise XML "Unimplemented AV_ENTITY"
    | Base.AV_ENTITIES (il : int list) => raise XML "Unimplemented AV_ENTITIES"
    | Base.AV_GROUP (il : int list, i : int) => raise XML "Unimplemented AV_GROUP" 
    | Base.AV_NOTATION (il : int list, i : int) => raise XML "Unimplemented AV_NOTATION"


  fun spectoattr (i : int, ap : HookData.AttPresent,
                  (* This is the text before the attribute name
                     and between the attribute and value (I think),
                     which is useless except for unparsing *)
                  uo : (UniChar.Data * UniChar.Data) option) =
      (datatoutf8 (Dtd.Index2AttNot d i), 
       case ap of
           HookData.AP_IMPLIED => SOME "IMPLIED"
         | HookData.AP_MISSING => SOME "MISSING"
         | HookData.AP_DEFAULT (_, v2, ao) => SOME (vectortoutf8 v2)
         | HookData.AP_PRESENT (_, v2, ao) => SOME (vectortoutf8 v2)
(*
               SOME ("PRESENT: " ^ vectortoutf8 v1 ^ "/" ^
                     vectortoutf8 v2 ^
                     (case ao of
                          NONE => "NONE"
                        | SOME _ => "S"))
*)
(*
       case ap of
           HookData.AP_IMPLIED => NONE (* ? *)
         | HookData.AP_MISSING => NONE
         | HookData.AP_DEFAULT (_, _, ao) => Option.map getvalue ao
         | HookData.AP_PRESENT (_, _, ao) => Option.map getvalue ao
*)
)

  structure Hooks =
  struct
      open IgnoreHooks
          
      type AppData = tree list * (tag * tree list) list
      type AppFinal = tree
          
      val appstart = (nil, nil)
          
      fun hookStartTag ((content, stack),
                        (dtd, id, atts, _, empty)) =
          let val t = datatoutf8 (Dtd.Index2Element d id)
          in
              if empty 
              then (Elem ((t, map spectoattr atts), nil) :: content, stack)
              else (nil, ((t, map spectoattr atts), content) :: stack)
          end

      fun hookEndTag ((_, nil), _) = raise XML "ill-formed: no tag open"
        | hookEndTag ((content, (tag, content') :: stack), _) =
          (Elem (tag, rev content) :: content', stack)

      fun hookData ((content, stack), (_, vec, _)) =
          (Text (vectortoutf8 vec) :: content, stack)

      fun hookCData ((content, stack), (_, vec)) =
          (Text (vectortoutf8 vec) :: content, stack)

      fun hookCharRef ((content, stack), (_, c, _)) =
          (Text (datatoutf8 [c]) :: content, stack)

      fun hookFinish ([tree], nil) = tree
        | hookFinish (nil, _) = raise XML "ill-formed: parsed zero trees"
        | hookFinish _ = raise XML "ill-formed: multiple trees?"
  end

  structure Parser = Parse(structure Dtd = Dtd
                           structure Hooks = Hooks
                           structure ParserOptions = ParserOptions ()
                           structure Resolve = ResolveNull)

  fun parsefile file = 
      Parser.parseDocument (SOME (Uri.String2Uri file)) (SOME d) Hooks.appstart

end