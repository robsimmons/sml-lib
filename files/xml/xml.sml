
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

  fun vectortoutf8 v =
      let
          val BUFFER_SIZE = 1024
              
          fun write ((s, arr, sz), w) =
              let in
                  CharArray.update(arr, sz, chr (Word8.toInt w));
                  (s, arr, sz + 1)
              end handle Subscript =>
                  write ((CharArraySlice.vector(CharArraySlice.slice(arr, 0, NONE)) :: s, arr, 0), w)

          fun finalize (s, arr, sz) = 
              String.concat(rev (CharArraySlice.vector (CharArraySlice.slice(arr, 0, SOME sz)) :: s))

          val start = (nil, CharArray.array(BUFFER_SIZE, chr 0), 0)

          val finish = Vector.foldl (EncodeUTF8.writeCharUtf8 write) start v
      in
          finalize finish
      end

  (* PERF *)
  fun datatoutf8 l = vectortoutf8 (Vector.fromList l)

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
              then (Elem ((t, atts), nil) :: content, stack)
              else (nil, ((t, atts), content) :: stack)
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