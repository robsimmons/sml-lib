(*
require "basis.__array";
require "basis.__byte";
require "basis.__string";
require "basis.__vector";
require "basis.__word";
require "basis.__word8";

require "util.unsafe";
require "util.utilInt"; 

require "chars"; 
require "naming"; 
*)
signature Uri =
   sig
      eqtype Uri      

      val emptyUri   : Uri

      val hashUri    : Uri -> word
      val compareUri : Uri * Uri -> order

      val uriJoin    : Uri * Uri -> Uri
      val uriSuffix  : Uri -> string

      val Data2Uri   : UniChar.Data -> Uri
      val Vector2Uri : UniChar.Vector -> Uri
      val String2Uri : string -> Uri
      val Uri2String : Uri -> string

      val retrieveUri : Uri -> string * string * bool
      (* Like above, but supports in-memory URIs with raw-data:<bytes> url scheme. 
         Can raise IO.Io. *)
      val retrieveUriStream : Uri -> { uri : string,
                                       filename : string,
                                       instream : TextIO.instream,
                                       tmp : bool }

   end

structure Uri :> Uri =
   struct
      open UniChar UniClasses UriDecode UriEncode UtilError UtilInt

      (*--------------------------------------------------------------------*)
      (* decoding                                                           *)
      (*--------------------------------------------------------------------*)
      type Uri = string

      val emptyUri = ""

      val Vector2Uri = Vector2UriUtf8
      val Data2Uri = Data2UriUtf8
      val String2Uri = String2UriUtf8
      val Uri2String = decodeUriUtf8 

      val slash = "/"

      fun uriSuffix s =
         let fun search i = if i<0 then NONE else case String.sub(s,i) 
                                                    of #"." => SOME i
                                                     | #"/" => NONE
                                                     | _ => search (i-1)
         in case search (String.size s-1)
              of NONE => ""
               | SOME i => String.extract(s,i+1,NONE)
         end

      fun isScheme c = 
         Char.isAlphaNum c orelse #"+"=c orelse #"-"=c orelse #"."=c

      fun uriAbsolute uri =
         let fun search i = 
            if i>=String.size uri then false
            else let val c=String.sub(uri,i)
                 in if #":"=c then true else if isScheme c then search (i+1)
                                             else false
                 end
         in 
            if uri="" then false
            else if Char.isAlpha (String.sub(uri,0)) then search 1
                 else false
         end
      fun uriRelative uri = not (uriAbsolute uri)
                 
      fun uriLocal uri =
         if String.isPrefix "file:" uri 
            then SOME(String.extract(uri,5,NONE)) 
         else if uriRelative uri then SOME uri 
              else NONE

      fun uriPath s =
         let 
            fun search (i,hadSlash) = 
               if i<0 then if hadSlash then SOME 0 else NONE
               else case String.sub(s,i) 
                      of #"/" => if hadSlash then NONE else search(i-1,true)
                       | _ => if hadSlash then SOME(i+1) else search(i-1,false)
            val len = String.size s
            val posOpt = search(len-1,false)
         in case posOpt
              of NONE => emptyUri
               | SOME i => if i=0 then slash
                           else String.extract(s,0,SOME(i+1))
         end

      fun uriAuth uri =
         let 
            fun searchScheme i = 
               if i>=String.size uri then NONE
               else let val c=String.sub(uri,i)
                    in if #":"=c then SOME i else if isScheme c then searchScheme (i+1)
                                                  else NONE
                 end
            fun searchSlash i = 
               if i>=String.size uri then NONE
               else let val c=String.sub(uri,i)
                    in if #"/"=c then SOME i else searchSlash (i+1)
                    end
         in 
            if uri="" then ""
            else if not (Char.isAlpha(String.sub(uri,0))) then ""
                 else case searchScheme 1
                        of NONE => ""
                         | SOME i => 
                           if String.size uri<=i+2 then String.extract(uri,0,SOME(i+1))
                           else if #"/"=String.sub(uri,i+1) andalso #"/"=String.sub(uri,i+2)
                                   then case searchSlash (i+3)
                                          of NONE => uri
                                           | SOME j => String.extract(uri,0,SOME j)
                                else String.extract(uri,0,SOME(i+1))
         end

      fun uriScheme uri =
         let 
            fun searchScheme i = 
               if i>=String.size uri then NONE
               else let val c=String.sub(uri,i)
                    in if #":"=c then SOME i else if isScheme c then searchScheme (i+1)
                                                  else NONE
                 end
         in 
            if uri="" then ""
            else if not (Char.isAlpha(String.sub(uri,0))) then ""
                 else case searchScheme 1
                        of NONE => ""
                         | SOME i => String.extract(uri,0,SOME(i+1))
         end
      
      fun uriJoin(abs,rel) = 
         if rel="" then uriPath abs
         else if abs="" then rel
         else if String.isPrefix "//" rel then uriScheme abs^rel
         else if #"/"=String.sub(rel,0) then uriAuth abs^rel
         else if uriAbsolute rel then rel
              else uriPath abs^rel

      val compareUri = String.compare
      val hashUri = UtilHash.hashString

      fun convertCommand str (src,dst) =
         let 
            val s = Substring.full str
            fun doit ss s = 
               if Substring.isEmpty s then ss
               else let val (sl,sr) = Substring.splitr (fn c => #"%"<>c) s
                    in if Substring.isEmpty sl then sr::ss
                       else let val sl' =  Substring.trimr 1 sl
                            in case Substring.first sr
                                 of SOME #"1" => let val sr' = Substring.triml 1 sr
                                                 in doit (Substring.full src::sr'::ss) sl'
                                                 end
                                  | SOME #"2" => let val sr' = Substring.triml 1 sr
                                                 in doit (Substring.full dst::sr'::ss) sl'
                                                 end
                                  | _ => doit (Substring.full "%"::sr::ss) sl'
                            end
                    end
            val ss = doit nil s
            val s = Substring.concat ss
         in s
         end

      fun retrieveRemote uri = raise NoSuchFile (uri, "remote access disabled")
(*
         let 
            val tmp = OS.FileSys.tmpName()
            val cmd = convertCommand Config.retrieveCommand (uri,tmp)
            val status = OS.Process.system cmd
            val _ = if status = OS.Process.success then ()
                    else let val _ = (OS.FileSys.remove tmp 
                                      handle OS.SysErr _ => ())
                             val cmd = convertCommand 
                                Config.retrieveCommand ("<uri>",tmp)
                         in raise NoSuchFile (uri,"command '"^cmd^"' failed")
                         end
         in (Uri2String uri,tmp,true)
         end
*)

      fun retrieveUri uri = 
         case uriLocal uri
           of SOME f => (Uri2String uri,Uri2String f,false) 
            | NONE => retrieveRemote uri 

      fun uriData (uri : string) =
         if String.isPrefix "raw-data:" uri 
            then SOME(String.extract(uri,9,NONE)) 
         else NONE

      fun retrieveUriStream uri =
          case uriData uri of
              NONE =>
                let val (str, fname, tmp) = retrieveUri uri
                in { uri = str, filename = fname, tmp = tmp,
                     instream = TextIO.openIn fname }
                end
            | SOME s =>
                { uri = Uri2String uri, 
                  filename = "<in-memory>",
                  tmp = false,
                  instream = 
                  (* Create a TextIO.instream from a string in
                     memory. Not sure why nullRd works (from
                     the Basis documentation it doesn't seem
                     like it should have any functions defined),
                     but it does. - tom 7   23 May 2010 *)
                  TextIO.mkInstream 
                  (TextIO.StreamIO.mkInstream
                   (TextPrimIO.nullRd (),
                    s)) }
   end
