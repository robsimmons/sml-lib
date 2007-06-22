
structure ID3v11 :> ID3V11 =
struct

    type id3v11 =
        { (* truncated to 30 chars *)
          title : string,
          (* truncated to 30 chars *)
          artist : string,
          (* truncated to 30 chars *)
          album : string,
          (* truncated to 4 chars -- Y10Kbug *)
          year : string,
          (* truncated to 28 chars *)
          comment : string,
          (* 0-255 *)
          track : int,
          (* 0-80 *)
          genre : int }

    structure FS = Posix.FileSys
    structure IO = Posix.IO

    fun vec fd n =
        let val v = IO.readVec(fd, n)
        in
            CharVector.tabulate
              (n, (fn i => chr (Word8.toInt (Word8Vector.sub(v, i)))))
        end

    fun stripzeros s =
        let
            val n = size s
            fun go m =
                if m >= n then s
                else if #"\000" = CharVector.sub(s, m)
                     then String.substring(s, 0, m)
                     else go (m + 1)
        in
            go 0
        end

    fun gets s st n =
        stripzeros (String.substring (s, st, n))

    fun readid3 f =
        let
            val fd = FS.openf (f, FS.O_RDONLY, FS.O.flags nil)
        in
            let in
                (* tag begins 128 bytes from eof *)
                IO.lseek(fd, 0, IO.SEEK_END);
                IO.lseek(fd, ~128, IO.SEEK_CUR);

                case vec fd 3 of
                    "TAG" =>
                        let
                            val rest = vec fd 125
                        in
                            IO.close fd;
                            SOME
                            { title = gets rest 0 30,
                              artist = gets rest 30 30,
                              album = gets rest 60 30,
                              year = gets rest 90 4,
                              comment = gets rest 94 28,
                              (* byte 122 = 0 *)
                              track = ord (CharVector.sub(rest, 123)),
                              genre = ord (CharVector.sub(rest, 124)) }
                        end
                  | _ => NONE

            end handle _ =>
                let in
                    IO.close fd;
                    NONE
                end
        end handle _ => NONE

    structure C = CharArray

    (* for old basis, add: len=NONE, si=0, *)
    fun writeat a st s =
        C.copyVec{di=st, dst=a, src=s}

    fun truncs s n =
        if size s > n 
        then String.substring(s, 0, n)
        else s

    fun getw8v s =
        Word8Vector.tabulate(CharArray.length s,
                             fn i => Word8.fromInt (ord (CharArray.sub(s, i))))

    fun writecreateid3 f {title, artist, album, year, comment, track, genre} =
        let
            val a = C.array(128, chr 0)
        in
            writeat a 0 "TAG";
            writeat a 3 (truncs title 30);
            writeat a 33 (truncs artist 30);
            writeat a 63 (truncs album 30);
            writeat a 93 (truncs year 4);
            writeat a 97 (truncs comment 28);
            (* 122 already 0 *)
            writeat a 126 (implode [chr (track mod 256)]);
            writeat a 127 (implode [chr (genre mod 256)]);

            (* if it exists, truncate file *)
            (case readid3 f of
                 SOME _ => 
                     let
                         val fd = FS.openf (f, 
                                            FS.O_WRONLY,
                                            FS.O.flags nil)
                         val old =
                             FS.ST.size (FS.stat f)
                     in
                         FS.ftruncate(fd,
                                      Position.-(old, Position.fromInt 128));
                         IO.close fd
                     end
               | NONE => ());
                 
            let
                val fd = FS.openf (f, FS.O_WRONLY, FS.O.flags [FS.O.append])
            in
                IO.writeVec (fd, Word8VectorSlice.full (getw8v a));
                IO.close fd
            end
        end

end
