(* This is based on PAM's md5_crypt. The code is in the public domain,
   and was written by Tom 7 in June 2001.

   FYI, this algorithm is completely screwed up. I wouldn't be
   surprised if all of this nonsense actually reduces the security as
   compared to just doing a single pass of MD5. What were they
   thinking? *)

structure MD5Crypt :> MD5CRYPT =
struct

  exception Error of string
  
  fun itoa64 i = 
    str 
    (CharVector.sub
     ("./0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz", i))

  fun to64 0 i = ""
    | to64 n i = itoa64 (i mod 64) ^ to64 (n-1) (i div 64)

  fun repc (s, m, n) =
    if n <= 0 then ""
    else if n >= m then s ^ repc (s, m, n - m)
         else String.substring(s, 0, n)

  (* "Then something really weird..."
     I think the guy who wrote this meant to increment j;
     as it is, it only uses the first character of final
     (which was just zero-filled???) and pw, adding 0 each time.
     Anyway, we have to implement what he has, not what he
     meant. =)
   *)

  fun really_weird ( _, 0) = ""
    | really_weird (pw, n) =
    (if n mod 2 = 1 then "\000"
     else pw) ^ really_weird (pw, n div 2)

  fun getfields pwf =
    case String.fields (fn #"$" => true | _ => false) pwf of
      ["", "1", salt, hash] => (salt, hash)
    | _ => raise Error "Not in MD5Crypt ($1$) format."

  fun crypt salt realpass =
        let
          val s1 = realpass ^ "$1$" ^ salt
          val final = MD5.md5 (realpass ^ salt ^ realpass)
            
          val s1 = s1 ^ (repc (final, size final, size realpass))
          val s1 = s1 ^ really_weird (String.substring(realpass, 0, 1), 
                                      size realpass)
          val final = MD5.md5 s1

          fun loop 1000 final = final
            | loop i    final =
            let
            in
              loop (i + 1)
              (MD5.md5 ((if i mod 2 = 1 then realpass
                        else final) ^
                        (if i mod 3 <> 0 then salt
                         else "") ^
                        (if i mod 7 <> 0 then realpass
                         else "") ^
                        (if i mod 2 = 1 then final
                         else realpass)))
            end
          val final = loop 0 final

          fun ci n = ord (CharVector.sub (final, n))
          fun ff (a, b, c) =
            to64 4 ((ci a) * 256 * 256 +
                    (ci b) * 256 +
                    (ci c))
        in
          foldr op^ (to64 2 (ci 11))
                      (map ff [(0,  6, 12),
                               (1,  7, 13),
                               (2,  8, 14),
                               (3,  9, 15),
                               (4, 10,  5)])
        end
        
end
