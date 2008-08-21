structure S = struct
val _ = print (SHA1.bintohex (SHA1.hash "hi\n"));
end
