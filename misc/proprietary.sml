(* This is a cryptographically-unsound proprietary hash algorithm.
   It's used to create verifiable signatures by "blessed" official
   binary releases of applications, so that client-generated data
   can have some moderate level of trustworthiness (e.g. high scores).

   This problem doesn't have a satisfying solution, especially for
   an open-source program. If we just use a known cryptographically-
   strong signature algorithm (say, El Gamal) then an adversary can
   inspect the code to learn how it works, and create his own signing
   function. At worst the key is visible in the code and he uses his
   crypto library's implementation of El Gamal to sign whatever
   message he wants. We can avoid having the key visible in the code,
   only providing it at compilation time, but then the key is probably
   neatly visible as a symbol in the compiled binary, and easily
   extracted.

   At some level the signing function must be manifest in the compiled
   code, which means that the adversary will be able to read that code,
   and thus conceivably reproduce the function. The best strategy is
   therefore to make that task as difficult as possible, by making the 
   compiled code incomprehensible.

   This meta-program generates a hash function based on an input
   string; the function is SML (and Aphasia 2) source code. The
   implementation of the function is pointlessly roundabout, dependent
   on the input string, and prone to optimization by the compiler.
   It should be difficult to reverse engineer by looking at the
   compiled code. It is not a design goal that it be difficult to
   discover the secret key from the uncompiled source, but that may
   well be true, too.

   If you're reading this it might be because you're interested
   in tampering, like to fake a high score. Be my guest, I guess, since
   if people like you don't exist then I'm wasting my time right now.
   If you succeed and I notice, then I will just undo your fake data,
   make a few minor changes to this code, and force a new release. I
   think this is less work for me than you.

   The remainder of this code is deliberately unclear and light on
   documentation.
*)
