(* Standard Mersenne Twister pseudorandom number generator. 
   (MT19937 with the improved initialization scheme.)
   Based on the BSD-licensed code by the algorithm's authors; this
   implementation also BSD-licensed (see mersenne-twister.sml for
   details).

   Important note: This is a good source of randomness but not
   suitable for cryptography, because observing 624 consecutive
   ouputs allows an attacker to determine the internal state and
   predict future outputs. For cryptographic uses, the outputs
   should be passed through a secure one-way hash function.

   For a description of the algorithm, see M. Matsumoto and T.
   Nishimura, "Mersenne Twister: A 623-Dimensionally Equidistributed
   Uniform Pseudo-random Number Generator," ACM Transactions on
   Modeling and Computer Simulation, Vol. 8, No. 1, January 1998, pp
   3-30. *)
signature MERSENNETWISTER =
sig

    (* Mersenne twister state; imperative *)
    type mt
    val init32 : Word32.word -> mt
    val init : Word32.word Vector.vector -> mt

    val rand32 : mt -> Word32.word

end
