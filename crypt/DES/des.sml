
(* The "Data Encryption Standard." *)

structure DES :> DES =
struct

  exception DES

  infix >> << orb andb xorb
  
  val op >> = Word32.>>
  val op << = Word32.<<
  val op andb = Word32.andb
  val op orb = Word32.orb
  val op xorb = Word32.xorb

  (* 32 words *)
  type key = Word32.word Array.array

  (* S boxes, permuted with P and rotated one bit to the left *)  
  val sbox1 =
    Vector.fromList
    [0wx01010400, 0wx00000000, 0wx00010000, 0wx01010404, 
     0wx01010004, 0wx00010404, 0wx00000004, 0wx00010000,
     0wx00000400, 0wx01010400, 0wx01010404, 0wx00000400, 
     0wx01000404, 0wx01010004, 0wx01000000, 0wx00000004,
     0wx00000404, 0wx01000400, 0wx01000400, 0wx00010400, 
     0wx00010400, 0wx01010000, 0wx01010000, 0wx01000404,
     0wx00010004, 0wx01000004, 0wx01000004, 0wx00010004, 
     0wx00000000, 0wx00000404, 0wx00010404, 0wx01000000,
     0wx00010000, 0wx01010404, 0wx00000004, 0wx01010000, 
     0wx01010400, 0wx01000000, 0wx01000000, 0wx00000400,
     0wx01010004, 0wx00010000, 0wx00010400, 0wx01000004, 
     0wx00000400, 0wx00000004, 0wx01000404, 0wx00010404,
     0wx01010404, 0wx00010004, 0wx01010000, 0wx01000404, 
     0wx01000004, 0wx00000404, 0wx00010404, 0wx01010400,
     0wx00000404, 0wx01000400, 0wx01000400, 0wx00000000, 
     0wx00010004, 0wx00010400, 0wx00000000, 0wx01010004 : Word32.word]

  val sbox2 =
    Vector.fromList
    [0wx80108020, 0wx80008000, 0wx00008000, 0wx00108020, 
     0wx00100000, 0wx00000020, 0wx80100020, 0wx80008020,
     0wx80000020, 0wx80108020, 0wx80108000, 0wx80000000, 
     0wx80008000, 0wx00100000, 0wx00000020, 0wx80100020,
     0wx00108000, 0wx00100020, 0wx80008020, 0wx00000000, 
     0wx80000000, 0wx00008000, 0wx00108020, 0wx80100000,
     0wx00100020, 0wx80000020, 0wx00000000, 0wx00108000, 
     0wx00008020, 0wx80108000, 0wx80100000, 0wx00008020,
     0wx00000000, 0wx00108020, 0wx80100020, 0wx00100000, 
     0wx80008020, 0wx80100000, 0wx80108000, 0wx00008000,
     0wx80100000, 0wx80008000, 0wx00000020, 0wx80108020, 
     0wx00108020, 0wx00000020, 0wx00008000, 0wx80000000,
     0wx00008020, 0wx80108000, 0wx00100000, 0wx80000020, 
     0wx00100020, 0wx80008020, 0wx80000020, 0wx00100020,
     0wx00108000, 0wx00000000, 0wx80008000, 0wx00008020, 
     0wx80000000, 0wx80100020, 0wx80108020, 0wx00108000 : Word32.word]

  val sbox3 =
    Vector.fromList
    [0wx00000208, 0wx08020200, 0wx00000000, 0wx08020008, 
     0wx08000200, 0wx00000000, 0wx00020208, 0wx08000200,
     0wx00020008, 0wx08000008, 0wx08000008, 0wx00020000, 
     0wx08020208, 0wx00020008, 0wx08020000, 0wx00000208,
     0wx08000000, 0wx00000008, 0wx08020200, 0wx00000200, 
     0wx00020200, 0wx08020000, 0wx08020008, 0wx00020208,
     0wx08000208, 0wx00020200, 0wx00020000, 0wx08000208, 
     0wx00000008, 0wx08020208, 0wx00000200, 0wx08000000,
     0wx08020200, 0wx08000000, 0wx00020008, 0wx00000208, 
     0wx00020000, 0wx08020200, 0wx08000200, 0wx00000000,
     0wx00000200, 0wx00020008, 0wx08020208, 0wx08000200, 
     0wx08000008, 0wx00000200, 0wx00000000, 0wx08020008,
     0wx08000208, 0wx00020000, 0wx08000000, 0wx08020208, 
     0wx00000008, 0wx00020208, 0wx00020200, 0wx08000008,
     0wx08020000, 0wx08000208, 0wx00000208, 0wx08020000, 
     0wx00020208, 0wx00000008, 0wx08020008, 0wx00020200 : Word32.word]
    
  val sbox4 =
    Vector.fromList
    [0wx00802001, 0wx00002081, 0wx00002081, 0wx00000080, 
     0wx00802080, 0wx00800081, 0wx00800001, 0wx00002001,
     0wx00000000, 0wx00802000, 0wx00802000, 0wx00802081, 
     0wx00000081, 0wx00000000, 0wx00800080, 0wx00800001,
     0wx00000001, 0wx00002000, 0wx00800000, 0wx00802001, 
     0wx00000080, 0wx00800000, 0wx00002001, 0wx00002080,
     0wx00800081, 0wx00000001, 0wx00002080, 0wx00800080, 
     0wx00002000, 0wx00802080, 0wx00802081, 0wx00000081,
     0wx00800080, 0wx00800001, 0wx00802000, 0wx00802081, 
     0wx00000081, 0wx00000000, 0wx00000000, 0wx00802000,
     0wx00002080, 0wx00800080, 0wx00800081, 0wx00000001, 
     0wx00802001, 0wx00002081, 0wx00002081, 0wx00000080,
     0wx00802081, 0wx00000081, 0wx00000001, 0wx00002000, 
     0wx00800001, 0wx00002001, 0wx00802080, 0wx00800081,
     0wx00002001, 0wx00002080, 0wx00800000, 0wx00802001, 
     0wx00000080, 0wx00800000, 0wx00002000, 0wx00802080 : Word32.word]
    
  val sbox5 =
    Vector.fromList
    [0wx00000100, 0wx02080100, 0wx02080000, 0wx42000100, 
     0wx00080000, 0wx00000100, 0wx40000000, 0wx02080000,
     0wx40080100, 0wx00080000, 0wx02000100, 0wx40080100, 
     0wx42000100, 0wx42080000, 0wx00080100, 0wx40000000,
     0wx02000000, 0wx40080000, 0wx40080000, 0wx00000000, 
     0wx40000100, 0wx42080100, 0wx42080100, 0wx02000100,
     0wx42080000, 0wx40000100, 0wx00000000, 0wx42000000, 
     0wx02080100, 0wx02000000, 0wx42000000, 0wx00080100,
     0wx00080000, 0wx42000100, 0wx00000100, 0wx02000000, 
     0wx40000000, 0wx02080000, 0wx42000100, 0wx40080100,
     0wx02000100, 0wx40000000, 0wx42080000, 0wx02080100, 
     0wx40080100, 0wx00000100, 0wx02000000, 0wx42080000,
     0wx42080100, 0wx00080100, 0wx42000000, 0wx42080100, 
     0wx02080000, 0wx00000000, 0wx40080000, 0wx42000000,
     0wx00080100, 0wx02000100, 0wx40000100, 0wx00080000, 
     0wx00000000, 0wx40080000, 0wx02080100, 0wx40000100 : Word32.word]
    
  val sbox6 = 
    Vector.fromList
    [0wx20000010, 0wx20400000, 0wx00004000, 0wx20404010, 
     0wx20400000, 0wx00000010, 0wx20404010, 0wx00400000,
     0wx20004000, 0wx00404010, 0wx00400000, 0wx20000010, 
     0wx00400010, 0wx20004000, 0wx20000000, 0wx00004010,
     0wx00000000, 0wx00400010, 0wx20004010, 0wx00004000, 
     0wx00404000, 0wx20004010, 0wx00000010, 0wx20400010,
     0wx20400010, 0wx00000000, 0wx00404010, 0wx20404000, 
     0wx00004010, 0wx00404000, 0wx20404000, 0wx20000000,
     0wx20004000, 0wx00000010, 0wx20400010, 0wx00404000, 
     0wx20404010, 0wx00400000, 0wx00004010, 0wx20000010,
     0wx00400000, 0wx20004000, 0wx20000000, 0wx00004010, 
     0wx20000010, 0wx20404010, 0wx00404000, 0wx20400000,
     0wx00404010, 0wx20404000, 0wx00000000, 0wx20400010, 
     0wx00000010, 0wx00004000, 0wx20400000, 0wx00404010,
     0wx00004000, 0wx00400010, 0wx20004010, 0wx00000000, 
     0wx20404000, 0wx20000000, 0wx00400010, 0wx20004010 : Word32.word]
    
  val sbox7 =
    Vector.fromList
    [0wx00200000, 0wx04200002, 0wx04000802, 0wx00000000, 
     0wx00000800, 0wx04000802, 0wx00200802, 0wx04200800,
     0wx04200802, 0wx00200000, 0wx00000000, 0wx04000002, 
     0wx00000002, 0wx04000000, 0wx04200002, 0wx00000802,
     0wx04000800, 0wx00200802, 0wx00200002, 0wx04000800, 
     0wx04000002, 0wx04200000, 0wx04200800, 0wx00200002,
     0wx04200000, 0wx00000800, 0wx00000802, 0wx04200802, 
     0wx00200800, 0wx00000002, 0wx04000000, 0wx00200800,
     0wx04000000, 0wx00200800, 0wx00200000, 0wx04000802, 
     0wx04000802, 0wx04200002, 0wx04200002, 0wx00000002,
     0wx00200002, 0wx04000000, 0wx04000800, 0wx00200000, 
     0wx04200800, 0wx00000802, 0wx00200802, 0wx04200800,
     0wx00000802, 0wx04000002, 0wx04200802, 0wx04200000, 
     0wx00200800, 0wx00000000, 0wx00000002, 0wx04200802,
     0wx00000000, 0wx00200802, 0wx04200000, 0wx00000800, 
     0wx04000002, 0wx04000800, 0wx00000800, 0wx00200002 : Word32.word]
    
  val sbox8 =
    Vector.fromList
    [0wx10001040, 0wx00001000, 0wx00040000, 0wx10041040, 
     0wx10000000, 0wx10001040, 0wx00000040, 0wx10000000,
     0wx00040040, 0wx10040000, 0wx10041040, 0wx00041000, 
     0wx10041000, 0wx00041040, 0wx00001000, 0wx00000040,
     0wx10040000, 0wx10000040, 0wx10001000, 0wx00001040, 
     0wx00041000, 0wx00040040, 0wx10040040, 0wx10041000,
     0wx00001040, 0wx00000000, 0wx00000000, 0wx10040040, 
     0wx10000040, 0wx10001000, 0wx00041040, 0wx00040000,
     0wx00041040, 0wx00040000, 0wx10041000, 0wx00001000, 
     0wx00000040, 0wx10040040, 0wx00001000, 0wx00041040,
     0wx10001000, 0wx00000040, 0wx10000040, 0wx10040000, 
     0wx10040040, 0wx10000000, 0wx00040000, 0wx10001040,
     0wx00000000, 0wx10041040, 0wx00040040, 0wx10000040, 
     0wx10040000, 0wx10001000, 0wx10001040, 0wx00000000,
     0wx10041040, 0wx00041000, 0wx00041000, 0wx00001040, 
     0wx00001040, 0wx00040040, 0wx10000000, 0wx10041000 : Word32.word]

  val leftkey_swap =
    Vector.fromList
    [0wx00000000, 0wx00000001, 0wx00000100, 0wx00000101,
     0wx00010000, 0wx00010001, 0wx00010100, 0wx00010101,
     0wx01000000, 0wx01000001, 0wx01000100, 0wx01000101,
     0wx01010000, 0wx01010001, 0wx01010100, 0wx01010101 : Word32.word]
    
  val rightkey_swap =
    Vector.fromList
    [0wx00000000, 0wx01000000, 0wx00010000, 0wx01010000,
     0wx00000100, 0wx01000100, 0wx00010100, 0wx01010100,
     0wx00000001, 0wx01000001, 0wx00010001, 0wx01010001,
     0wx00000101, 0wx01000101, 0wx00010101, 0wx01010101 : Word32.word]
    
  val encrypt_rotate_tab =
    Vector.fromList
    [0w1, 0w1, 0w2, 0w2, 0w2, 0w2, 0w2, 0w2, 0w1, 0w2, 0w2, 0w2, 0w2, 0w2, 0w2, 0w1]

  (* functions to access these tables *)
  val sbox1 = fn x => Vector.sub(sbox1, Word32.toInt x)
  val sbox2 = fn x => Vector.sub(sbox2, Word32.toInt x)
  val sbox3 = fn x => Vector.sub(sbox3, Word32.toInt x)
  val sbox4 = fn x => Vector.sub(sbox4, Word32.toInt x)
  val sbox5 = fn x => Vector.sub(sbox5, Word32.toInt x)
  val sbox6 = fn x => Vector.sub(sbox6, Word32.toInt x)
  val sbox7 = fn x => Vector.sub(sbox7, Word32.toInt x)
  val sbox8 = fn x => Vector.sub(sbox8, Word32.toInt x)

  val leftkey_swap  = fn x => Vector.sub(leftkey_swap,  Word32.toInt x)
  val rightkey_swap = fn x => Vector.sub(rightkey_swap, Word32.toInt x)
  val encrypt_rotate_tab = fn x => Vector.sub(encrypt_rotate_tab, x)

  (* XXX don't bother checking for weak keys now; they are extremely rare *)
      
  fun do_permutation (a, b, offset, mask) =
    let
      val temp = ((a >> offset) xorb b) andb mask
      val b = b xorb temp
      val a = a xorb (temp << offset);
    in
      (a, b)
    end
  
  fun initial_permutation(left, right) =
    let
      val (left, right) = do_permutation(left, right, 0w4, 0wx0f0f0f0f)
      val (left, right) = do_permutation(left, right, 0w16, 0wx0000ffff)
      val (right, left) = do_permutation(right, left, 0w2, 0wx33333333)
      val (right, left) = do_permutation(right, left, 0w8, 0wx00ff00ff)
      val right = (right << 0w1) orb (right >> 0w31)
      val temp  = (left xorb right) andb 0wxaaaaaaaa
      val right = temp xorb right
      val left  = temp xorb left
      val left  = (left << 0w1) orb (left >> 0w31)
    in
      (left, right)
    end
  
  (* inverse of initial *)
  fun final_permutation(left, right) =
    let
      val left  = (left << 0w31) orb (left >> 0w1)
      val temp  = (left xorb right) andb 0wxaaaaaaaa
      val left  = temp xorb left
      val right = temp xorb right
      val right = (right << 0w31) orb (right >> 0w1)
      val (right, left) = do_permutation(right, left, 0w8, 0wx00ff00ff)
      val (right, left) = do_permutation(right, left, 0w2, 0wx33333333)
      val (left, right) = do_permutation(left, right, 0w16, 0wx0000ffff)
      val (left, right) = do_permutation(left, right, 0w4, 0wx0f0f0f0f)
    in
      (left, right)
    end
  
  (* reference code calls this with subkey; a pointer which
     is incremented. Instead, pass in the key array and current
     index. (the index should increase by 2 each time) *)
  fun des_round(from, to, keys, k) =
    let
      val work = from xorb (keys k)
      val to = to xorb sbox8(  work          andb 0wx3f )
      val to = to xorb sbox6( (work >> 0w8)  andb 0wx3f )
      val to = to xorb sbox4( (work >> 0w16) andb 0wx3f )
      val to = to xorb sbox2( (work >> 0w24) andb 0wx3f )

      val work = ((from << 0w28) orb (from >> 0w4)) xorb (keys (k + 1))
      val to = to xorb sbox7(  work          andb 0wx3f )
      val to = to xorb sbox5( (work >> 0w8)  andb 0wx3f )
      val to = to xorb sbox3( (work >> 0w16) andb 0wx3f )
      val to = to xorb sbox1( (work >> 0w24) andb 0wx3f )
    in
      to
    end

  (* set up key vector for 16 encryption rounds *)

  fun des_key_schedule key =
    let
      (* two successive subkeys per round *)
      val subkeys = Array.array (32, 0wx0 : Word32.word)

      (* XXX *)
      val (left, right) = key (* READ_64BIT_DATA (key) *)

      val (right, left) = do_permutation(right, left, 0w4, 0wx0f0f0f0f);
      val (right, left) = do_permutation(right, left, 0w0, 0wx10101010);

      val left = 
        (leftkey_swap((left >> 0w0) andb 0wxf) << 0w3) orb 
        (leftkey_swap((left >> 0w8) andb 0wxf) << 0w2) orb 
        (leftkey_swap((left >> 0w16) andb 0wxf) << 0w1) orb
        (leftkey_swap((left >> 0w24) andb 0wxf)) orb 
        (leftkey_swap((left >> 0w5) andb 0wxf) << 0w7) orb 
        (leftkey_swap((left >> 0w13) andb 0wxf) << 0w6) orb
        (leftkey_swap((left >> 0w21) andb 0wxf) << 0w5) orb
        (leftkey_swap((left >> 0w29) andb 0wxf) << 0w4)

      val left = left andb 0wx0fffffff

      val right = 
        (rightkey_swap((right >> 0w1) andb 0wxf) << 0w3) orb
        (rightkey_swap((right >> 0w9) andb 0wxf) << 0w2) orb
        (rightkey_swap((right >> 0w17) andb 0wxf) << 0w1) orb
        (rightkey_swap((right >> 0w25) andb 0wxf)) orb 
        (rightkey_swap((right >> 0w4) andb 0wxf) << 0w7) orb
        (rightkey_swap((right >> 0w12) andb 0wxf) << 0w6) orb
        (rightkey_swap((right >> 0w20) andb 0wxf) << 0w5) orb 
        (rightkey_swap((right >> 0w28) andb 0wxf) << 0w4)

      val right = right andb 0wx0fffffff

      fun loop 16 _ = ()
        | loop round (left, right) =
        let
          val left  = ((left << encrypt_rotate_tab(round)) orb 
                       (left >> (0w28 - encrypt_rotate_tab(round)))) 
                      andb 0wx0fffffff

          val right = ((right << encrypt_rotate_tab(round)) orb 
                       (right >> (0w28 - encrypt_rotate_tab(round)))) 
                      andb 0wx0fffffff
        in
          Array.update(subkeys, 2 * round,
                       (    (left << 0w4)  andb 0wx24000000)
                       orb ((left << 0w28) andb 0wx10000000)
                       orb ((left << 0w14) andb 0wx08000000)
                       orb ((left << 0w18) andb 0wx02080000)
                       orb ((left << 0w6) andb 0wx01000000)
                       orb ((left << 0w9) andb 0wx00200000)
                       orb ((left >> 0w1) andb 0wx00100000)
                       orb ((left << 0w10) andb 0wx00040000)
                       orb ((left << 0w2) andb 0wx00020000)
                       orb ((left >> 0w10) andb 0wx00010000)
                       orb ((right >> 0w13) andb 0wx00002000)
                       orb ((right >> 0w4) andb 0wx00001000)
                       orb ((right << 0w6) andb 0wx00000800)
                       orb ((right >> 0w1) andb 0wx00000400)
                       orb ((right >> 0w14) andb 0wx00000200)
                       orb (right        andb 0wx00000100)
                       orb ((right >> 0w5) andb 0wx00000020)
                       orb ((right >> 0w10) andb 0wx00000010)
                       orb ((right >> 0w3) andb 0wx00000008)
                       orb ((right >> 0w18) andb 0wx00000004)
                       orb ((right >> 0w26) andb 0wx00000002)
                       orb ((right >> 0w24) andb 0wx00000001));

          Array.update(subkeys, 2 * round + 1,
                       (    (left << 0w15) andb 0wx20000000)
                       orb ((left << 0w17) andb 0wx10000000)
                       orb ((left << 0w10) andb 0wx08000000)
                       orb ((left << 0w22) andb 0wx04000000)
                       orb ((left >> 0w2) andb 0wx02000000)
                       orb ((left << 0w1) andb 0wx01000000)
                       orb ((left << 0w16) andb 0wx00200000)
                       orb ((left << 0w11) andb 0wx00100000)
                       orb ((left << 0w3) andb 0wx00080000)
                       orb ((left >> 0w6) andb 0wx00040000)
                       orb ((left << 0w15) andb 0wx00020000)
                       orb ((left >> 0w4) andb 0wx00010000)
                       orb ((right >> 0w2) andb 0wx00002000)
                       orb ((right << 0w8) andb 0wx00001000)
                       orb ((right >> 0w14) andb 0wx00000808)
                       orb ((right >> 0w9) andb 0wx00000400)
                       orb ( right         andb 0wx00000200)
                       orb ((right << 0w7) andb 0wx00000100)
                       orb ((right >> 0w7) andb 0wx00000020)
                       orb ((right >> 0w3) andb 0wx00000011)
                       orb ((right << 0w2) andb 0wx00000004)
                       orb ((right >> 0w21) andb 0wx00000002));
          
          loop (round + 1) (left, right)
        end
    in
      loop 0 (left, right);
      subkeys
    end

  fun key k = 
    let 
      val keys = des_key_schedule k
    in
      (*
      Array.appi (fn (x, k) =>
                  print ("Key " ^ Int.toString x ^ ": " ^
                         Word32.toString k ^ "\n")) keys;
      *)
      keys
    end

  fun crypt ks (left, right) =
    let
      val (left, right) = initial_permutation(left, right)
                               (* swap back! *)
      fun loop 16 (left, right) = (right, left)
        | loop round (left, right) =
        let
          (*
          val _ = print ("Round " ^ Int.toString round ^
                         ": " ^ Word32.toString left ^ " / " ^
                         Word32.toString right ^ "\n")
          *)
          val left = des_round(right, left, ks, round * 2)
        in
          loop (round + 1) (right, left)
        end

      val (left, right) = loop 0 (left, right)

        (*
      val _ = print ("Before perm: " ^ Word32.toString left ^ " / " ^
                     Word32.toString right ^ "\n")
         *)

      val (left, right) = final_permutation (left, right)
    in
      (left, right)
    end

  fun encrypt keys =
      crypt (fn x => Array.sub(keys, x))

  fun decrypt keys =
      crypt (fn x => Array.sub(keys, if x mod 2 = 0
                                     then 30 - x
                                     else 32 - x))
    
end
