
functor BlowfishFn(A : W32ARRAY) :> BLOWFISH =
struct

  exception Unimplemented

  type fish =
    { (* length 18 *)
      p  : A.array,
      (* reversed *)
      pr : A.array,
      (* length 256 *)
      s1 : A.array,
      s2 : A.array,
      s3 : A.array,
      s4 : A.array }
    
  val po : Word32.word list = 
    [0wx243f6a88, 0wx85a308d3, 0wx13198a2e, 0wx03707344,
     0wxa4093822, 0wx299f31d0, 0wx082efa98, 0wxec4e6c89,
     0wx452821e6, 0wx38d01377, 0wxbe5466cf, 0wx34e90c6c,
     0wxc0ac29b7, 0wxc97c50dd, 0wx3f84d5b5, 0wxb5470917,
     0wx9216d5d9, 0wx8979fb1b]

  val s1o : Word32.word list =
    [0wxd1310ba6, 0wx98dfb5ac, 0wx2ffd72db, 0wxd01adfb7, 
     0wxb8e1afed, 0wx6a267e96, 0wxba7c9045, 0wxf12c7f99, 
     0wx24a19947, 0wxb3916cf7, 0wx0801f2e2, 0wx858efc16, 
     0wx636920d8, 0wx71574e69, 0wxa458fea3, 0wxf4933d7e, 
     0wx0d95748f, 0wx728eb658, 0wx718bcd58, 0wx82154aee, 
     0wx7b54a41d, 0wxc25a59b5, 0wx9c30d539, 0wx2af26013, 
     0wxc5d1b023, 0wx286085f0, 0wxca417918, 0wxb8db38ef, 
     0wx8e79dcb0, 0wx603a180e, 0wx6c9e0e8b, 0wxb01e8a3e, 
     0wxd71577c1, 0wxbd314b27, 0wx78af2fda, 0wx55605c60, 
     0wxe65525f3, 0wxaa55ab94, 0wx57489862, 0wx63e81440, 
     0wx55ca396a, 0wx2aab10b6, 0wxb4cc5c34, 0wx1141e8ce, 
     0wxa15486af, 0wx7c72e993, 0wxb3ee1411, 0wx636fbc2a, 
     0wx2ba9c55d, 0wx741831f6, 0wxce5c3e16, 0wx9b87931e, 
     0wxafd6ba33, 0wx6c24cf5c, 0wx7a325381, 0wx28958677, 
     0wx3b8f4898, 0wx6b4bb9af, 0wxc4bfe81b, 0wx66282193, 
     0wx61d809cc, 0wxfb21a991, 0wx487cac60, 0wx5dec8032, 
     0wxef845d5d, 0wxe98575b1, 0wxdc262302, 0wxeb651b88, 
     0wx23893e81, 0wxd396acc5, 0wx0f6d6ff3, 0wx83f44239, 
     0wx2e0b4482, 0wxa4842004, 0wx69c8f04a, 0wx9e1f9b5e, 
     0wx21c66842, 0wxf6e96c9a, 0wx670c9c61, 0wxabd388f0, 
     0wx6a51a0d2, 0wxd8542f68, 0wx960fa728, 0wxab5133a3, 
     0wx6eef0b6c, 0wx137a3be4, 0wxba3bf050, 0wx7efb2a98, 
     0wxa1f1651d, 0wx39af0176, 0wx66ca593e, 0wx82430e88, 
     0wx8cee8619, 0wx456f9fb4, 0wx7d84a5c3, 0wx3b8b5ebe, 
     0wxe06f75d8, 0wx85c12073, 0wx401a449f, 0wx56c16aa6, 
     0wx4ed3aa62, 0wx363f7706, 0wx1bfedf72, 0wx429b023d, 
     0wx37d0d724, 0wxd00a1248, 0wxdb0fead3, 0wx49f1c09b, 
     0wx075372c9, 0wx80991b7b, 0wx25d479d8, 0wxf6e8def7, 
     0wxe3fe501a, 0wxb6794c3b, 0wx976ce0bd, 0wx04c006ba, 
     0wxc1a94fb6, 0wx409f60c4, 0wx5e5c9ec2, 0wx196a2463, 
     0wx68fb6faf, 0wx3e6c53b5, 0wx1339b2eb, 0wx3b52ec6f, 
     0wx6dfc511f, 0wx9b30952c, 0wxcc814544, 0wxaf5ebd09, 
     0wxbee3d004, 0wxde334afd, 0wx660f2807, 0wx192e4bb3, 
     0wxc0cba857, 0wx45c8740f, 0wxd20b5f39, 0wxb9d3fbdb, 
     0wx5579c0bd, 0wx1a60320a, 0wxd6a100c6, 0wx402c7279, 
     0wx679f25fe, 0wxfb1fa3cc, 0wx8ea5e9f8, 0wxdb3222f8, 
     0wx3c7516df, 0wxfd616b15, 0wx2f501ec8, 0wxad0552ab, 
     0wx323db5fa, 0wxfd238760, 0wx53317b48, 0wx3e00df82, 
     0wx9e5c57bb, 0wxca6f8ca0, 0wx1a87562e, 0wxdf1769db, 
     0wxd542a8f6, 0wx287effc3, 0wxac6732c6, 0wx8c4f5573, 
     0wx695b27b0, 0wxbbca58c8, 0wxe1ffa35d, 0wxb8f011a0, 
     0wx10fa3d98, 0wxfd2183b8, 0wx4afcb56c, 0wx2dd1d35b, 
     0wx9a53e479, 0wxb6f84565, 0wxd28e49bc, 0wx4bfb9790, 
     0wxe1ddf2da, 0wxa4cb7e33, 0wx62fb1341, 0wxcee4c6e8, 
     0wxef20cada, 0wx36774c01, 0wxd07e9efe, 0wx2bf11fb4, 
     0wx95dbda4d, 0wxae909198, 0wxeaad8e71, 0wx6b93d5a0, 
     0wxd08ed1d0, 0wxafc725e0, 0wx8e3c5b2f, 0wx8e7594b7, 
     0wx8ff6e2fb, 0wxf2122b64, 0wx8888b812, 0wx900df01c, 
     0wx4fad5ea0, 0wx688fc31c, 0wxd1cff191, 0wxb3a8c1ad, 
     0wx2f2f2218, 0wxbe0e1777, 0wxea752dfe, 0wx8b021fa1, 
     0wxe5a0cc0f, 0wxb56f74e8, 0wx18acf3d6, 0wxce89e299, 
     0wxb4a84fe0, 0wxfd13e0b7, 0wx7cc43b81, 0wxd2ada8d9, 
     0wx165fa266, 0wx80957705, 0wx93cc7314, 0wx211a1477, 
     0wxe6ad2065, 0wx77b5fa86, 0wxc75442f5, 0wxfb9d35cf, 
     0wxebcdaf0c, 0wx7b3e89a0, 0wxd6411bd3, 0wxae1e7e49, 
     0wx00250e2d, 0wx2071b35e, 0wx226800bb, 0wx57b8e0af, 
     0wx2464369b, 0wxf009b91e, 0wx5563911d, 0wx59dfa6aa, 
     0wx78c14389, 0wxd95a537f, 0wx207d5ba2, 0wx02e5b9c5, 
     0wx83260376, 0wx6295cfa9, 0wx11c81968, 0wx4e734a41, 
     0wxb3472dca, 0wx7b14a94a, 0wx1b510052, 0wx9a532915, 
     0wxd60f573f, 0wxbc9bc6e4, 0wx2b60a476, 0wx81e67400, 
     0wx08ba6fb5, 0wx571be91f, 0wxf296ec6b, 0wx2a0dd915, 
     0wxb6636521, 0wxe7b9f9b6, 0wxff34052e, 0wxc5855664, 
     0wx53b02d5d, 0wxa99f8fa1, 0wx08ba4799, 0wx6e85076a]

  val s2o : Word32.word list =
    [0wx4b7a70e9, 0wxb5b32944, 0wxdb75092e, 0wxc4192623, 
     0wxad6ea6b0, 0wx49a7df7d, 0wx9cee60b8, 0wx8fedb266, 
     0wxecaa8c71, 0wx699a17ff, 0wx5664526c, 0wxc2b19ee1, 
     0wx193602a5, 0wx75094c29, 0wxa0591340, 0wxe4183a3e, 
     0wx3f54989a, 0wx5b429d65, 0wx6b8fe4d6, 0wx99f73fd6, 
     0wxa1d29c07, 0wxefe830f5, 0wx4d2d38e6, 0wxf0255dc1, 
     0wx4cdd2086, 0wx8470eb26, 0wx6382e9c6, 0wx021ecc5e, 
     0wx09686b3f, 0wx3ebaefc9, 0wx3c971814, 0wx6b6a70a1, 
     0wx687f3584, 0wx52a0e286, 0wxb79c5305, 0wxaa500737, 
     0wx3e07841c, 0wx7fdeae5c, 0wx8e7d44ec, 0wx5716f2b8, 
     0wxb03ada37, 0wxf0500c0d, 0wxf01c1f04, 0wx0200b3ff, 
     0wxae0cf51a, 0wx3cb574b2, 0wx25837a58, 0wxdc0921bd, 
     0wxd19113f9, 0wx7ca92ff6, 0wx94324773, 0wx22f54701, 
     0wx3ae5e581, 0wx37c2dadc, 0wxc8b57634, 0wx9af3dda7, 
     0wxa9446146, 0wx0fd0030e, 0wxecc8c73e, 0wxa4751e41, 
     0wxe238cd99, 0wx3bea0e2f, 0wx3280bba1, 0wx183eb331, 
     0wx4e548b38, 0wx4f6db908, 0wx6f420d03, 0wxf60a04bf, 
     0wx2cb81290, 0wx24977c79, 0wx5679b072, 0wxbcaf89af, 
     0wxde9a771f, 0wxd9930810, 0wxb38bae12, 0wxdccf3f2e, 
     0wx5512721f, 0wx2e6b7124, 0wx501adde6, 0wx9f84cd87, 
     0wx7a584718, 0wx7408da17, 0wxbc9f9abc, 0wxe94b7d8c, 
     0wxec7aec3a, 0wxdb851dfa, 0wx63094366, 0wxc464c3d2, 
     0wxef1c1847, 0wx3215d908, 0wxdd433b37, 0wx24c2ba16, 
     0wx12a14d43, 0wx2a65c451, 0wx50940002, 0wx133ae4dd, 
     0wx71dff89e, 0wx10314e55, 0wx81ac77d6, 0wx5f11199b, 
     0wx043556f1, 0wxd7a3c76b, 0wx3c11183b, 0wx5924a509, 
     0wxf28fe6ed, 0wx97f1fbfa, 0wx9ebabf2c, 0wx1e153c6e, 
     0wx86e34570, 0wxeae96fb1, 0wx860e5e0a, 0wx5a3e2ab3, 
     0wx771fe71c, 0wx4e3d06fa, 0wx2965dcb9, 0wx99e71d0f, 
     0wx803e89d6, 0wx5266c825, 0wx2e4cc978, 0wx9c10b36a, 
     0wxc6150eba, 0wx94e2ea78, 0wxa5fc3c53, 0wx1e0a2df4, 
     0wxf2f74ea7, 0wx361d2b3d, 0wx1939260f, 0wx19c27960, 
     0wx5223a708, 0wxf71312b6, 0wxebadfe6e, 0wxeac31f66, 
     0wxe3bc4595, 0wxa67bc883, 0wxb17f37d1, 0wx018cff28, 
     0wxc332ddef, 0wxbe6c5aa5, 0wx65582185, 0wx68ab9802, 
     0wxeecea50f, 0wxdb2f953b, 0wx2aef7dad, 0wx5b6e2f84, 
     0wx1521b628, 0wx29076170, 0wxecdd4775, 0wx619f1510, 
     0wx13cca830, 0wxeb61bd96, 0wx0334fe1e, 0wxaa0363cf, 
     0wxb5735c90, 0wx4c70a239, 0wxd59e9e0b, 0wxcbaade14, 
     0wxeecc86bc, 0wx60622ca7, 0wx9cab5cab, 0wxb2f3846e, 
     0wx648b1eaf, 0wx19bdf0ca, 0wxa02369b9, 0wx655abb50, 
     0wx40685a32, 0wx3c2ab4b3, 0wx319ee9d5, 0wxc021b8f7, 
     0wx9b540b19, 0wx875fa099, 0wx95f7997e, 0wx623d7da8, 
     0wxf837889a, 0wx97e32d77, 0wx11ed935f, 0wx16681281, 
     0wx0e358829, 0wxc7e61fd6, 0wx96dedfa1, 0wx7858ba99, 
     0wx57f584a5, 0wx1b227263, 0wx9b83c3ff, 0wx1ac24696, 
     0wxcdb30aeb, 0wx532e3054, 0wx8fd948e4, 0wx6dbc3128, 
     0wx58ebf2ef, 0wx34c6ffea, 0wxfe28ed61, 0wxee7c3c73, 
     0wx5d4a14d9, 0wxe864b7e3, 0wx42105d14, 0wx203e13e0, 
     0wx45eee2b6, 0wxa3aaabea, 0wxdb6c4f15, 0wxfacb4fd0, 
     0wxc742f442, 0wxef6abbb5, 0wx654f3b1d, 0wx41cd2105, 
     0wxd81e799e, 0wx86854dc7, 0wxe44b476a, 0wx3d816250, 
     0wxcf62a1f2, 0wx5b8d2646, 0wxfc8883a0, 0wxc1c7b6a3, 
     0wx7f1524c3, 0wx69cb7492, 0wx47848a0b, 0wx5692b285, 
     0wx095bbf00, 0wxad19489d, 0wx1462b174, 0wx23820e00, 
     0wx58428d2a, 0wx0c55f5ea, 0wx1dadf43e, 0wx233f7061, 
     0wx3372f092, 0wx8d937e41, 0wxd65fecf1, 0wx6c223bdb, 
     0wx7cde3759, 0wxcbee7460, 0wx4085f2a7, 0wxce77326e, 
     0wxa6078084, 0wx19f8509e, 0wxe8efd855, 0wx61d99735, 
     0wxa969a7aa, 0wxc50c06c2, 0wx5a04abfc, 0wx800bcadc, 
     0wx9e447a2e, 0wxc3453484, 0wxfdd56705, 0wx0e1e9ec9, 
     0wxdb73dbd3, 0wx105588cd, 0wx675fda79, 0wxe3674340, 
     0wxc5c43465, 0wx713e38d8, 0wx3d28f89e, 0wxf16dff20, 
     0wx153e21e7, 0wx8fb03d4a, 0wxe6e39f2b, 0wxdb83adf7]

  val s3o : Word32.word list =
    [0wxe93d5a68, 0wx948140f7, 0wxf64c261c, 0wx94692934, 
     0wx411520f7, 0wx7602d4f7, 0wxbcf46b2e, 0wxd4a20068, 
     0wxd4082471, 0wx3320f46a, 0wx43b7d4b7, 0wx500061af, 
     0wx1e39f62e, 0wx97244546, 0wx14214f74, 0wxbf8b8840, 
     0wx4d95fc1d, 0wx96b591af, 0wx70f4ddd3, 0wx66a02f45, 
     0wxbfbc09ec, 0wx03bd9785, 0wx7fac6dd0, 0wx31cb8504, 
     0wx96eb27b3, 0wx55fd3941, 0wxda2547e6, 0wxabca0a9a, 
     0wx28507825, 0wx530429f4, 0wx0a2c86da, 0wxe9b66dfb, 
     0wx68dc1462, 0wxd7486900, 0wx680ec0a4, 0wx27a18dee, 
     0wx4f3ffea2, 0wxe887ad8c, 0wxb58ce006, 0wx7af4d6b6, 
     0wxaace1e7c, 0wxd3375fec, 0wxce78a399, 0wx406b2a42, 
     0wx20fe9e35, 0wxd9f385b9, 0wxee39d7ab, 0wx3b124e8b, 
     0wx1dc9faf7, 0wx4b6d1856, 0wx26a36631, 0wxeae397b2, 
     0wx3a6efa74, 0wxdd5b4332, 0wx6841e7f7, 0wxca7820fb, 
     0wxfb0af54e, 0wxd8feb397, 0wx454056ac, 0wxba489527, 
     0wx55533a3a, 0wx20838d87, 0wxfe6ba9b7, 0wxd096954b, 
     0wx55a867bc, 0wxa1159a58, 0wxcca92963, 0wx99e1db33, 
     0wxa62a4a56, 0wx3f3125f9, 0wx5ef47e1c, 0wx9029317c, 
     0wxfdf8e802, 0wx04272f70, 0wx80bb155c, 0wx05282ce3, 
     0wx95c11548, 0wxe4c66d22, 0wx48c1133f, 0wxc70f86dc, 
     0wx07f9c9ee, 0wx41041f0f, 0wx404779a4, 0wx5d886e17, 
     0wx325f51eb, 0wxd59bc0d1, 0wxf2bcc18f, 0wx41113564, 
     0wx257b7834, 0wx602a9c60, 0wxdff8e8a3, 0wx1f636c1b, 
     0wx0e12b4c2, 0wx02e1329e, 0wxaf664fd1, 0wxcad18115, 
     0wx6b2395e0, 0wx333e92e1, 0wx3b240b62, 0wxeebeb922, 
     0wx85b2a20e, 0wxe6ba0d99, 0wxde720c8c, 0wx2da2f728, 
     0wxd0127845, 0wx95b794fd, 0wx647d0862, 0wxe7ccf5f0, 
     0wx5449a36f, 0wx877d48fa, 0wxc39dfd27, 0wxf33e8d1e, 
     0wx0a476341, 0wx992eff74, 0wx3a6f6eab, 0wxf4f8fd37, 
     0wxa812dc60, 0wxa1ebddf8, 0wx991be14c, 0wxdb6e6b0d, 
     0wxc67b5510, 0wx6d672c37, 0wx2765d43b, 0wxdcd0e804, 
     0wxf1290dc7, 0wxcc00ffa3, 0wxb5390f92, 0wx690fed0b, 
     0wx667b9ffb, 0wxcedb7d9c, 0wxa091cf0b, 0wxd9155ea3, 
     0wxbb132f88, 0wx515bad24, 0wx7b9479bf, 0wx763bd6eb, 
     0wx37392eb3, 0wxcc115979, 0wx8026e297, 0wxf42e312d, 
     0wx6842ada7, 0wxc66a2b3b, 0wx12754ccc, 0wx782ef11c, 
     0wx6a124237, 0wxb79251e7, 0wx06a1bbe6, 0wx4bfb6350, 
     0wx1a6b1018, 0wx11caedfa, 0wx3d25bdd8, 0wxe2e1c3c9, 
     0wx44421659, 0wx0a121386, 0wxd90cec6e, 0wxd5abea2a, 
     0wx64af674e, 0wxda86a85f, 0wxbebfe988, 0wx64e4c3fe, 
     0wx9dbc8057, 0wxf0f7c086, 0wx60787bf8, 0wx6003604d, 
     0wxd1fd8346, 0wxf6381fb0, 0wx7745ae04, 0wxd736fccc, 
     0wx83426b33, 0wxf01eab71, 0wxb0804187, 0wx3c005e5f, 
     0wx77a057be, 0wxbde8ae24, 0wx55464299, 0wxbf582e61, 
     0wx4e58f48f, 0wxf2ddfda2, 0wxf474ef38, 0wx8789bdc2, 
     0wx5366f9c3, 0wxc8b38e74, 0wxb475f255, 0wx46fcd9b9, 
     0wx7aeb2661, 0wx8b1ddf84, 0wx846a0e79, 0wx915f95e2, 
     0wx466e598e, 0wx20b45770, 0wx8cd55591, 0wxc902de4c, 
     0wxb90bace1, 0wxbb8205d0, 0wx11a86248, 0wx7574a99e, 
     0wxb77f19b6, 0wxe0a9dc09, 0wx662d09a1, 0wxc4324633, 
     0wxe85a1f02, 0wx09f0be8c, 0wx4a99a025, 0wx1d6efe10, 
     0wx1ab93d1d, 0wx0ba5a4df, 0wxa186f20f, 0wx2868f169, 
     0wxdcb7da83, 0wx573906fe, 0wxa1e2ce9b, 0wx4fcd7f52, 
     0wx50115e01, 0wxa70683fa, 0wxa002b5c4, 0wx0de6d027, 
     0wx9af88c27, 0wx773f8641, 0wxc3604c06, 0wx61a806b5, 
     0wxf0177a28, 0wxc0f586e0, 0wx006058aa, 0wx30dc7d62, 
     0wx11e69ed7, 0wx2338ea63, 0wx53c2dd94, 0wxc2c21634, 
     0wxbbcbee56, 0wx90bcb6de, 0wxebfc7da1, 0wxce591d76, 
     0wx6f05e409, 0wx4b7c0188, 0wx39720a3d, 0wx7c927c24, 
     0wx86e3725f, 0wx724d9db9, 0wx1ac15bb4, 0wxd39eb8fc, 
     0wxed545578, 0wx08fca5b5, 0wxd83d7cd3, 0wx4dad0fc4, 
     0wx1e50ef5e, 0wxb161e6f8, 0wxa28514d9, 0wx6c51133c, 
     0wx6fd5c7e7, 0wx56e14ec4, 0wx362abfce, 0wxddc6c837, 
     0wxd79a3234, 0wx92638212, 0wx670efa8e, 0wx406000e0]

  val s4o : Word32.word list =
    [0wx3a39ce37, 0wxd3faf5cf, 0wxabc27737, 0wx5ac52d1b, 
     0wx5cb0679e, 0wx4fa33742, 0wxd3822740, 0wx99bc9bbe, 
     0wxd5118e9d, 0wxbf0f7315, 0wxd62d1c7e, 0wxc700c47b, 
     0wxb78c1b6b, 0wx21a19045, 0wxb26eb1be, 0wx6a366eb4, 
     0wx5748ab2f, 0wxbc946e79, 0wxc6a376d2, 0wx6549c2c8, 
     0wx530ff8ee, 0wx468dde7d, 0wxd5730a1d, 0wx4cd04dc6, 
     0wx2939bbdb, 0wxa9ba4650, 0wxac9526e8, 0wxbe5ee304, 
     0wxa1fad5f0, 0wx6a2d519a, 0wx63ef8ce2, 0wx9a86ee22, 
     0wxc089c2b8, 0wx43242ef6, 0wxa51e03aa, 0wx9cf2d0a4, 
     0wx83c061ba, 0wx9be96a4d, 0wx8fe51550, 0wxba645bd6, 
     0wx2826a2f9, 0wxa73a3ae1, 0wx4ba99586, 0wxef5562e9, 
     0wxc72fefd3, 0wxf752f7da, 0wx3f046f69, 0wx77fa0a59, 
     0wx80e4a915, 0wx87b08601, 0wx9b09e6ad, 0wx3b3ee593, 
     0wxe990fd5a, 0wx9e34d797, 0wx2cf0b7d9, 0wx022b8b51, 
     0wx96d5ac3a, 0wx017da67d, 0wxd1cf3ed6, 0wx7c7d2d28, 
     0wx1f9f25cf, 0wxadf2b89b, 0wx5ad6b472, 0wx5a88f54c, 
     0wxe029ac71, 0wxe019a5e6, 0wx47b0acfd, 0wxed93fa9b, 
     0wxe8d3c48d, 0wx283b57cc, 0wxf8d56629, 0wx79132e28, 
     0wx785f0191, 0wxed756055, 0wxf7960e44, 0wxe3d35e8c, 
     0wx15056dd4, 0wx88f46dba, 0wx03a16125, 0wx0564f0bd, 
     0wxc3eb9e15, 0wx3c9057a2, 0wx97271aec, 0wxa93a072a, 
     0wx1b3f6d9b, 0wx1e6321f5, 0wxf59c66fb, 0wx26dcf319, 
     0wx7533d928, 0wxb155fdf5, 0wx03563482, 0wx8aba3cbb, 
     0wx28517711, 0wxc20ad9f8, 0wxabcc5167, 0wxccad925f, 
     0wx4de81751, 0wx3830dc8e, 0wx379d5862, 0wx9320f991, 
     0wxea7a90c2, 0wxfb3e7bce, 0wx5121ce64, 0wx774fbe32, 
     0wxa8b6e37e, 0wxc3293d46, 0wx48de5369, 0wx6413e680, 
     0wxa2ae0810, 0wxdd6db224, 0wx69852dfd, 0wx09072166, 
     0wxb39a460a, 0wx6445c0dd, 0wx586cdecf, 0wx1c20c8ae, 
     0wx5bbef7dd, 0wx1b588d40, 0wxccd2017f, 0wx6bb4e3bb, 
     0wxdda26a7e, 0wx3a59ff45, 0wx3e350a44, 0wxbcb4cdd5, 
     0wx72eacea8, 0wxfa6484bb, 0wx8d6612ae, 0wxbf3c6f47, 
     0wxd29be463, 0wx542f5d9e, 0wxaec2771b, 0wxf64e6370, 
     0wx740e0d8d, 0wxe75b1357, 0wxf8721671, 0wxaf537d5d, 
     0wx4040cb08, 0wx4eb4e2cc, 0wx34d2466a, 0wx0115af84, 
     0wxe1b00428, 0wx95983a1d, 0wx06b89fb4, 0wxce6ea048, 
     0wx6f3f3b82, 0wx3520ab82, 0wx011a1d4b, 0wx277227f8, 
     0wx611560b1, 0wxe7933fdc, 0wxbb3a792b, 0wx344525bd, 
     0wxa08839e1, 0wx51ce794b, 0wx2f32c9b7, 0wxa01fbac9, 
     0wxe01cc87e, 0wxbcc7d1f6, 0wxcf0111c3, 0wxa1e8aac7, 
     0wx1a908749, 0wxd44fbd9a, 0wxd0dadecb, 0wxd50ada38, 
     0wx0339c32a, 0wxc6913667, 0wx8df9317c, 0wxe0b12b4f, 
     0wxf79e59b7, 0wx43f5bb3a, 0wxf2d519ff, 0wx27d9459c, 
     0wxbf97222c, 0wx15e6fc2a, 0wx0f91fc71, 0wx9b941525, 
     0wxfae59361, 0wxceb69ceb, 0wxc2a86459, 0wx12baa8d1, 
     0wxb6c1075e, 0wxe3056a0c, 0wx10d25065, 0wxcb03a442, 
     0wxe0ec6e0e, 0wx1698db3b, 0wx4c98a0be, 0wx3278e964, 
     0wx9f1f9532, 0wxe0d392df, 0wxd3a0342b, 0wx8971f21e, 
     0wx1b0a7441, 0wx4ba3348c, 0wxc5be7120, 0wxc37632d8, 
     0wxdf359f8d, 0wx9b992f2e, 0wxe60b6f47, 0wx0fe3f11d, 
     0wxe54cda54, 0wx1edad891, 0wxce6279cf, 0wxcd3e7e6f, 
     0wx1618b166, 0wxfd2c1d05, 0wx848fd2c5, 0wxf6fb2299, 
     0wxf523f357, 0wxa6327623, 0wx93a83531, 0wx56cccd02, 
     0wxacf08162, 0wx5a75ebb5, 0wx6e163697, 0wx88d273cc, 
     0wxde966292, 0wx81b949d0, 0wx4c50901b, 0wx71c65614, 
     0wxe6c6c7bd, 0wx327a140a, 0wx45e1d006, 0wxc3f27b9a, 
     0wxc9aa53fd, 0wx62a80f00, 0wxbb25bfe2, 0wx35bdd2f6, 
     0wx71126905, 0wxb2040222, 0wxb6cbcf7c, 0wxcd769c2b, 
     0wx53113ec0, 0wx1640e3d3, 0wx38abbd60, 0wx2547adf0, 
     0wxba38209c, 0wxf746ce76, 0wx77afa1c5, 0wx20756060, 
     0wx85cbfe4e, 0wx8ae88dd8, 0wx7aaaf9b0, 0wx4cf9aa7e, 
     0wx1948c25c, 0wx02fb8a8c, 0wx01c36ae4, 0wxd6ebe1f9, 
     0wx90d4f869, 0wxa65cdea0, 0wx3f09252d, 0wxc208e69f, 
     0wxb74e6132, 0wxce77e25b, 0wx578fdfe3, 0wx3ac372e6]

  fun initw v =
    let
      val p = A.fromList po
      val s1 = A.fromList s1o
      val s2 = A.fromList s2o
      val s3 = A.fromList s3o
      val s4 = A.fromList s4o

      val ctx = (p, s1, s2, s3, s4)

      local 
	val ++ = Word32.orb
	infix ++
      in
	fun mk32 (a, b, c, d) =
	  (Word32.<<(Word32.fromInt a, 0w24)) ++
	  (Word32.<<(Word32.fromInt b, 0w16)) ++
	  (Word32.<<(Word32.fromInt c, 0w8)) ++
	  Word32.fromInt d
      end

      fun keyat n = 
	   Word8.toInt (Word8Vector.sub 
			(v, n mod Word8Vector.length v))

      (* use the key to initialize p *)
      fun xorize 18 _ = ()
	| xorize n m =
	let in
	  A.update(p, n,
		   Word32.xorb
		   (A.sub(p, n),
		    mk32 (keyat m,
			  keyat (m + 1),
			  keyat (m + 2),
			  keyat (m + 3))));
	  xorize (n + 1) (m + 4)
	end

      val _ = xorize 0 0

      (* update the array a with n/2 new words,
	 starting at offset s. *)
      fun update _ _ 0 d = d
	| update a s n d =
	let val (d as (vl, vr)) = cipher ctx d
	in 
	  A.update(a, s, vl);
	  A.update(a, s + 1, vr);
	  update a (s + 2) (n - 2) d
	end

      val d = update p 0 18 (0w0, 0w0)
      val d = update s1 0 256 d
      val d = update s2 0 256 d
      val d = update s3 0 256 d
      val d = update s4 0 256 d

    in
      { p = p, pr = A.fromList (A.foldl op:: nil p),
        s1 = s1, s2 = s2, s3 = s3, s4 = s4 }
    end

  and encrypt1 {p, pr, s1, s2, s3, s4} d =
      cipher (p, s1, s2, s3, s4) d

  and decrypt1 {p, pr, s1, s2, s3, s4} d =
      cipher (pr, s1, s2, s3, s4) d

  and cipher (p, s1, s2, s3, s4) (xl, xr) =
    let
      
      fun f x =
	let
	  val a = Word32.andb(0wx00FF, Word32.>>(x, 0w24))
	  val b = Word32.andb(0wx00FF, Word32.>>(x, 0w16))
	  val c = Word32.andb(0wx00FF, Word32.>>(x, 0w8))
	  val d = Word32.andb(0wx00FF, x)
	in
	  Word32.+
	    (Word32.xorb
	       (Word32.+(A.sub(s1, Word32.toInt a),
			 A.sub(s2, Word32.toInt b)),
		A.sub(s3, Word32.toInt c)),
	     A.sub(s4, Word32.toInt d))
	end

      fun loop (16, xr, xl) =
	(Word32.xorb (xl, A.sub(p, 17)),
	 Word32.xorb (xr, A.sub(p, 16)))
	| loop (n, xl, xr) =
	let 
	  val xl = Word32.xorb (xl, A.sub(p, n))
	  val xr = Word32.xorb (f xl, xr)
	in
	  (* recurse, swapping xl,xr *)
	  loop (n + 1, xr, xl)
	end

    in
      loop (0, xl, xr)
    end

  fun init s =
    initw (Word8Vector.tabulate
	   (size s,
	    fn i => Word8.fromInt (ord (CharVector.sub(s, i)))))


  fun is_weak _ = raise Unimplemented

end


structure Blowfish = 
    BlowfishFn(type array = Word32.word Array.array
	       val sub = Array.sub
	       val update = Array.update
	       val fromList = Array.fromList
	       val array = Array.array
	       val foldl = Array.foldl)
