(* Copyright 2010 Tom Murphy VII and Erin Catto. See COPYING for details. *)

(* Math types and utilities used throughout BDD.
   Corresponding to common/b2math.h. *)
structure BDDMath :> BDDMATH =
struct

  exception Unimplemented of string

  (* This function is used to ensure that a floating point number is
     not a NaN or infinity. *)
  val is_valid = Real.isFinite (* checks both NaN and infs. *)

  (* This is a approximate yet fast inverse square-root. *)
  fun inv_sqrt x = 1.0 / Math.sqrt x
  (* PERF: Use magic 0x5f3759df method.
{
        union
        {
                float32 x;
                int32 i;
        } convert;

        convert.x = x;
        float32 xhalf = 0.5f * x;
        convert.i = 0x5f3759df - (convert.i >> 1);
        x = convert.x;
        x = x * (1.5f - xhalf * x * x);
        return x;
}
*)

  val sqrt = Math.sqrt
  val atan2 = Math.atan2
  val abs = Real.abs

  (* A 2D column vector. *)
  type vec2 = { x : real ref, y : real ref }

  fun vec2 (x, y) = { x = ref x, y = ref y }
  fun vec2copy ({x, y} : vec2) = vec2(!x, !y)
  fun vec2x ({x, y = _} : vec2) = !x
  fun vec2y ({x = _, y} : vec2) = !y
  fun vec2xy {x, y} = (!x, !y)
  fun vec2setzero {x, y} = (x := 0.0; y := 0.0)
  fun vec2set ({x, y}, xx, yy) = (x := xx; y := yy)
  fun vec2setfrom ({x, y} : vec2, {x = xx, y = yy} : vec2) =
      (x := !xx; y := !yy)
  fun vec2neg ({x, y} : vec2) = { x = ref (0.0 - !x), y = ref (0.0 - !y) }
  fun vec2idx ({x, y} : vec2) 0 = !x
    | vec2idx {x, y} 1 = !y
    | vec2idx _ _ = raise Subscript

  fun vec2pluseq ({x, y} : vec2, {x = ref xx, y = ref yy} : vec2) =
      (x := !x + xx; y := !y + yy)

  fun vec2minuseq ({x, y} : vec2, {x = ref xx, y = ref yy} : vec2) =
      (x := !x - xx; y := !y - yy)

  fun vec2timeseq ({x, y} : vec2, a : real) =
      (x := !x * a; y := !y * a)

  fun vec2length {x = ref x, y = ref y} = sqrt(x * x + y * y)

  fun vec2length_squared ({x = ref x, y = ref y} : vec2) = x * x + y * y

  (* Convert into a unit vector with the same direction.
     Returns the (old) length. *)
  fun vec2normalize (v as {x, y}) =
      let val length = vec2length v
      in
          if length < BDDSettings.epsilon
          then 0.0
          else let val inv = 1.0 / length
               in
                   x := !x * inv;
                   y := !y * inv;
                   length
               end
      end

  fun vec2normalized (v as {x, y}) : vec2 =
      let val length = vec2length v
      in if length < BDDSettings.epsilon
         then vec2copy v
         else let val inv = 1.0 / length
              in vec2 (!x * inv, !y * inv)
              end
      end

  fun vec2is_valid {x = ref x, y = ref y} =
      is_valid x andalso is_valid y
      

  type vec3 = { x : real ref, y : real ref, z : real ref }
  fun vec3 (x, y, z) = { x = ref x, y = ref y, z = ref z }
  fun vec3copy ({x, y, z} : vec3) = vec3 (!x, !y, !z)
  fun vec3x ({x, y, z} : vec3) = !x
  fun vec3y ({x, y, z} : vec3) = !y
  fun vec3z ({x, y, z} : vec3) = !z
  fun vec3zero {x, y, z} = (x := 0.0; y := 0.0; z := 0.0)
  fun vec3set ({x, y, z}, xx, yy, zz) = (x := xx; y := yy; z := zz)
  fun vec3neg ({x, y, z} : vec3) = { x = ref (0.0 - !x), 
                                     y = ref (0.0 - !y), 
                                     z = ref (0.0 - !z) }
  fun vec3idx ({x, y, z} : vec3) 0 = !x
    | vec3idx {x, y, z} 1 = !y
    | vec3idx {x, y, z} 2 = !z
    | vec3idx _ _ = raise Subscript

  fun vec3pluseq ({x, y, z} : vec3, {x = ref xx, y = ref yy, z = ref zz}) =
      (x := !x + xx; y := !y + yy; z := !z + zz)

  fun vec3minuseq ({x, y, z} : vec3, {x = ref xx, y = ref yy, z = ref zz}) =
      (x := !x - xx; y := !y - yy; z := !z - zz)

  fun vec3timeseq ({x, y, z} : vec3, a : real) =
      (x := !x * a; y := !y * a; z := !z * a)

  (* 2x2 matrix; column-major order. *)
  type mat22 = { col1 : vec2, col2 : vec2 }

  fun mat22cols (col1, col2) = { col1 = vec2copy col1,
                                 col2 = vec2copy col2 }

  fun mat22copy { col1, col2 } = mat22cols (col1, col2)

  fun mat22with (a11, a12, 
                 a21, a22) =
      { col1 = vec2(a11, 
                    a21),
                            col2 = vec2(a12, 
                                        a22) }

  (* Construct this matrix using an angle. This matrix becomes
     an orthonormal rotation matrix. *)
  fun mat22angle angle =
      (* PERF compute sin and cos together *)
      let val c = Math.cos angle
          val s = Math.sin angle
      in
          mat22with ( c, s,
                     ~s, c)
      end


  fun mat22set ({ col1, col2 }, c1, c2) =
      (vec2setfrom (col1, c1);
       vec2setfrom (col2, c2))

  fun mat22setangle ({ col1, col2 } : mat22, angle) =
      let val c = Math.cos angle
          val s = Math.sin angle
      in
          vec2set(col1, c, ~s);
          vec2set(col2, s, c)
      end


  fun mat22setidentity ({ col1, col2 } : mat22) =
      (vec2set(col1, 1.0, 
                     0.0);
                             vec2set(col2, 0.0, 
                                           1.0))

  fun mat22setzero ({ col1, col2 } : mat22) =
      (vec2setzero col1; vec2setzero col2)

  fun mat22getangle ({ col1, col2 } : mat22) =
      atan2 (vec2y col1, vec2x col1)

  fun mat22inverse ({ col1, col2 } : mat22) =
      let
          val a = vec2x col1
          val b = vec2x col2
          val c = vec2y col1
          val d = vec2y col2
          val det = a * d - b * c
          val det = if Real.!= (det, 0.0)
                    then 1.0 / det
                    else det
      in
          mat22with(det * d, ~det * b,
                    ~det * c, det * a)
      end

  (* Solve A * x = b, where b is a column vector. This is more efficient
     than computing the inverse in one-shot cases. *)
  fun mat22solve ({ col1, col2 }, b : vec2) : vec2 =
      let
          val a11 = vec2x col1   val a12 = vec2x col2
          val a21 = vec2y col1   val a22 = vec2y col2

          val det = a11 * a22 - a12 * a21
          val det = if Real.!= (det, 0.0)
                    then 1.0 / det
                    else det
      in
          vec2(det * (a22 * vec2x b - a12 * vec2y b),
               det * (a11 * vec2y b - a21 * vec2x b))
      end


  type mat33 = { col1 : vec3, col2 : vec3, col3 : vec3 }

  fun mat33 (col1, col2, col3) = { col1 = col1, col2 = col2, col3 = col3 }

  fun mat33setzero { col1, col2, col3 } =
      (vec3zero col1; vec3zero col2; vec3zero col3)

  fun dot2(a : vec2, b : vec2) : real =
      vec2x a * vec2x b + vec2y a * vec2y b
  fun dot3(a : vec3, b : vec3) : real =
      vec3x a * vec3x b + vec3y a * vec3y b + vec3z a * vec3z b

  fun cross2vv(a : vec2, b : vec2) : real = 
      vec2x a * vec2y b - vec2y a * vec2x b
  fun cross2vs(a : vec2, s : real) : vec2 =
      vec2(s * vec2y a, ~s * vec2x a)
  fun cross2sv(s : real, a : vec2) : vec2 =
      vec2(~s * vec2y a, s * vec2x a)

  fun cross3vv(a : vec3, b : vec3) : vec3 =
      vec3 (vec3y a * vec3z b - vec3z a * vec3y b, 
            vec3z a * vec3x b - vec3x a * vec3z b, 
            vec3x a * vec3y b - vec3y a * vec3x b)

  fun mat33solve33 ({ col1, col2, col3 }, b : vec3) : vec3 =
      let
          val cross23 = cross3vv (col2, col3)
          val det = dot3 (col1, cross23)
          val det = if Real.!= (det, 0.0)
                    then 1.0 / det
                    else det
      in
          vec3(det * dot3(b, cross23),
               det * dot3(col1, cross3vv(b, col3)),
               det * dot3(col1, cross3vv(col2, b)))
      end

  fun mat33solve22 ({ col1, col2, col3 } : mat33, b : vec2) : vec2 =
      let
          val a11 = vec3x col1     val a12 = vec3x col2
          val a21 = vec3y col1     val a22 = vec3y col2
       
          val det = a11 * a22 - a12 * a21
          val det = if Real.!= (det, 0.0)
                    then 1.0 / det
                    else det
      in
          vec2(det * (a22 * vec2x b - a12 * vec2y b),
               det * (a11 * vec2y b - a21 * vec2x b))
      end

  type transform = { position : vec2, r : mat22 }
  fun transform (pp, rr) = { position = vec2copy pp, r = mat22copy rr }
  fun transform_pos_angle (pp, angle : real) =
      { position = vec2copy pp,
        r = mat22angle angle }
  fun transformposition { position, r } = position
  fun transformr ({ position, r } : transform) = r
  fun transform_setidentity ({ position, r } : transform) =
      (vec2setzero position;
       mat22setidentity r)

  fun identity_transform () = { position = vec2 (0.0, 0.0),
                                r = mat22with (1.0, 0.0,
                                               0.0, 1.0) }

  fun transform_set ({ position, r }, pp, rr : real) =
      (vec2set(position, vec2x pp, vec2y pp);
       mat22setangle(r, rr))

  fun transform_getangle { position, r } = mat22getangle r

  (* Don't modify these. *)
  val vec2_zero = vec2(0.0, 0.0)
  val mat22_identity = mat22with(1.0, 0.0,
                                 0.0, 1.0)
  val transform_identity = { position = vec2 (0.0, 0.0),
                             r = mat22_identity }

  (* Functional math on vectors, matrices, etc. *)

  fun vec2sub (a : vec2, b : vec2) : vec2 =
      vec2 (vec2x a - vec2x b, vec2y a - vec2y b)

  fun vec3sub (a : vec3, b : vec3) : vec3 =
      vec3 (vec3x a - vec3x b,
            vec3y a - vec3y b,
            vec3z a - vec3z b)

  fun vec2add (a : vec2, b : vec2) : vec2 =
      vec2 (vec2x a + vec2x b, vec2y a + vec2y b)

  fun vec3add (a : vec3, b : vec3) : vec3 =
      vec3 (vec3x a + vec3x b,
            vec3y a + vec3y b,
            vec3z a + vec3z b)

  fun mat22add (a : mat22, b : mat22) : mat22 =
      mat22cols (vec2add (#col1 a, #col1 b), vec2add (#col2 a, #col2 b)) 

  fun vec2stimes (s : real, v : vec2) : vec2 =
      vec2 (s * vec2x v, s * vec2y v)
  fun vec3stimes (s : real, v : vec3) : vec3 =
      vec3 (s * vec3x v, s * vec3y v, s * vec3z v)

  fun mul22v (a : mat22, v : vec2) : vec2 =
      vec2 (vec2x (#col1 a) * vec2x v +
            vec2x (#col2 a) * vec2y v,
            vec2y (#col1 a) * vec2x v +
            vec2y (#col2 a) * vec2y v)

  fun mul22m (a : mat22, b : mat22) : mat22 =
      mat22cols (mul22v(a, #col1 b), mul22v(a, #col2 b))

  fun mul33v (a : mat33, v : vec3) : vec3 =
      vec3add(vec3add (vec3stimes (vec3x v, #col1 a),
                       vec3stimes (vec3y v, #col2 a)),
              vec3stimes (vec3z v, #col3 a))

  fun multransformv (t : transform, v : vec2) : vec2 =
      let
          val x = vec2x (transformposition t) +
              vec2x (#col1 (transformr t)) *
              vec2x v +
              vec2x (#col2 (transformr t)) *
              vec2y v
          val y = vec2y (transformposition t) +
              vec2y (#col1 (transformr t)) *
              vec2x v +
              vec2y (#col2 (transformr t)) *
              vec2y v
      in
        vec2 (x, y)
      end

  fun vec2eq (a, b) = Real.== (vec2x a, vec2x b) andalso
      Real.== (vec2y a, vec2y b)

  fun vec2abs v = vec2 (abs (vec2x v), abs (vec2y v))
  fun mat22abs (a : mat22) = mat22cols (vec2abs (#col1 a), vec2abs (#col2 a))

  fun distance(a : vec2, b : vec2) =
      vec2length (vec2sub (a, b))

  fun distance_squared (a, b) =
      let val c = vec2sub (a, b)
      in dot2 (c, c)
      end

  fun vec2min (a : vec2, b : vec2) =
      vec2 (Real.min (vec2x a, vec2x b),
            Real.min (vec2y a, vec2y b))

  fun vec2max (a : vec2, b : vec2) =
      vec2 (Real.max (vec2x a, vec2x b),
            Real.max (vec2y a, vec2y b))

  fun clampr (a, low, high) =
      Real.max(low, Real.min(a, high))

  fun vec2clamp (a : vec2, low : vec2, high : vec2) : vec2 =
      vec2max (low, vec2min(a, high))

  (* These multiply the transpose of the matrix! *)
  fun mul_t22mv (a : mat22, v : vec2) : vec2 =
      vec2(dot2(v, #col1 a), dot2(v, #col2 a))
  fun mul_t22mm (a : mat22, b : mat22) : mat22 =
      let val c1 = vec2(dot2(#col1 a, #col1 b), dot2(#col2 a, #col1 b))
          val c2 = vec2(dot2(#col1 a, #col2 b), dot2(#col2 a, #col2 b))
      in
          mat22cols (c1, c2)
      end
  fun mul_ttransformv (t : transform, v : vec2) : vec2 =
      mul_t22mv (transformr t, vec2sub (v, transformposition t))

  (* Cool trick: By downshifting and oring, ensure that the bits are all ones
     starting with the first one in the input. Then the result plus 1 is the
     next largest power of two. *)
  fun next_power_of_two (w : Word32.word) =
      let
          val w = Word32.orb(w, Word32.>>(w, 0w1))
          val w = Word32.orb(w, Word32.>>(w, 0w2))
          val w = Word32.orb(w, Word32.>>(w, 0w4))
          val w = Word32.orb(w, Word32.>>(w, 0w8))
          val w = Word32.orb(w, Word32.>>(w, 0w16))
      in
          w + 0w1
      end

  fun is_power_of_two (w : Word32.word) =
      w <> 0w0 andalso Word32.andb(w, w - 0w1) = 0w0


  type sweep = { 
                 (* local center of mass position *)
                 local_center : vec2, 
                 (* center world positions *)
                 c0 : vec2,
                 c : vec2,
                 (* world angles *)
                 a0 : real ref,
                 a : real ref
               }
  fun sweep { local_center, c0, c, a0, a } : sweep =
      { local_center = local_center, c0 = c0, c = c, a0 = ref a0, a = ref a }

  fun sweepcopy { local_center, c0, c, a0, a } : sweep =
      { local_center = vec2copy local_center,
        c0 = vec2copy c0,
        c = vec2copy c,
        a0 = ref (!a0),
        a = ref (!a) }

  fun sweep_gettransform ({ local_center, c0, c, a0, a }, 
                          transform : transform, 
                          alpha : real) =
      let val angle : real = 1.0 - alpha * !a0 + alpha * !a
      in
          vec2setfrom (transformposition transform,
                       vec2add(vec2stimes (1.0 - alpha, c0),
                               vec2stimes (alpha, c)));
          mat22setangle (transformr transform, angle);

          vec2minuseq (transformposition transform,
                       mul22v(transformr transform,
                              local_center))
      end

  fun sweepa ({ a, ... } : sweep) = !a
  fun sweepc ({ c, ... } : sweep) = c
  fun sweeplocalcenter ({ local_center, ... } : sweep) = local_center

  (* PERF *)
  fun sweep_transform (arg, alpha : real) =
      let val transform = identity_transform ()
      in
          sweep_gettransform (arg, transform, alpha);
          transform
      end

  fun sweep_advance({c0, c, a0, a, ...} : sweep, t : real) =
      let in
          vec2setfrom(c0, vec2add(vec2stimes(1.0 - t, c0),
                                  vec2stimes(t, c)));
          a0 := (1.0 - t) * !a0 + t * !a
      end

  fun sweep_normalize ({ a0, a, ... } : sweep) =
      let 
          val twopi = 2.0 * BDDSettings.pi
          val d = twopi * real (Real.floor(!a0 / twopi))
      in
          a0 := !a0 - d;
          a := !a - d
      end

end
