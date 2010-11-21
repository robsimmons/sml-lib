(* Copyright 2010 Tom Murphy VII and Erin Catto. See COPYING for details. *)

(* Corresponding to dynamics/contacts/b2contactsolver.cpp *)
structure BDDContactSolver :> BDDCONTACT_SOLVER =
struct

  exception BDDContactSolver of string
  open BDDMath BDDTypes BDDSettings BDDOps
  infix 6 :+: :-: %-% %+% +++
  infix 7 *: *% +*: +*+ #*% @*:
  structure D = BDDDynamics

  type constraint_point =
      { local_point : BDDMath.vec2,
        r_a : BDDMath.vec2,
        r_b : BDDMath.vec2,
        normal_impulse : real ref,
        tangent_impulse : real ref,
        normal_mass : real,
        tangent_mass : real,
        velocity_bias : real }

  type ('b, 'f, 'j) constraint =
      { points : constraint_point Array.array,
        local_normal : BDDMath.vec2,
        local_point : BDDMath.vec2,
        normal : BDDMath.vec2,
        normal_mass : BDDMath.mat22,
        k : BDDMath.mat22,
        body_a : ('b, 'f, 'j) BDDDynamics.body,
        body_b : ('b, 'f, 'j) BDDDynamics.body,
        typ : BDDTypes.manifold_type,
        radius : real,
        friction : real,
        point_count : int,
        manifold : BDDTypes.manifold }

  (* Parameterized by user data, since it uses the internal
     polymorpic types. *)
  type ('b, 'f, 'j) contact_solver =
      (* Representation invariant: These two have the
         same length. *)
      { constraints : ('b, 'f, 'j) constraint array,
        (* Kept for 'report'; just a copy of island's contacts. *)
        contacts : ('b, 'f, 'j) BDDDynamics.contact Vector.vector }
        

  fun contact_solver 
      (contacts : ('b, 'f, 'j) BDDDynamics.contact Vector.vector,
       impulse_ratio : real) : ('b, 'f, 'j) contact_solver =
    let
        (* Convert a contact into a constraint. *)
        fun onecontact (contact : ('b, 'f, 'j) BDDDynamics.contact) =
          let
            val fixture_a = D.C.get_fixture_a contact
            val fixture_b = D.C.get_fixture_b contact
            val shape_a = D.F.get_shape fixture_a
            val shape_b = D.F.get_shape fixture_b
            val radius_a = BDDShape.get_radius shape_a
            val radius_b = BDDShape.get_radius shape_b
            val body_a = D.F.get_body fixture_a
            val body_b = D.F.get_body fixture_b
            val manifold = D.C.get_manifold contact

            val friction = mix_friction(D.F.get_friction fixture_a,
                                        D.F.get_friction fixture_b)
            val restitution = mix_restitution(D.F.get_restitution fixture_a,
                                              D.F.get_restitution fixture_b)
            val v_a : vec2 = D.B.get_linear_velocity body_a
            val v_b : vec2 = D.B.get_linear_velocity body_b
            val w_a : real = D.B.get_angular_velocity body_a
            val w_b : real = D.B.get_angular_velocity body_b

            (* PERF assert *)
            val () = if #point_count manifold > 0
                     then ()
                     else raise BDDContactSolver "pointcount assertion"

            val world_manifold = 
                BDDCollision.create_world_manifold (manifold,
                                                    D.B.get_xf body_a, radius_a,
                                                    D.B.get_xf body_b, radius_b)

            val normal = #normal world_manifold

            fun one_point (j : int) : constraint_point =
              let
                val cp : manifold_point = Array.sub (#points manifold, j)
                val wmpj = Array.sub(#points world_manifold, j)
                val r_a = wmpj :-: sweepc (D.B.get_sweep body_a)
                val r_b = wmpj :-: sweepc (D.B.get_sweep body_b)
                val rn_a = cross2vv(r_a, normal)
                val rn_b = cross2vv(r_b, normal)
                val rn_a = rn_a * rn_a
                val rn_b = rn_b * rn_b
                val k_normal : real = 
                    D.B.get_inv_mass body_a +
                    D.B.get_inv_mass body_b +
                    D.B.get_inv_i body_a * rn_a +
                    D.B.get_inv_i body_b * rn_b

                (* PERF assert *)
                val () = if k_normal > epsilon
                         then ()
                         else raise BDDContactSolver "knormal assertion"

                val tangent : vec2 = cross2vs(normal, 1.0)
                val rt_a = cross2vv(r_a, tangent)
                val rt_b = cross2vv(r_b, tangent)
                val rt_a = rt_a * rt_a
                val rt_b = rt_b * rt_b

                val k_tangent = 
                    D.B.get_inv_mass body_a +
                    D.B.get_inv_mass body_b +
                    D.B.get_inv_i body_a * rt_a +
                    D.B.get_inv_i body_b * rt_b

                (* PERF assert *)
                val () = if k_tangent > epsilon
                         then ()
                         else raise BDDContactSolver "ktangent assertion"

                (* Set up a velocity bias for restitution. *)
                val v_rel : real = dot2(normal, 
                                        v_b :+: cross2sv(w_b, r_b) :-:
                                        v_a :-: cross2sv(w_a, r_a))
                val velocity_bias =
                  if v_rel < ~ velocity_threshold
                  then ~restitution * v_rel
                  else 0.0
              in
                { normal_impulse = ref (impulse_ratio * #normal_impulse cp),
                  tangent_impulse = ref (impulse_ratio * #tangent_impulse cp),
                  local_point = #local_point cp,
                  r_a = r_a,
                  r_b = r_b,
                  normal_mass = 1.0 / k_normal,
                  tangent_mass = 1.0 / k_tangent,
                  velocity_bias = velocity_bias } : constraint_point

              end

            val points = Array.tabulate(Array.length (#points manifold),
                                        one_point)

            val (k, normal_mass, points) =
                (* If we have two points, then prepare the block solver. *)
                if Array.length points = 2
                then 
                  let
                      val ccp1 = Array.sub(points, 0)
                      val ccp2 = Array.sub(points, 1)

                      val inv_mass_a = D.B.get_inv_mass body_a
                      val inv_i_a = D.B.get_inv_i body_a
                      val inv_mass_b = D.B.get_inv_mass body_b
                      val inv_i_b = D.B.get_inv_i body_b

                      val rn1_a = cross2vv(#r_a ccp1, normal)
                      val rn1_b = cross2vv(#r_b ccp1, normal)
                      val rn2_a = cross2vv(#r_a ccp2, normal)
                      val rn2_b = cross2vv(#r_b ccp2, normal)

                      val k11 = inv_mass_a + inv_mass_b + 
                          inv_i_a * rn1_a * rn1_a +
                          inv_i_b * rn1_b * rn1_b
                      val k22 = inv_mass_a + inv_mass_b +
                          inv_i_a * rn2_a * rn2_a + 
                          inv_i_b * rn2_b * rn2_b
                      val k12 = inv_mass_a + inv_mass_b +
                          inv_i_a * rn1_a * rn2_a +
                          inv_i_b * rn1_b * rn2_b

                      (* Ensure a reasonable condition number. *)
                      val MAX_CONDITION_NUMBER = 100.0
                  in
                      if k11 * k11 < 
                         MAX_CONDITION_NUMBER * (k11 * k22 - k12 * k12)
                      then (* K is safe to invert. *)
                          let
                              val k = mat22cols (vec2(k11, k12), vec2(k12, k22))
                              val norm = mat22inverse k
                          in
                              (k, norm, points)
                          end
                      else
                          (* The constraints are redundant; just use one.
                             TODO_ERIN: use deepest? *)
                          (mat22with (0.0, 0.0, 0.0, 0.0),
                           mat22with (0.0, 0.0, 0.0, 0.0),
                           Array.tabulate(1, fn _ => Array.sub(points, 0)))

                  end
                      (* PERF uninitialized *)
                else (mat22with (0.0, 0.0, 0.0, 0.0), 
                      mat22with (0.0, 0.0, 0.0, 0.0), 
                      points)
                
          in
              { body_a = body_a,
                body_b = body_b,
                manifold = manifold,
                normal = #normal world_manifold,
                point_count = Array.length points,
                friction = friction,
                local_normal = #local_normal manifold,
                local_point = #local_point manifold,
                radius = radius_a + radius_b,
                typ = #typ manifold,
                points = points,
                k = k,
                normal_mass = normal_mass }
          end
        val constraints = 
            Array.tabulate (Vector.length contacts,
                            fn x => onecontact (Vector.sub(contacts, x)))

        fun warm_start_one (c as { body_a, body_b,
                                   normal, points, ... } 
                            : ('b, 'f, 'j) constraint) : unit =
          let
            val tangent = cross2vs(normal, 1.0)
            val inv_mass_a = D.B.get_inv_mass body_a
            val inv_i_a = D.B.get_inv_i body_a
            val inv_mass_b = D.B.get_inv_mass body_b
            val inv_i_b = D.B.get_inv_i body_b

            fun warm_point (ccp as 
                            { normal_impulse, 
                              tangent_impulse, 
                              r_a, r_b, ... } : constraint_point) : unit =
              let
                val p : vec2 = 
                    !normal_impulse *: normal :+: !tangent_impulse *: tangent
              in
                D.B.set_angular_velocity (body_a,
                                          D.B.get_angular_velocity body_a -
                                          inv_i_a * cross2vv(r_a, p));
                D.B.set_linear_velocity (body_a,
                                         D.B.get_linear_velocity body_a :-:
                                         inv_mass_a *: p);
                D.B.set_angular_velocity (body_b,
                                          D.B.get_angular_velocity body_b -
                                          inv_i_b * cross2vv(r_b, p));
                D.B.set_linear_velocity (body_b,
                                         D.B.get_linear_velocity body_b :-:
                                         inv_mass_b *: p)
              end
          in
            Array.app warm_point points
          end
    in
        (* Port note: Rolled the warm start function, which is always
           called immediately after initialization, into this. *)
        Array.app warm_start_one constraints;
        { contacts = contacts,
          constraints = constraints }
    end

  (* Port note: Inner case analysis in solve_one_velocity_constraints,
     for two points. The Box2D code has a for(;;) loop, but this just
     appears to be so that the code can 'break' early. Here we just
     return. *)
  fun solve_loop (b : vec2, 
                  c : ('b, 'f, 'j) constraint, 
                  a : vec2, 
                  normal : vec2, 
                  v_a, inv_mass_a, w_a, inv_i_a,
                  v_b, inv_mass_b, w_b, inv_i_b,
                  cp1 : constraint_point, 
                  cp2 : constraint_point) : unit =
  let
    (* Only used in assertions. *)
    val ERROR_TOL : real = 1e~3

    (* Case 1: vn = 0
       0 = A * x' + b'
       Solve for x':
       x' = - inv(A) * b' 
    *)
    val x : vec2 = vec2neg (#normal_mass c +*: b)

    (* Port note: The body of each case is the same, and
       only depends on x. *)
    fun resubstitute_and_apply x =
      let 
        (* Resubstitute for the incremental impulse *)
        val d : vec2 = x :-: a
        (* Apply incremental update *)
        val p1 : vec2 = vec2x d *: normal
        val p2 : vec2 = vec2y d *: normal
      in
        v_a := !v_a :-: (inv_mass_a *: (p1 :+: p2));
        w_a := !w_a - (inv_i_a * (cross2vv (#r_a cp1, p1) +
                                  cross2vv (#r_a cp2, p2)));
        v_b := !v_b :+: (inv_mass_b *: (p1 :+: p2));
        w_b := !w_b + (inv_i_b * (cross2vv (#r_b cp1, p1) +
                                  cross2vv (#r_b cp2, p2)));
        (* Accumulate *)
        #normal_impulse cp1 := vec2x x;
        #normal_impulse cp2 := vec2y x
      end

  in
    if vec2x x >= 0.0 andalso vec2y x >= 0.0
    then
    let
    in
        resubstitute_and_apply x;

        (* Postconditions *)
        (* PERF all this is just assertion when B2_DEBUG_SOLVER *)
        let val dv1 = !v_b :+: cross2sv(!w_b, #r_b cp1) :-: !v_a :-:
                cross2sv(!w_a, #r_a cp1)
            val dv2 = !v_b :+: cross2sv(!w_b, #r_b cp2) :-: !v_a :-:
                cross2sv(!w_a, #r_a cp2)
            (* Compute normal activity *)
            val vn1 = dot2(dv1, normal)
            val vn2 = dot2(dv2, normal)
        in
            if Real.abs(vn1 - #velocity_bias cp1) < ERROR_TOL andalso
               Real.abs(vn2 - #velocity_bias cp2) < ERROR_TOL
            then ()
            else raise BDDContactSolver "assertion failure"
        end
    end
    else
    let
        (* Case 2: vn1 = 0 and x2 = 0
             0 = a11 * x1' + a12 * 0 + b1'
           vn2 = a21 * x1' + a22 * 0 + b2'
        *)
        val x = vec2 (~ (#normal_mass cp1) * vec2x b, 0.0)
        val vn1 = 0.0
        val vn2 = vec2y (mat22col1 (#k c)) * vec2x x + vec2y b
    in
        if vec2x x >= 0.0 andalso vn2 >= 0.0
        then
        let in
          resubstitute_and_apply x;

          (* Postcondtions *)
          (* PERF all assertion *)
          let
            val dv1 : vec2 = !v_b :+: cross2sv(!w_b, #r_b cp1) :-:
                !v_a :-: cross2sv(!w_a, #r_a cp1)
            (* Compute normal velocity *)
            val vn1 = dot2(dv1, normal)
          in
            if Real.abs(vn1 - #velocity_bias cp1) < ERROR_TOL
            then ()
            else raise BDDContactSolver "assertion failure"
          end
        end
        else
        let
            (* Case 3: vn2 = 0 and x1 = 0
               vn1 = a11 * 0 + a12 * x2' + b1' 
                 0 = a21 * 0 + a22 * x2' + b2'
            *)
            val x : vec2 = vec2 (0.0, ~ (#normal_mass cp2) * vec2y b)
            val vn1 = vec2x(mat22col2(#k c)) * vec2y x + vec2x b
            val vn2 = 0.0
        in
            if vec2y x >= 0.0 andalso vn1 >= 0.0
            then
            let in
                resubstitute_and_apply x;
                
                (* Postconditions *)
                (* PERF all assertion *)
                let
                  val dv2 = !v_b :+: cross2sv(!w_b, #r_b cp2) :-:
                      !v_a :-: cross2sv(!w_a, #r_a cp2)
                  val vn2 = dot2(dv2, normal)
                in
                  if Real.abs(vn2 - #velocity_bias cp2) < ERROR_TOL
                  then ()
                  else raise BDDContactSolver "assertion failure"
                end
            end
            else
            let
              (* Case 4: x1 = 0 and x2 = 0
                 vn1 = b1
                 vn2 = b2 *)
              val x = vec2 (0.0, 0.0)
              val vn1 = vec2x b
              val vn2 = vec2y b
            in
              if vn1 >= 0.0 andalso vn2 >= 0.0
              then resubstitute_and_apply x
              else 
                  (* No solution; give up. This is hit sometimes,
                     but it doesn't seem to matter. *)
                  ()
            end
        end
    end
  end

  (* Port note: Body of loop in SolveVelocityConstraints. *)
  fun solve_one_velocity_constraint 
      (c as { body_a, body_b, normal, friction, point_count, ... } 
       : ('b, 'f, 'j) constraint) : unit =
  let
      val () = print "Solving a velocity constraint.\n"

      val w_a : real ref = ref (D.B.get_angular_velocity body_a)
      val w_b : real ref = ref (D.B.get_angular_velocity body_b)
      val v_a : vec2 ref = ref (D.B.get_linear_velocity body_a)
      val v_b : vec2 ref = ref (D.B.get_linear_velocity body_b)
      val inv_mass_a : real = D.B.get_inv_mass body_a
      val inv_mass_b : real = D.B.get_inv_mass body_b
      val inv_i_a : real = D.B.get_inv_i body_a
      val inv_i_b : real = D.B.get_inv_i body_b
      val tangent : vec2 = cross2vs (normal, 1.0)

      (* PERF assert *)
      val () = if point_count = 1 orelse point_count = 2
               then ()
               else raise BDDContactSolver "assertion failed"

      (* Solve tangent constraints. *)
      fun one_tangent_constraint (ccp : constraint_point) : unit =
        let
            (* Relative velocity at contact. *)
            val dv : vec2 = !v_b :+: cross2sv(!w_b, #r_b ccp) :-: !v_a :-:
                cross2sv(!w_a, #r_a ccp)
            (* Compute tangent force *)
            val vt : real = dot2(dv, tangent)
            val lambda : real = #tangent_mass ccp * ~vt
            (* Clamp the accumulated force *)
            val max_friction : real = friction * !(#normal_impulse ccp)
            val new_impulse : real = clampr(!(#tangent_impulse ccp) + lambda,
                                            ~max_friction,
                                            max_friction)
            val lambda = new_impulse - !(#tangent_impulse ccp)
        
            (* Apply contact impulse *)
            val p : vec2 = lambda *: tangent
        in
            v_a := !v_a :-: (inv_mass_a *: p);
            w_a := !w_a - (inv_i_a * cross2vv(#r_a ccp, p));
            v_b := !v_b :+: (inv_mass_b *: p);
            w_b := !w_b + (inv_i_b * cross2vv(#r_b ccp, p));
            #tangent_impulse ccp := new_impulse
        end
  in
      Array.app one_tangent_constraint (#points c);
      (* Solve normal constraints. *)
      (case point_count of
        1 =>
          let
            val ccp = Array.sub(#points c, 0)
            (* Relative velocity at contact *)
            val dv : vec2 = !v_b :+: cross2sv(!w_b, #r_b ccp) :-:
                !v_a :-: cross2sv(!w_a, #r_a ccp)
            (* Compute normal impulse *)
            val vn : real = dot2(dv, normal)
            val lambda : real = ~(#normal_mass ccp) * (vn - #velocity_bias ccp)
            (* Clamp the accumulated impulse *)
            val new_impulse : real = 
                Real.max(!(#normal_impulse ccp) + lambda, 0.0)
            val lambda = new_impulse - !(#normal_impulse ccp)

            (* Apply contact impulse. *)
            val p : vec2 = lambda *: normal
          in
            v_a := !v_a :-: (inv_mass_a *: p);
            w_a := !w_a - (inv_i_a * cross2vv (#r_a ccp, p));
            v_b := !v_b :+: (inv_mass_b *: p);
            w_b := !w_b + (inv_i_b * cross2vv (#r_b ccp, p));
            #normal_impulse ccp := new_impulse
          end
      | 2 =>
          (* Block solver developed in collaboration with Dirk Gregorius 
             (back in 01/07 on Box2D_Lite).
             
             Build the mini LCP for this contact patch
             
             vn = A * x + b, vn >= 0, , vn >= 0, x >= 0 
             and vn_i * x_i = 0 with i = 1..2
             
             A = J * W * JT and J = ( -n, -r1 x n, n, r2 x n )
             b = vn_0 - velocityBias
             
             The system is solved using the "Total enumeration method" 
             (s. Murty). The complementary constraint vn_i * x_i
             implies that we must have in any solution either 
             vn_i = 0 or x_i = 0. So for the 2D contact problem the cases
             vn1 = 0 and vn2 = 0, x1 = 0 and x2 = 0, x1 = 0 
             and vn2 = 0, x2 = 0 and vn1 = 0 need to be tested. 
             The first valid solution that satisfies the problem is chosen.
             
             In order to account of the accumulated impulse 'a' 
             (because of the iterative nature of the solver which only
             requires that the accumulated impulse is clamped and not 
             the incremental impulse) we change the impulse variable (x_i).
             
             Substitute:
             
             x = x' - a
             
             Plug into above equation:
             
             vn = A * x + b
                = A * (x' - a) + b
                = A * x' + b - A * a
                = A * x' + b'
             b' = b - A * a
          *)
          let
            val cp1 = Array.sub(#points c, 0)
            val cp2 = Array.sub(#points c, 1)
            val a : vec2 = vec2(!(#normal_impulse cp1),
                                !(#normal_impulse cp2))

            (* PERF assert *)
            val () = if vec2x a >= 0.0 andalso vec2y a >= 0.0
                     then ()
                     else raise BDDContactSolver "assertion failure"
                      
            (* Relative velocity at contact *)
            val dv1 : vec2 = !v_b :+: cross2sv(!w_b, #r_b cp1) :-:
                !v_a :-: cross2sv(!w_a, #r_a cp1)
            val dv2 : vec2 = !v_b :+: cross2sv(!w_b, #r_b cp2) :-:
                !v_a :-: cross2sv(!w_a, #r_a cp2)

            (* Compute normal velocity *)
            val vn1 : real = dot2(dv1, normal)
            val vn2 : real = dot2(dv2, normal)

            val b : vec2 = vec2(vn1 - #velocity_bias cp1,
                                vn2 - #velocity_bias cp2)
            val b : vec2 = b :-: (#k c +*: a)
          in
            solve_loop (b, c, a, normal, 
                        v_a, inv_mass_a, w_a, inv_i_a,
                        v_b, inv_mass_b, w_b, inv_i_b,
                        cp1, cp2)
          end
      | _ => raise BDDContactSolver "can only solve 1 or 2-point contacts");

      D.B.set_linear_velocity (body_a, !v_a);
      D.B.set_angular_velocity (body_a, !w_a);
      D.B.set_linear_velocity (body_b, !v_b);
      D.B.set_angular_velocity (body_b, !w_b)
  end

  fun solve_velocity_constraints 
      ({ constraints, ... } : ('b, 'f, 'j) contact_solver) : unit =
      Array.app solve_one_velocity_constraint constraints

  fun store_impulses (solver : ('b, 'f, 'j) contact_solver) : unit =
    Array.appi 
    (fn (i, {manifold, point_count, points, ... } : ('b, 'f, 'j) constraint) =>
     for 0 (point_count - 1)
     (fn j =>
      let
          val { local_point, id, ... } =
              Array.sub(#points manifold, j)
      in
          Array.update (#points manifold, j,
                        { local_point = local_point,
                          id = id,
                          normal_impulse =
                            !(#normal_impulse (Array.sub (points, j))),
                          tangent_impulse =
                            !(#tangent_impulse (Array.sub (points, j))) })
      end)) (#constraints solver)

  (* Port note: A class in Box2D; it's just a function that
     returns multiple values.

     Note, this is almost the same function as in toi-solver.
     (Redundancy is present in Box2D too.) *)
  fun contact_solver_manifold (cc : ('b, 'f, 'j) constraint, index : int) :
      { normal : vec2, point : vec2, separation : real } =
    case #typ cc of
        E_Circles =>
          let
              val point_a : vec2 = 
                  D.B.get_world_point (#body_a cc,
                                       #local_point cc)
              val point_b : vec2 = 
                  D.B.get_world_point (#body_b cc,
                                       #local_point
                                       (Array.sub (#points cc, 0)))

              val normal =
                if distance_squared (point_a, point_b) > epsilon * epsilon
                then vec2normalized (point_b :-: point_a)
                else vec2 (1.0, 0.0)
          in
              { normal = normal,
                point = 0.5 *: (point_a :+: point_b),
                separation = dot2(point_b :-: point_a, normal) - #radius cc }
          end
    | E_FaceA =>
          let
              val normal = D.B.get_world_vector (#body_a cc,
                                                 #local_normal cc)
              val plane_point : vec2 =
                  D.B.get_world_point(#body_a cc, #local_point cc)
              val clip_point : vec2 =
                  D.B.get_world_point(#body_b cc, #local_point 
                                      (Array.sub(#points cc,
                                                 index)))
              val separation : real =
                  dot2(clip_point :-: plane_point, normal) - #radius cc
          in
              { normal = normal,
                separation = separation,
                point = clip_point }
          end
    | E_FaceB =>
          let
              val normal = D.B.get_world_vector (#body_b cc,
                                                 #local_normal cc)
              val plane_point : vec2 =
                  D.B.get_world_point(#body_b cc, #local_point cc)
              val clip_point : vec2 =
                  D.B.get_world_point(#body_a cc, #local_point
                                      (Array.sub(#points cc,
                                                 index)))
              val separation : real =
                  dot2(clip_point :-: plane_point, normal) - #radius cc
          in
              (* Ensure normal points from A to B. *)
              { normal = vec2neg normal,
                separation = separation,
                point = clip_point }
          end

  (* Sequential solver.
     
     Port note: This is nearly identical to the code in toi-solver, so
     if you change something here, it probably should be changed there
     too. The duplication comes from Box2D. Obviously it would be
     better to factor out this common routine. *)
  fun solve_position_constraints (solver : ('b, 'f, 'j) contact_solver,
                                  baumgarte : real) : bool =
    let

      (* XXX bug here! *)
      val () = print 
          ("Solving " ^ Int.toString (Array.length (#constraints solver)) ^
           " position constraints.\n")

      val min_separation = ref 0.0
      fun oneconstraint (c : ('b, 'f, 'j) constraint) =
        let
            val body_a = #body_a c
            val body_b = #body_b c

            val inv_mass_a = D.B.get_mass body_a * D.B.get_inv_mass body_a
            val inv_i_a = D.B.get_mass body_a * D.B.get_inv_i body_a
            val inv_mass_b = D.B.get_mass body_b * D.B.get_inv_mass body_b
            val inv_i_b = D.B.get_mass body_b * D.B.get_inv_i body_b

            (* Solve normal constraints. *)
        in
            for 0 (#point_count c - 1)
            (fn j =>
             let
                 val { normal : vec2, point : vec2, separation : real } =
                     contact_solver_manifold (c, j)

                 val r_a : vec2 = point :-: sweepc (D.B.get_sweep body_a)
                 val r_b : vec2 = point :-: sweepc (D.B.get_sweep body_b)

                 (* Track max constraint error. *)
                 val () = if separation < !min_separation
                          then min_separation := separation
                          else ()

                 (* Prevent large corrections and allow slop. *)
                 val capital_c : real = 
                     clampr (baumgarte * (separation + linear_slop),
                             ~max_linear_correction,
                             0.0)
                 (* Compute the effective mass. *)
                 val rn_a : real = cross2vv (r_a, normal)
                 val rn_b : real = cross2vv (r_b, normal)
                 val k : real = inv_mass_a + inv_mass_b + 
                     inv_i_a * rn_a * rn_a +
                     inv_i_b * rn_b * rn_b

                 (* Compute normal impulse. *)
                 val impulse : real = if k > 0.0 then ~ capital_c / k else 0.0
                 val p : vec2 = impulse *: normal

                 fun update_sweep (body, inv_mass, inv_i, r) =
                   let
                     val sweep : sweep = D.B.get_sweep body
                   in
                     sweep_set_a (sweep, sweepa sweep - 
                                  (inv_i * cross2vv (r, p)));
                     sweep_set_c (sweep, sweepc sweep :-: (inv_mass *: p));
                     D.B.synchronize_transform body
                   end
             in
                 update_sweep (body_a, inv_mass_a, inv_i_a, r_a);
                 update_sweep (body_b, inv_mass_b, inv_i_b, r_b)
             end)
        end
    in
      Array.app oneconstraint (#constraints solver);
      (* We can't expect minSpeparation >= -b2_linearSlop because we don't
         push the separation above -b2_linearSlop. *)
      !min_separation >= ~1.5 * linear_slop
    end


  (* Apply the function to every contact, paired with all of its
     impulses. *)
  fun app_contacts ({ contacts, 
                      constraints, 
                      ... } : ('b, 'f, 'j) contact_solver, 
                    f : ('b, 'f, 'j) BDDDynamics.contact * 
                        { normal_impulses : real array,
                          tangent_impulses : real array } -> unit) : unit =
      Vector.appi 
      (fn (i, c : ('b, 'f, 'j) BDDDynamics.contact) =>
       let
           val cc : ('b, 'f, 'j) constraint = Array.sub(constraints, i)
           val normal_impulses = 
               Array.tabulate (#point_count cc,
                               fn j =>
                               !(#normal_impulse (Array.sub(#points cc, j))))
           val tangent_impulses = 
               Array.tabulate (#point_count cc,
                               fn j =>
                               !(#tangent_impulse (Array.sub(#points cc, j))))
       in
           f (c, { normal_impulses = normal_impulses,
                   tangent_impulses = tangent_impulses })
       end) contacts

end
