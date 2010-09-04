(* Copyright 2010 Tom Murphy VII and Erin Catto. See COPYING for details. *)

(* Corresponding to dynamics/b2body.cpp, dynamics/b2fixture.cpp, and the
   implementation portions of their headers. *)
functor BDDWorld(Arg : BDDWORLD_ARG) :>
  BDDWORLD where type fixture_data = Arg.fixture_data 
             and type body_data = Arg.body_data =
struct
  open Arg
  open BDDSettings
  open BDDTypes
  open BDDMath
  open BDDOps
  infix 6 :+: :-: %-% %+% +++
  infix 7 *: *% +*: +*+ #*% @*:

  exception BDDWorld of string

  type filter = Word32.word * int

  type body = unit

  type world = unit

  datatype fixturecell =
      F of { aabb : aabb,
             density : real,
             next : fixturecell ref option,
             body : body option,
             shape : BDDShape.shape,
             friction : real,
             restitution : real,
             (* Broad phase proxy, where the user data is
                this fixture. *)
             proxy : fixturecell ref BDDBroadPhase.proxy,
             filter : filter,
             sensor : bool,
             data : fixture_data }

  type fixture = fixturecell ref

  (* Internal, fixtures *)      
  structure F =
  struct

    fun get_aabb (ref (F{ aabb, ... })) = aabb
    fun get_density (ref (F{ density, ... })) = density
    fun get_next (ref (F{ next, ... })) = next
    fun get_body (ref (F{ body, ... })) = body
    fun get_shape (ref (F{ shape, ... })) = shape
    fun get_friction (ref (F{ friction, ... })) = friction
    fun get_restitution (ref (F{ restitution, ... })) = restitution
    fun get_filter (ref (F{ filter, ... })) = filter
    fun get_sensor (ref (F{ sensor, ... })) = sensor
    fun get_data (ref (F{ data, ... })) = data

    (* This is annoying, but the least error prone way to simulate what's
       happening in the C++ code. *)
    fun set_aabb (r as ref (F { aabb = _, density, next, body, shape, friction,
                                restitution, proxy, filter, sensor, data }), aabb) =
        r := F { aabb = aabb, density = density, next = next, body = body,
                 shape = shape, friction = friction, restitution = restitution,
                 proxy = proxy, filter = filter, sensor = sensor, data = data }

    fun set_density (r as ref (F { aabb, density = _, next, body, shape, friction,
                                   restitution, proxy, filter, sensor, data }), density) =
        (* XXX Box2D has check on range *)
        r := F { aabb = aabb, density = density, next = next, body = body,
                 shape = shape, friction = friction, restitution = restitution,
                 proxy = proxy, filter = filter, sensor = sensor, data = data }

    fun set_next (r as ref (F { aabb, density, next = _, body, shape, friction,
                                restitution, proxy, filter, sensor, data }), next) =
        r := F { aabb = aabb, density = density, next = next, body = body,
                 shape = shape, friction = friction, restitution = restitution,
                 proxy = proxy, filter = filter, sensor = sensor, data = data }

    fun set_body (r as ref (F { aabb, density, next, body = _, shape, friction,
                                restitution, proxy, filter, sensor, data }), body) =
        r := F { aabb = aabb, density = density, next = next, body = body,
                 shape = shape, friction = friction, restitution = restitution,
                 proxy = proxy, filter = filter, sensor = sensor, data = data }

    fun set_shape (r as ref (F { aabb, density, next, body, shape = _, friction,
                                 restitution, proxy, filter, sensor, data }), shape) =
        r := F { aabb = aabb, density = density, next = next, body = body,
                 shape = shape, friction = friction, restitution = restitution,
                 proxy = proxy, filter = filter, sensor = sensor, data = data }

    fun set_friction (r as ref (F { aabb, density, next, body, shape, friction = _,
                                restitution, proxy, filter, sensor, data }), friction) =
        r := F { aabb = aabb, density = density, next = next, body = body,
                 shape = shape, friction = friction, restitution = restitution,
                 proxy = proxy, filter = filter, sensor = sensor, data = data }

    fun set_restitution (r as ref (F { aabb, density, next, body, shape, friction,
                                       restitution = _, proxy, filter, sensor, data }), 
                         restitution) =
        r := F { aabb = aabb, density = density, next = next, body = body,
                 shape = shape, friction = friction, restitution = restitution,
                 proxy = proxy, filter = filter, sensor = sensor, data = data }

    fun set_proxy (r as ref (F { aabb, density, next, body, shape, friction,
                                 restitution, proxy = _, filter, sensor, data }), proxy) =
        r := F { aabb = aabb, density = density, next = next, body = body,
                 shape = shape, friction = friction, restitution = restitution,
                 proxy = proxy, filter = filter, sensor = sensor, data = data }

    (* XXX exposed function needs to do SetFilterData below. *)
    fun set_filter (r as ref (F { aabb, density, next, body, shape, friction,
                                restitution, proxy, filter = _, sensor, data }), filter) =
        r := F { aabb = aabb, density = density, next = next, body = body,
                 shape = shape, friction = friction, restitution = restitution,
                 proxy = proxy, filter = filter, sensor = sensor, data = data }

    fun set_sensor (r as ref (F { aabb, density, next, body, shape, friction,
                                  restitution, proxy, filter, sensor = _, data }), sensor) =
        r := F { aabb = aabb, density = density, next = next, body = body,
                 shape = shape, friction = friction, restitution = restitution,
                 proxy = proxy, filter = filter, sensor = sensor, data = data }

    fun set_data (r as ref (F { aabb, density, next, body, shape, friction,
                                restitution, proxy, filter, sensor, data = _ }), data) =
        r := F { aabb = aabb, density = density, next = next, body = body,
                 shape = shape, friction = friction, restitution = restitution,
                 proxy = proxy, filter = filter, sensor = sensor, data = data }
  end

  (* Internal, bodies *)
  structure B =
  struct
     datatype body_type =
        Static
      | Kinematic
      | Dynamic

    fun get_transform (b : body) : BDDMath.transform = raise BDDWorld "unimplemented"
  end

 (*
void b2Fixture::CreateProxy(b2BroadPhase* broadPhase, const b2Transform& xf)
{
        b2Assert(m_proxyId == b2BroadPhase::e_nullProxy);

        // Create proxy in the broad-phase.
        m_shape->ComputeAABB(&m_aabb, xf);
        m_proxyId = broadPhase->CreateProxy(m_aabb, this);
}

void b2Fixture::DestroyProxy(b2BroadPhase* broadPhase)
{
        if (m_proxyId == b2BroadPhase::e_nullProxy)
        {
                return;
        }

        // Destroy proxy in the broad-phase.
        broadPhase->DestroyProxy(m_proxyId);
        m_proxyId = b2BroadPhase::e_nullProxy;
}

void b2Fixture::Synchronize(b2BroadPhase* broadPhase, const b2Transform& transform1, const b2Transform& transform2)
{
        if (m_proxyId == b2BroadPhase::e_nullProxy)
        {       
                return;
        }

        // Compute an AABB that covers the swept shape (may miss some rotation effect).
        b2AABB aabb1, aabb2;
        m_shape->ComputeAABB(&aabb1, transform1);
        m_shape->ComputeAABB(&aabb2, transform2);
        
        m_aabb.Combine(aabb1, aabb2);

        b2Vec2 displacement = transform2.position - transform1.position;

        broadPhase->MoveProxy(m_proxyId, m_aabb, displacement);
}


void b2Fixture::SetFilterData(const b2Filter& filter)
{
        m_filter = filter;

        if (m_body == NULL)
        {
                return;
        }

        // Flag associated contacts for filtering.
        b2ContactEdge* edge = m_body->GetContactList();
        while (edge)
        {
                b2Contact* contact = edge->contact;
                b2Fixture* fixtureA = contact->GetFixtureA();
                b2Fixture* fixtureB = contact->GetFixtureB();
                if (fixtureA == this || fixtureB == this)
                {
                        contact->FlagForFiltering();
                }

                edge = edge->next;
        }
}

 *)

  (* Exported *)
  structure Body =
  struct
    open B

    fun create_fixture (b : body,
                        { shape : BDDShape.shape,
                          data : body_data,
                          friction : real,
                          restitution : real,
                          density : real,
                          is_sensor : bool,
                          filter : filter }) : fixture =
        raise BDDWorld "unimplemented"

  end

  (* Exported *)
  structure Fixture =
  struct
    type filter = filter
    open F

    fun filter_list { categories : int list,
                      mask : int list,
                      group_index : int } : filter =
        raise BDDWorld "unimplemented"

    fun filter_mask { category_bits : Word16.word,
                      mask_bits : Word16.word,
                      group_index : int } : filter =
        raise BDDWorld "unimplemented"

    fun fixture_transform f =
        case get_body f of
            NONE => raise BDDWorld "fixture is not attached to a body."
          | SOME b => B.get_transform b

    fun test_point (f, p : vec2) : bool =
        BDDShape.test_point (get_shape f, fixture_transform f, p)
        
    fun ray_cast (f, input) =
        BDDShape.ray_cast (get_shape f, fixture_transform f, input)

    fun get_mass_data f =
        BDDShape.compute_mass (get_shape f, get_density f)

    val is_sensor = get_sensor
    val shape = get_shape

  end

  structure World =
  struct

    fun create_body (world : world,
        { (* The body type: static, kinematic, or dynamic.
             Note: if a dynamic body would have zero mass, the mass is set to one. *)
          typ : Body.body_type,

          (* The initial world position of the body. Avoid creating bodies at
             the origin since this can lead to many overlapping shapes. *)
          position : BDDMath.vec2,

          (* The world angle of the body in radians. *)
          angle : real,

          (* The linear velocity of the body's origin in world co-ordinates. *)
          linear_velocity : BDDMath.vec2,

          (* The angular velocity of the body. *)
          angular_velocity : real,

          (* Linear damping is use to reduce the linear velocity. The damping
             parameter can be larger than 1.0f but the damping effect
             becomes sensitive to the time step when the damping parameter
             is large.
             Default: 0.0 *)
          linear_damping : real,

          (* Angular damping is use to reduce the angular velocity. The damping
             parameter can be larger than 1.0f but the damping effect
             becomes sensitive to the time step when the damping
             parameter is large.
             Default: 0.0 *)
          angular_damping : real,

          (* Set this flag to false if this body should never fall asleep. Note
             that this increases CPU usage. *)
          allow_sleep : bool,

          (* Is this body initially awake or sleeping? *)
          awake : bool,

          (* Should this body be prevented from rotating? Useful for characters. *)
          fixed_rotation : bool,

          (* Is this a fast moving body that should be prevented from tunneling through
             other moving bodies? Note that all bodies are prevented from tunneling through
             kinematic and static bodies. This setting is only considered on dynamic bodies.
             You should use this flag sparingly since it increases processing time. *)
          bullet : bool,

          (* Does this body start out active? *)
          active : bool,

          (* Use this to store application specific body data. *)
          data : body_data,

          (* Experimental: scales the inertia tensor.
             Default: 1.0 *)
          inertia_scale : real }) : body =
        raise BDDWorld "unimplemented"
  
  end

end
