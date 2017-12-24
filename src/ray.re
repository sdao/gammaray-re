open Vec.Ops;

/**
 * A ray in 3-d space with origin and direction.
 * The direction need not be unit-length, but the length will affect parametric computations like
 * at(...).
 */
type t = {
    origin: Vec.t,
    dir: Vec.t
};

/** Caches some data used repeatedly during ray intersections. */
type intersection_data_t = {
    inv_dir: Vec.t,
    dir_is_neg: array(bool)
};

/* The distance to push the origin of each new ray along the normal.
   XXX: PBRT says that we should be reprojecting and computing an error bound instead.
   XXX: Seems like we need around 1e-3 for floats and 1e-6 for doubles if hardcoding. */
let _ray_push_dist = Vec.from_scalar(1.0e-3);

let zero = {origin: Vec.zero, dir: Vec.zero};

/** Creates a ray with given origin and direction. */
let create = (origin: Vec.t, direction: Vec.t) => {origin: origin, dir: direction};

/** Returns the parametric position along the ray at the given parameter. */
let at = (a: t, k: float) => a.origin +^ (a.dir *^ Vec.from_scalar(k));

/** Pre-computes some data used to accelerate intersection computations. */
let compute_intersection_data = (a: t) => {
    {
        inv_dir: Vec.xyz(
            1.0 /. a.dir.x,
            1.0 /. a.dir.y,
            1.0 /. a.dir.z),
        dir_is_neg: [|
            a.dir.x < 0.0,
            a.dir.y < 0.0,
            a.dir.z < 0.0|]
    }
};

/** Slightly move the origin of the ray in its direction to avoid self-intersection. */
let nudge = (a: t) => {
    {origin: a.origin +^ (a.dir *^ _ray_push_dist), dir: a.dir}
};
