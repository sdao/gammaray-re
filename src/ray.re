open Vec.Ops;

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

let create = (origin: Vec.t, direction: Vec.t) => {origin: origin, dir: direction};

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

let nudge = (a: t) => {
    {origin: a.origin +^ (a.dir *^ _ray_push_dist), dir: a.dir}
};
