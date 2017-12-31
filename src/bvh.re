type t = {
    prims: array(Prim.t),
};

type intersection_t =
    | Hit(float, SurfaceProperties.t, int)
    | NoHit;

let build = (prims: array(Prim.t)) => {
    {prims: prims}
};

/** Naive intersection for debugging purposes. */
let intersect_naive = (bvh: t, ray: Ray.t) => {
    let closest_dist = ref(max_float);
    let closest = ref(NoHit);
    for (prim_index in 0 to Array.length(bvh.prims) - 1) {
        let prim = bvh.prims[prim_index];
        for (comp_index in 0 to prim#num_components) {
            let (dist, surface_props) = prim#intersect_world(ray, comp_index);
            if (dist != 0.0 && dist < closest_dist^) {
                closest := Hit(dist, surface_props, prim_index);
                closest_dist := dist;
            }
        };
    };

    closest^
};

let intersect = (bvh: t, initial_ray: Ray.t) => {
    intersect_naive(bvh, initial_ray)
};

let prim = (bvh: t, prim_index: int) => {
    bvh.prims[prim_index]
};
