open Vec.Ops;

type t = {
    min: Vec.t,
    max: Vec.t,
};

let empty = {
    min: Vec.from_scalar(max_float),
    max: Vec.from_scalar(min_float),
};

let is_empty = (a: t) => {
    a.min.x >= a.max.x || a.min.y >= a.max.y || a.min.z >= a.max.z
};

let union_with = (a: t, v: Vec.t) => {
    {
        min: Vec.xyz(min(a.min.x, v.x), min(a.min.y, v.y), min(a.min.z, v.z)),
        max: Vec.xyz(max(a.max.x, v.x), max(a.max.y, v.y), max(a.max.z, v.z))
    }
};

let combine_with = (a: t, b: t) => {
    {
        min: Vec.xyz(min(a.min.x, b.min.x), min(a.min.y, b.min.y), min(a.min.z, b.min.z)),
        max: Vec.xyz(max(a.max.x, b.max.x), max(a.max.y, b.max.y), max(a.max.z, b.max.z))
    }
};

let diagonal = (a: t) => a.max -^ a.min;

/** Returns 0 if the maximum extent is along the X-axis, 1 if Y-axis, 2 if Z-axis. */
let maximum_extent = (a: t) => {
    let diagonal = diagonal(a);
    if (diagonal.x >= diagonal.y && diagonal.y >= diagonal.z) {
        0
    }
    else if (diagonal.y >= diagonal.z) {
        1
    }
    else {
        2
    }
};

/**
  * Returns the position of v relative to the corners of the bounding box, where (0, 0, 0)
  * represents the min corner and (1, 1, 1) represents the max corner.
  */
let relative_offset = (a: t, v: Vec.t) => {
    let ofs = v -^ a.min;
    let diag = diagonal(a);
    ofs /^ diag
};

let surface_area = (a: t) => {
    let d = diagonal(a);
    d.x *. d.y *. d.z
};

/** Convenience for switching between min and max bounds. */
let get_bound = (a: t, getx_max: bool) => {
    if (getx_max) {
        a.max
    }
    else {
        a.min
    }
};

let intersect = (a: t, ray: Ray.t, data: Ray.intersection_data_t, max_dist: float) => {
    /* Check for ray intersection against x and y slabs. */
    let tx_min = (get_bound(a,  data.dir_is_neg[0]).x -. ray.origin.x) *. data.inv_dir.x;
    let tx_max = (get_bound(a, !data.dir_is_neg[0]).x -. ray.origin.x) *. data.inv_dir.x;
    let ty_min = (get_bound(a,  data.dir_is_neg[1]).y -. ray.origin.y) *. data.inv_dir.y;
    let ty_max = (get_bound(a, !data.dir_is_neg[1]).y -. ray.origin.y) *. data.inv_dir.y;

    /* Use PBRT gamma function to make more numerically stable. */
    let tx_max_g = tx_max *. (1.0 +. 2.0 *. Math.gamma(3.0));
    let ty_max_g = ty_max *. (1.0 +. 2.0 *. Math.gamma(3.0));
    if (tx_min > ty_max_g || ty_min > tx_max_g) {
        false
    }
    else {
        let t_min = if (ty_min > tx_min) {
            ty_min;
        }
        else {
            tx_min
        };
        let txy_max = if (ty_max_g < tx_max_g) {
            ty_max_g;
        }
        else {
            tx_max_g
        };

        /* Check for ray intersection against z slab. */
        let tz_min = (get_bound(a,  data.dir_is_neg[2]).z -. ray.origin.z) *. data.inv_dir.z;
        let tz_max = (get_bound(a, !data.dir_is_neg[2]).z -. ray.origin.z) *. data.inv_dir.z;

        /* Use PBRT gamma function to make more numerically stable. */
        let tz_max_g = tz_max *. (1.0 +. 2.0 *. Math.gamma(3.0));
        if (t_min > tz_max_g || tz_min > txy_max) {
            false
        }
        else {
            let t_max = if (tz_max_g < txy_max) {
                tz_max_g
            }
            else {
                txy_max
            };

            t_min < max_dist && t_max > 0.0
        }
    }
};
