/**
 * Implements a bounding-volume hierarchy (BVH) acceleration structure for raytracing.
 *
 * Note: the implementation of a bounding-volume hierarchy in this file is taken from
 * PBRT, 3rd edition, section 4.3 (starting around page 256).
 */
 open Vec.Ops;

/** Intermediary data per-prim-component data structure. */
type component_info_t = {
    prim: Prim.t,
    component_index: int,
    bbox: Bbox.t,
    centroid: Vec.t,
};

/** Intermediary data structure for SAH partition buckets. */
type bucket_info_t = {
    count: int,
    bbox: Bbox.t,
};

/** Contains node data used during BVH construction. */
type build_node_t = {
    bbox: Bbox.t,
    child_1: int,
    child_2: int,
    split_axis: int,
    first_component_offset: int,
    num_components: int,
};

/** Contains node data after the BVH is finalized and ready for use. */
type linear_node_t = {
    bbox: Bbox.t,
    offset: int,
    num_components: int,
    axis: int,
};

/** Encapsulates entire bounding-volume hierarchy. */
type t = {
    prims: array(Prim.t),
    components: array((Prim.t, int)),
    nodes: array(linear_node_t),
    light_indices: array(int),
};

/** Intersection test result. */
type intersection_t =
    /** Ray hit a surface at given distance; surface properties; prim. */
    | Hit(float, SurfaceProperties.t, Prim.t)
    /** Ray didn't hit anything and continued into the abyss forever. */
    | NoHit;

let build = (prims: array(Prim.t)) => {
    /* Initialize component_info_t by scanning all prims for components. */
    let component_info = ref([]);
    Array.iter((prim: Prim.t) => {
        for (component_index in (prim#num_components - 1) downto 0) {
            let bbox = prim#bbox_world(component_index);
            let centroid = (bbox.min *^. 0.5) +^ (bbox.max *^. 0.5);
            let info = {
                prim: prim,
                component_index: component_index,
                bbox: bbox,
                centroid: centroid
            };
            component_info := [info, ...component_info^];
        };
    }, prims);
    let component_info_arr = Array.of_list(component_info^);

    {
        prims: prims,
        components: [||],
        nodes: [||],
        light_indices: [||]
    }
};

/** Naive intersection for debugging purposes. */
let intersect_naive = (bvh: t, ray: Ray.t) => {
    let closest_dist = ref(max_float);
    let closest = ref(NoHit);
    Array.iter(prim => {
        for (comp_index in 0 to prim#num_components) {
            let (dist, surface_props) = prim#intersect_world(ray, comp_index);
            if (dist != 0.0 && dist < closest_dist^) {
                closest := Hit(dist, surface_props, prim);
                closest_dist := dist;
            }
        };
    }, bvh.prims);

    closest^
};

let intersect = (bvh: t, initial_ray: Ray.t) => {
    intersect_naive(bvh, initial_ray)
};
