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
    mutable count: int,
    mutable bbox: Bbox.t,
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

let _new_leaf = (
    first_component_offset: int,
    num_components: int,
    bbox: Bbox.t) =>
{
    {
        bbox: bbox,
        child_1: -1,
        child_2: -1,
        split_axis: 0,
        first_component_offset: first_component_offset,
        num_components: num_components
    }
};

let _new_interior = (axis: int, c0: (build_node_t, int), c1: (build_node_t, int)) => {
    {
        bbox: Bbox.combine(fst(c0).bbox, fst(c1).bbox),
        child_1: snd(c0),
        child_2: snd(c1),
        split_axis: axis,
        first_component_offset: 0,
        num_components: 0
    }
};

let swap = (arr: array('a), i: int, j: int) => {
    let tmp = arr[i];
    arr[i] = arr[j];
    arr[j] = tmp;
};

/**
 * Rearranges the items in the slice such that all items for which the predicate is true come
 * before all elements for which the predicate is false. Returns the index of the first item for
 * which the predicate is false.
 */
let partition = (arr: array('a), arr_min: int, arr_max: int, predicate: ('a) => bool) => {
    let cursor = ref(arr_min);
    for (i in arr_min to arr_max - 1) {
        if (predicate(arr[i])) {
            swap(arr, i, cursor^);
            cursor := cursor^ + 1;
        };
    };

    cursor^
};

/**
 * Rearranges the items in the slice such that, in the new configuration, all items before
 * the nth item are less than the nth item, and all items after the nth item are greater than or
 * equal to the nth item. The item in the nth position will be the nth smallest item in the
 * slice.
 * XXX lol this needs to be refactored to not do all this weird slice stuff
 * that I blindly ported from my Rust code.
 */
let rec nth_element = (arr: array('a), arr_min: int, arr_max: int, nth: int,
        less_than: ('a, 'a) => bool) =>
{
    let slice_len = arr_max - arr_min;
    if (slice_len >= 2) {
        let pivot = slice_len / 2;
        swap(arr, arr_min + pivot, arr_max - 1);

        /* Partition the slice so that all items before i are less than slice[i], and all items
         * after i are greater than or equal to slice[i]. */
        let pivot_val = arr[arr_max - 1];
        let final_pivot_index = partition(arr, arr_min, arr_max - 1,
                (x) => less_than(x, pivot_val));
        swap(arr, arr_max - 1, final_pivot_index);

        /* Choose which side of the slice to recurse into. (If the pivot position is nth, then
         * done!) */
        let rel_pivot_index = final_pivot_index - arr_min;
        if (nth < rel_pivot_index) {
            nth_element(arr, arr_min, arr_min + rel_pivot_index - 1, nth, less_than);
        }
        else if (nth > rel_pivot_index) {
            nth_element(arr, arr_min + rel_pivot_index + 1, arr_max, nth - rel_pivot_index - 1,
                    less_than);
        };
    };
};

let rec _recurse_build = (
    build_nodes: ref(list(build_node_t)),
    build_nodes_len: ref(int),
    component_infos: array(component_info_t),
    component_infos_min: int,
    component_infos_max: int,
    ordered_components: array((Prim.t, int)),
    ordered_components_len: ref(int)) =>
{
    let bbox = ref(Bbox.empty);
    for (i in component_infos_min to component_infos_max - 1) {
        bbox := Bbox.combine(bbox^, component_infos[i].bbox);
    };
    
    let num_components = component_infos_max - component_infos_min;
    if (num_components == 1) {
        /* Create a leaf node with one component. */
        let ci = component_infos[component_infos_min];
        let first_component_offset = ordered_components_len^;
        ordered_components[ordered_components_len^] = (ci.prim, ci.component_index);
        ordered_components_len := ordered_components_len^ + 1;
        
        let leaf = _new_leaf(first_component_offset, 1, bbox^);
        build_nodes := [leaf, ...build_nodes^];
        build_nodes_len := build_nodes_len^ + 1;
        (List.hd(build_nodes^), build_nodes_len^ - 1)
    }
    else {
        /* Partition the current node into two child subtrees. */
        let centroid_bbox = ref(Bbox.empty);
        for (i in component_infos_min to component_infos_max - 1) {
            centroid_bbox := Bbox.union(centroid_bbox^, component_infos[i].centroid);
        };
        let dim = Bbox.maximum_extent(centroid_bbox^);
        if (Vec.get(centroid_bbox^.min, dim) == Vec.get(centroid_bbox^.max, dim)) {
            /* Cannot partition properly (components overlay one another).
             * Create a leaf node with multiple components. */
            let first_component_offset = ordered_components_len^;
            for (i in component_infos_min to component_infos_max - 1) {
                let ci = component_infos[i];
                ordered_components[ordered_components_len^] = (ci.prim, ci.component_index);
                ordered_components_len := ordered_components_len^ + 1;
            };

            let leaf = _new_leaf(first_component_offset, num_components, bbox^);
            build_nodes := [leaf, ...build_nodes^];
            build_nodes_len := build_nodes_len^ + 1;
            (List.hd(build_nodes^), build_nodes_len^ - 1)
        }
        else {
            let mid = if (num_components <= 4) {
                /* Partition into equally-sized subsets if too small to use SAH. */
                let mid = num_components / 2;
                nth_element(component_infos, component_infos_min, component_infos_max, mid,
                    (lhs, rhs) => {
                        Vec.get(lhs.centroid, dim) < Vec.get(rhs.centroid, dim)
                    });
                mid
            }
            else {
                /* Use the surface-area heuristic. */
                let num_buckets = 12;
                let buckets = Array.make(num_buckets, {
                    count: 0, bbox: Bbox.empty
                });

                /* Initialize bucket info for SAH partition buckets. */
                for (i in component_infos_min to component_infos_max - 1) {
                    let ci = component_infos[i];
                    let rel = Bbox.relative_offset(centroid_bbox^, ci.centroid);
                    let b = Math.clamp(
                            int_of_float(float_of_int(num_buckets) *.  Vec.get(rel, dim)),
                            0, num_buckets - 1);
                    buckets[b].count = buckets[b].count + 1;
                    buckets[b].bbox = Bbox.combine(buckets[b].bbox, ci.bbox);
                };

                /* Compute costs for splitting after each bucket. */
                let cost = Array.make(num_buckets - 1, 0.0);
                for (i in 0 to num_buckets - 2) {
                    /* Left side of split. */
                    let b0 = ref(Bbox.empty);
                    let count0 = ref(0);
                    for (j in 0 to i) {
                        b0 := Bbox.combine(b0^, buckets[j].bbox);
                        count0 := count0^ + buckets[j].count;
                    };

                    /* Right side of split. */
                    let b1 = ref(Bbox.empty);
                    let count1 = ref(0);
                    for (j in (i + 1) to (num_buckets - 1)) {
                        b1 := Bbox.combine(b1^, buckets[j].bbox);
                        count1 := count1^ + buckets[j].count;
                    };

                    cost[i] = 1.0 +. (float_of_int(count0^) *. Bbox.surface_area(b0^)
                            +. float_of_int(count1^) *. Bbox.surface_area(b1^)) /.
                            Bbox.surface_area(bbox^);
                };

                /* Find bucket to split at that minimizes SAH metric. */
                let min_cost = ref(cost[0]);
                let min_cost_split_bucket = ref(0);
                for (i in 1 to num_buckets - 2) {
                    if (cost[i] < min_cost^) {
                        min_cost := cost[i];
                        min_cost_split_bucket := i;
                    };
                };
            
                /* Either create leaf or split primitives at selected SAH bucket.
                 * (Leaf might be cheaper.) */
                let leaf_cost = float_of_int(num_components);

                let max_components_per_node = 255;
                let mid = if (num_components > max_components_per_node || min_cost^ < leaf_cost) {
                    /* Interior node. */
                    let mid = partition(component_infos, component_infos_min, component_infos_max, 
                        (ci) => {
                            let rel = Bbox.relative_offset(centroid_bbox^, ci.centroid);
                            let b = Math.clamp(
                                    int_of_float(float_of_int(num_buckets) *.  Vec.get(rel, dim)),
                                    0, num_buckets - 1);
                            b <= min_cost_split_bucket^
                        });
                    mid - component_infos_min /* needs to be relative */
                }
                else {
                    /* Leaf node. */
                    let mid = num_components / 2;
                    nth_element(component_infos, component_infos_min, component_infos_max, mid,
                        (lhs, rhs) => {
                            Vec.get(lhs.centroid, dim) < Vec.get(rhs.centroid, dim)
                        });
                    mid
                };

                mid
            };

            assert(mid > 0);
            assert(mid < num_components);
            let c0 = _recurse_build(build_nodes, build_nodes_len, component_infos,
                    component_infos_min, component_infos_min + mid,
                    ordered_components, ordered_components_len);
            let c1 = _recurse_build(build_nodes, build_nodes_len, component_infos,
                    component_infos_min + mid, component_infos_max,
                    ordered_components, ordered_components_len);
            let interior = _new_interior(dim, c0, c1);
            build_nodes := [interior, ...build_nodes^];
            build_nodes_len := build_nodes_len^ + 1;
            (List.hd(build_nodes^), build_nodes_len^ - 1)
        }
    }
};

let rec _flatten_tree = (
    build_nodes: array(build_node_t),
    linear_nodes: array(linear_node_t),
    linear_nodes_len: ref(int),
    root: int) =>
{
    let build_node = build_nodes[root];
    let idx = linear_nodes_len^;
    linear_nodes_len := linear_nodes_len^ + 1;

    if (build_node.num_components > 0) {
        /* Leaf node */
        linear_nodes[idx] = {
            bbox: build_node.bbox,
            offset: build_node.first_component_offset,
            num_components: build_node.num_components,
            axis: 0
        };
    }
    else {
        /* Interior node. */
        ignore(_flatten_tree(build_nodes, linear_nodes, linear_nodes_len, build_node.child_1));
        let second_child_offset = _flatten_tree(build_nodes, linear_nodes, linear_nodes_len, build_node.child_2);

        linear_nodes[idx] = {
            bbox: build_node.bbox,
            offset: second_child_offset,
            num_components: 0,
            axis: build_node.split_axis
        };
    };

    idx
};

let build = (prims: array(Prim.t)) => {
    /* Initialize component_info_t by scanning all prims for components. */
    let component_infos = ref([]);
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
            component_infos := [info, ...component_infos^];
        };
    }, prims);
    let component_info_arr = Array.of_list(List.rev(component_infos^));

    /* Build BVH tree for components from the component_info_t.
     * This will also create a lookup of all components. */
    let build_nodes_len = ref(0);
    let build_nodes = ref([]);
    let dummy_prim: Prim.t = {
        pub num_components = 0;
        pub display_color = Vec.zero;
        pub material = Material.empty;
        pub bbox_world = (_) => Bbox.empty;
        pub intersect_world = (_, _) => (0.0, SurfaceProperties.empty);
        pub sample_world = (_) => (Vec.zero, SurfaceProperties.empty, 0.0);
        pri _suppress_warning = this;
    };
    let ordered_components_len = ref(0);
    let ordered_components = Array.make(Array.length(component_info_arr), (dummy_prim, 0));
    let (_, root) = _recurse_build(build_nodes, build_nodes_len, component_info_arr, 0,
            Array.length(component_info_arr), ordered_components, ordered_components_len);
    let build_node_arr = Array.of_list(List.rev(build_nodes^));

    /* Compute representation of depth-first traversal of BVH tree. */
    let dummy_linear_node = {
        bbox: Bbox.empty,
        offset: 0,
        num_components: 0,
        axis: 0
    };
    let linear_nodes = Array.make(Array.length(build_node_arr), dummy_linear_node);
    let linear_nodes_len = ref(0);
    ignore(_flatten_tree(build_node_arr, linear_nodes, linear_nodes_len, root));

    {
        prims: prims,
        components: ordered_components,
        nodes: linear_nodes,
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

let intersect_bvh = (bvh: t, ray: Ray.t) => {
    let closest_dist = ref(max_float);
    let closest = ref(NoHit);
    let isect_data = Ray.compute_intersection_data(ray);

    /* Follow ray through BVH nodes to component intersections. */
    let current_node_index = ref(0);
    let nodes_to_visit = ref([]);

    let result = ref(None);
    while (result^ == None) {
        let node = bvh.nodes[current_node_index^];

        /* Check ray against BVH node. */
        if (Bbox.intersect(node.bbox, ray, isect_data, closest_dist^)) {
            if (node.num_components > 0) {
                /* Intersect ray with components in leaf. */
                for (i in node.offset to (node.offset + node.num_components - 1)) {
                    let (prim, component_index) = bvh.components[i];
                    let (dist, surface_props) = prim#intersect_world(ray, component_index);
                    if (dist != 0.0 && dist < closest_dist^) {
                        closest := Hit(dist, surface_props, prim);
                        closest_dist := dist;
                    };
                };
                switch nodes_to_visit^ {
                    | [] => result := Some(closest^)
                    | [i, ...rest] => {
                        current_node_index := i;
                        nodes_to_visit := rest;
                    }
                };
            }
            else {
                /* Put far BVH node on nodes_to_visit stack, advance to near node. */
                if (isect_data.dir_is_neg[node.axis]) {
                    nodes_to_visit := [current_node_index^ + 1, ...nodes_to_visit^];
                    current_node_index := node.offset;
                }
                else {
                    nodes_to_visit := [node.offset, ...nodes_to_visit^];
                    current_node_index := current_node_index^ + 1;
                };
            }
        }
        else {
            switch nodes_to_visit^ {
                | [] => result := Some(closest^)
                | [i, ...rest] => {
                    current_node_index := i;
                    nodes_to_visit := rest;
                }
            };
        }
    };

    result
};

let intersect = (bvh: t, initial_ray: Ray.t) => {
    intersect_naive(bvh, initial_ray)
};
