type t = {
    origin: Vec.t,
    dir: Vec.t
};

/** Caches some data used repeatedly during ray intersections. */
type intersection_data_t = {
    inv_dir: Vec.t,
    dir_is_neg: array(bool)
};
