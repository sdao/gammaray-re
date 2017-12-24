type t = {
    mat: Mat.t,
    inv_mat: Mat.t
};

let identity = {
    mat: Mat.identity,
    inv_mat: Mat.identity
};

let create = (mat: Mat.t) => {
    mat: mat,
    inv_mat: Mat.invert(mat)
};

/* XXX: unimplemented */
let transform_vec = (xform: t, v: Vec.t) => v;
let transform_dir = (xform: t, v: Vec.t) => v;