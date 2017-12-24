/**
 * Represents a geometric transformation in 3-d space, backed by a matrix and a cache of its
 * inverse.
 */
type t = {
    mat: Mat.t,
    inv_mat: Mat.t
};

/** The identity transformation, which performs no actual transformation. */
let identity = {
    mat: Mat.identity,
    inv_mat: Mat.identity
};

/** Creates an Xform from a backing matrix. */
let create = (mat: Mat.t) => {
    mat: mat,
    inv_mat: Mat.invert(mat)
};

let _transform = (mat: Mat.t, v: Vec.t) => {
    let x = (v.x *. mat.storage[0][0]) +. (v.y *. mat.storage[1][0]) +. (v.z *. mat.storage[2][0])
            +. mat.storage[3][0];
    let y = (v.x *. mat.storage[0][1]) +. (v.y *. mat.storage[1][1]) +. (v.z *. mat.storage[2][1])
            +. mat.storage[3][1];
    let z = (v.x *. mat.storage[0][2]) +. (v.y *. mat.storage[1][2]) +. (v.z *. mat.storage[2][2])
            +. mat.storage[3][2];
    let w = (v.x *. mat.storage[0][3]) +. (v.y *. mat.storage[1][3]) +. (v.z *. mat.storage[2][3])
            +. mat.storage[3][3];
    Vec.xyz(x /. w, y /. w, z /. w)
};

let _transform_dir = (mat: Mat.t, v: Vec.t) => {
    Vec.xyz(v.x *. mat.storage[0][0] +. v.y *. mat.storage[1][0] +. v.z *. mat.storage[2][0],
            v.x *. mat.storage[0][1] +. v.y *. mat.storage[1][1] +. v.z *. mat.storage[2][1],
            v.x *. mat.storage[0][2] +. v.y *. mat.storage[1][2] +. v.z *. mat.storage[2][2])
};

let _transform_normal = (inv_mat: Mat.t, v: Vec.t) => {
    Vec.xyz((v.x *. inv_mat.storage[0][0]) +. (v.y *. inv_mat.storage[0][1])
                +. (v.z *. inv_mat.storage[0][2]),
            (v.x *. inv_mat.storage[1][0]) +. (v.y *. inv_mat.storage[1][1])
                +. (v.z *. inv_mat.storage[1][2]),
            (v.x *. inv_mat.storage[2][0]) +. (v.y *. inv_mat.storage[2][1])
                +. (v.z *. inv_mat.storage[2][2]))
};

let _transform_ray = (mat: Mat.t, r: Ray.t) => {
    Ray.create(_transform(mat, r.origin), _transform_dir(mat, r.dir))
};

let _transform_bbox = (mat: Mat.t, b: Bbox.t) => {
    List.fold_left(Bbox.union, Bbox.empty, [
        _transform(mat, Vec.xyz(b.max.x, b.max.y, b.max.z)),
        _transform(mat, Vec.xyz(b.max.x, b.max.y, b.min.z)),
        _transform(mat, Vec.xyz(b.max.x, b.min.y, b.max.z)),
        _transform(mat, Vec.xyz(b.min.x, b.max.y, b.max.z)),
        _transform(mat, Vec.xyz(b.min.x, b.min.y, b.max.z)),
        _transform(mat, Vec.xyz(b.min.x, b.max.y, b.min.z)),
        _transform(mat, Vec.xyz(b.max.x, b.min.y, b.min.z)),
        _transform(mat, Vec.xyz(b.min.x, b.min.y, b.min.z))
    ])
};

/** Transforms the given vector as a point. */
let transform_vec = (xform: t, v: Vec.t) => _transform(xform.mat, v);
/** Untransforms the given vector as a point. */
let untransform_vec = (xform: t, v: Vec.t) => _transform(xform.inv_mat, v);

/** Transforms the given vector as a direction. */
let transform_dir = (xform: t, v: Vec.t) => _transform_dir(xform.mat, v);
/** Untransforms the given vector as a direction. */
let untransform_dir = (xform: t, v: Vec.t) => _transform_dir(xform.inv_mat, v);

/**
 * Transforms the given vector as a surface normal.
 * Note that normals are transformed by the transposed inverted matrix instead of the
 * regular matrix.
 */
let transform_normal = (xform: t, v: Vec.t) => _transform_normal(xform.inv_mat, v);
/** Untransforms the given vector as a surface normal. */
let untransform_normal = (xform: t, v: Vec.t) => _transform_normal(xform.mat, v);

/** Transforms the given ray. */
let transform_ray = (xform: t, r: Ray.t) => _transform_ray(xform.mat, r);
/** Untransforms the given ray. */
let untransform_ray = (xform: t, r: Ray.t) => _transform_ray(xform.inv_mat, r);

/** Transforms the given bounding box. */
let transform_bbox = (xform: t, b: Bbox.t) => _transform_bbox(xform.mat, b);
/**
 * Untransforms the given bounding box.
 * Note that this is not the exact inverse of transform_bbox;
 * `untransform_bbox(transform_bbox(...))` may cause the bounding box to grow.
 */
let untransform_bbox = (xform: t, b: Bbox.t) => _transform_bbox(xform.inv_mat, b);
