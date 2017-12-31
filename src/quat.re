open Vec.Ops;

/** Quaternion with a real part and an imaginary part. */
type t = {
    real: float,
    im: Vec.t,
};

/**
 * Infix operators for Quat module.
 * You can `open Quat::Ops` to use them without having to open the entire
 * module. The operators are all of the form ?%.
 */
module Ops = {
    /** Composes the rotations of two quaternions. */
    let (*%) = (a: t, b: t) => {
        let r1 = a.real;
        let r2 = b.real;

        let i1 = a.im;
        let i2 = b.im;

        let r = (r1 *. r2) -. Vec.dot(i1, i2);
        let i = Vec.xyz(
            r1 *. i2.x +. r2 *. i1.x +. (i1.y *. i2.z -. i1.z *. i2.y),
            r1 *. i2.y +. r2 *. i1.y +. (i1.z *. i2.x -. i1.x *. i2.z),
            r1 *. i2.z +. r2 *. i1.z +. (i1.x *. i2.y -. i1.y *. i2.x));
        
        {real: r, im: i}
    };

    /** Inverts the direction of the rotation specified by the quaternion. */
    let (~-%) = (a: t) => {
        let lsq = (a.real *. a.real) +. Vec.dot(a.im, a.im);
        {real: a.real /. lsq, im: (~-^a.im) /^. lsq}
    };
};

/** The identity quaternion, representing no rotation. */
let identity = {real: 1.0, im: Vec.zero};

/** Creates a quaternion with the specified real and imaginary parts. */
let create = (real: float, im: Vec.t) => {real: real, im: im};

/** Scales the rotation of the quaternion by the given amount. */
let scale = (a: t, k: float) => {real: a.real *. k, im: a.im *^. k};
