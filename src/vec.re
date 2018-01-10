/** 3-d column vector. Used for both points and directions. */
type t = {
    x: float,
    y: float,
    z: float,
};

/**
 * Infix operators for Vec module.
 * You can `open Vec::Ops` to use them without having to open the entire
 * module. The operators are all of the form ?^ where the caret is
 * reminiscent of the arrow-above notation for mathematical vectors.
 */
module Ops = {
    /** Component-wise addition. */
    let (+^) = (a: t, b: t) => {
        {x: a.x +. b.x, y: a.y +. b.y, z: a.z +. b.z}
    };

    /** Component-wise subtraction. */
    let (-^) = (a: t, b: t) => {
        {x: a.x -. b.x, y: a.y -. b.y, z: a.z -. b.z}
    };

    /** Component-wise multiplication. */
    let (*^) = (a: t, b: t) => {
        {x: a.x *. b.x, y: a.y *. b.y, z: a.z *. b.z}
    };

    /** Component-wise multiplication of vector by scalar. */
    let (*^.) = (a: t, k: float) => {
        {x: a.x *. k, y: a.y *. k, z: a.z *. k}
    };

    /** Component-wise division. */
    let (/^) = (a: t, b: t) => {
        {x: a.x /. b.x, y: a.y /. b.y, z: a.z /. b.z}
    };

    /** Component-wise division of vector by scalar. */
    let (/^.) = (a: t, k: float) => {
        {x: a.x /. k, y: a.y /. k, z: a.z /. k}
    };

    /** Unary negation of all components. */
    let (~-^) = (a: t) => {
        {x: ~-.a.x, y: ~-.a.y, z: ~-.a.z}
    };
};

open Ops;

/** Standard constructor for t from x/y/z values. */
let xyz = (x: float, y: float, z: float) => {
    {x: x, y: y, z: z}
};

/** Standard constructor for t filling x/y/z with k. */
let from_scalar = (k: float) => {
    {x: k, y: k, z: k}
};

/**
 * Computes a vector from spherical coordinates, with radius 1, inclination
 * theta, and azimuth phi.
 */
let from_spherical = (cos_theta: float, phi: float) => {
    let sin_theta = sqrt(1.0 -. (cos_theta *. cos_theta));
    xyz(sin_theta *. cos(phi), sin_theta *. sin(phi), cos_theta)
};

let zero = xyz(0.0, 0.0, 0.0);
let one = xyz(1.0, 1.0, 1.0);

let x_axis = xyz(1.0, 0.0, 0.0);
let y_axis = xyz(0.0, 1.0, 0.0);
let z_axis = xyz(0.0, 0.0, 1.0);

let red = x_axis;
let green = y_axis;
let blue = z_axis;

let repr = (a: t) => {
    Printf.sprintf("(%f, %f, %f)", a.x, a.y, a.z)
};

/** Cross product of two vectors. */
let cross = (a: t, b: t) => {
    xyz(
        (a.y *. b.z) -. (a.z *. b.y),
        (a.z *. b.x) -. (a.x *. b.z),
        (a.x *. b.y) -. (a.y *. b.x))
};

/** Dot (scalar) product of two vectors. */
let dot = (a: t, b: t) => {
    (a.x *. b.x) +. (a.y *. b.y) +. (a.z *. b.z);
};

/** Whether all vector components are exactly zero. */
let is_exactly_zero = (v: t) => {
    v.x == 0.0 && v.y == 0.0 && v.z == 0.0
};

/** Whether the squared magnitude is within epsilon of zero. */
let is_nearly_zero = (v: t) => {
    Math.is_nearly_zero(dot(v, v))
};

/** Whether two vectors are component-wise within the given epsilon of each other. */
let is_close = (a: t, b: t, eps: float) => {
    Math.is_close(a.x, b.x, eps) &&
            Math.is_close(a.y, b.y, eps) &&
            Math.is_close(a.z, b.z, eps)
};

/** Magnitude (length) of a vector. */
let magnitude = (v: t) => {
    sqrt(dot(v, v))
};

/** Returns a normalized (unit-length) copy of the given vector. */
let normalized = (v: t) => {
    let length = magnitude(v);
    xyz(v.x /. length, v.y /. length, v.z /. length)
};

/** Component-wise square root. */
let comp_sqrt = (v: t) => {
    xyz(sqrt(v.x), sqrt(v.y), sqrt(v.z))
};

/**
 * Generates an orthonormal coordinate basis. The first vector must be given,
 * and the other two orthogonal vectors will be generated from it.
 * Taken from page 63 of Pharr & Humphreys' Physically-Based Rendering 2e.
 */
let coord_system = (v1: t) => {
    if (abs_float(v1.x) > abs_float(v1.y)) {
        let inv_len = 1.0 /. sqrt((v1.x *. v1.x) +. (v1.z *. v1.z));
        let v2 = xyz(~-.v1.z *. inv_len, 0.0, v1.x *. inv_len);
        let v3 = cross(v1, v2);
        (v2, v3)
    }
    else {
        let inv_len = 1.0 /. sqrt((v1.y *. v1.y) +. (v1.z *. v1.z));
        let v2 = xyz(0.0, v1.z *. inv_len, ~-.v1.y *. inv_len);
        let v3 = cross(v1, v2);
        (v2, v3)
    }
};

/**
 * Converts a world-space vector to a local coordinate system defined by
 * a vector basis.
 * The resulting coordinates are (x, y, z), where x is the weight of the
 * tangent, y is the weight of the binormal, and z is the weight of the
 * normal.
 */
let world_to_local = (v: t, tangent: t, binormal: t, normal: t) =>
{
    xyz(dot(v, tangent), dot(v, binormal), dot(v, normal))
};

/**
 * Converts a local-space vector back to world-space. The local-space vector
 * should be (x, y, z), where x is the weight of the tangent, y is the weight
 * of the binormal, and z is the weight of the normal.
 */
let local_to_world = (v: t, tangent: t, binormal: t, normal: t) =>
{
    xyz(
        (tangent.x *. v.x) +. (binormal.x *. v.y) +. (normal.x *. v.z),
        (tangent.y *. v.x) +. (binormal.y *. v.y) +. (normal.y *. v.z),
        (tangent.z *. v.x) +. (binormal.z *. v.y) +. (normal.z *. v.z))
};

let cos_theta = (v: t) => v.z;
let cos2_theta = (v: t) => v.z *. v.z;
let abs_cos_theta = (v: t) => abs_float(v.z);
let sin2_theta = (v: t) => max(0.0, 1.0 -. cos2_theta(v));
let sin_theta = (v: t) => sqrt(sin2_theta(v));
let tan_theta = (v: t) => sin_theta(v) /. cos_theta(v);
let tan2_theta = (v: t) => sin2_theta(v) /. cos2_theta(v);
let cos_phi = (v: t) => {
    let sin_t = sin_theta(v);
    if (sin_t == 0.0) {
        1.0
    }
    else {
        Math.clamp(v.x /. sin_t, -1.0, 1.0)
    }
};
let cos2_phi = (v: t) => cos_phi(v) *. cos_phi(v);
let sin_phi = (v: t) => {
    let sin_t = sin_theta(v);
    if (sin_t == 0.0) {
        0.0
    }
    else {
        Math.clamp(v.y /. sin_t, -1.0, 1.0)
    }
};
let sin2_phi = (v: t) => sin_phi(v) *. sin_phi(v);

/**
 * Determines if two vectors in the same local coordinate space are in the
 * same hemisphere.
 */
let is_local_same_hemisphere = (a: t, b: t) => {
    a.z *. b.z > 0.0
};

/**
 * Luminance of an RGB color stored in a vector.
 */
let luminance = (v: t) => {
    (0.21 *. v.x) +. (0.71 *. v.y) +. (0.08 *. v.z)
};

/**
 * Interprets this vector as a color; returns a version normalized by luminance
 * to isolate hue and saturation.
 */
let tint = (v: t) => {
    let lume = luminance(v);
    if (lume > 0.0) {
        v /^. lume
    }
    else {
        one
    }
};

/**
 * Reflects a vector over a surface normal. The original and reflected vectors both
 * point away from the surface. (This produces the opposite result of GLSL reflect.)
 */
let reflect = (v: t, n: t) => {
    let k = 2.0 *. dot(n, v);
    xyz(
        (n.x *. k) -. v.x,
        (n.y *. k) -. v.y,
        (n.z *. k) -. v.z)
};

/**
 * Refracts a vector over a surface with the given angle and eta (IOR). The original and
 * refracted vectors both point away from the surface. (This produces a different result
 * from GLSL refract.)
 */
let refract = (v: t, n: t, eta: float) => {
    let cos_theta_in = dot(n, v);
    let sin2_theta_in = max(0.0, 1.0 -. (cos_theta_in *. cos_theta_in));
    let sin2_theta_trans = eta *. eta *. sin2_theta_in;
    if (sin2_theta_trans >= 1.0) {
        zero
    }
    else {
        let cos_theta_trans = sqrt(1.0 -. sin2_theta_trans);
        (v *^. ~-.eta) +^
                (n *^. ((eta *. cos_theta_in) -. cos_theta_trans))
    }
};

/** Whether all vector components are non-infinity and non-NaN. */
let is_finite = (v: t) => {
    Math.is_finite(v.x) && Math.is_finite(v.y) && Math.is_finite(v.z)
};

/**
 * Linearly interpolates between two vectors given a scale k; when k is 0.0, the value is a,
 * when k is 1.0, the value is b; otherwise the value is interpolated between a and b, with
 * extrapolation for k < 0.0 or k > 1.0.
 */
let lerp = (a: t, b: t, k: float) => {
    xyz(
        Math.lerp(a.x, b.x, k),
        Math.lerp(a.y, b.y, k),
        Math.lerp(a.z, b.z, k))
};

let get = (v: t, i: int) => {
    switch (i) {
        | 0 => v.x
        | 1 => v.y
        | 2 => v.z
        | _ => assert(false)
    }
};