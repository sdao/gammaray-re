type vec3 = {
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
    let (+^) = (a: vec3, b: vec3) => {
        {x: a.x +. b.x, y: a.y +. b.y, z: a.z +. b.z}
    };

    let (-^) = (a: vec3, b: vec3) => {
        {x: a.x -. b.x, y: a.y -. b.y, z: a.z -. b.z}
    };

    let (*^) = (a: vec3, b: vec3) => {
        {x: a.x *. b.x, y: a.y *. b.y, z: a.z *. b.z}
    };

    let (/^) = (a: vec3, b: vec3) => {
        {x: a.x /. b.x, y: a.y /. b.y, z: a.z /. b.z}
    };
};

open Ops;

/** Standard constructor for vec3 from x/y/z values. */
let xyz = (x: float, y: float, z: float) => {
    {x: x, y: y, z: z}
};

/** Standard constructor for vec3 filling x/y/z with k. */
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

let repr = (a: vec3) => {
    Printf.sprintf("(%f, %f, %f)", a.x, a.y, a.z)
};

let cross = (a: vec3, b: vec3) => {
    xyz(
        (a.y *. b.z) -. (a.z *. b.y),
        (a.z *. b.x) -. (a.x *. b.z),
        (a.x *. b.y) -. (a.y *. b.x))
};

let dot = (a: vec3, b: vec3) => {
    (a.x *. b.x) +. (a.y *. b.y) +. (a.z *. b.z);
};

let is_exactly_zero = (v: vec3) => {
    v.x == 0.0 && v.y == 0.0 && v.z == 0.0
};

let is_nearly_zero = (v: vec3) => {
    Math.is_nearly_zero(dot(v, v))
};

let is_close = (a: vec3, b: vec3, eps: float) => {
    Math.is_close(a.x, b.x, eps) &&
            Math.is_close(a.y, b.y, eps) &&
            Math.is_close(a.z, b.z, eps)
};

let magnitude = (v: vec3) => {
    sqrt(dot(v, v))
};

let normalized = (v: vec3) => {
    let length = magnitude(v);
    xyz(v.x /. length, v.y /. length, v.z /. length)
};

let comp_sqrt = (v: vec3) => {
    xyz(sqrt(v.x), sqrt(v.y), sqrt(v.z))
};

/**
 * Generates an orthonormal coordinate basis. The first vector must be given,
 * and the other two orthogonal vectors will be generated from it.
 * Taken from page 63 of Pharr & Humphreys' Physically-Based Rendering 2e.
 */
let coord_system = (v1: vec3) => {
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
let world_to_local = (v: vec3, tangent: vec3, binormal: vec3, normal: vec3) =>
{
    xyz(dot(v, tangent), dot(v, binormal), dot(v, normal))
};

/**
 * Converts a local-space vector back to world-space. The local-space vector
 * should be (x, y, z), where x is the weight of the tangent, y is the weight
 * of the binormal, and z is the weight of the normal.
 */
let local_to_world = (v: vec3, tangent: vec3, binormal: vec3, normal: vec3) =>
{
    xyz(
        (tangent.x *. v.x) +. (binormal.x *. v.y) +. (normal.x *. v.z),
        (tangent.y *. v.x) +. (binormal.y *. v.y) +. (normal.y *. v.z),
        (tangent.z *. v.x) +. (binormal.z *. v.y) +. (normal.z *. v.z))
};

let cos_theta = (v: vec3) => v.z;
let cos2_theta = (v: vec3) => v.z *. v.z;
let abs_cos_theta = (v: vec3) => abs_float(v.z);
let sin2_theta = (v: vec3) => max(0.0, 1.0 -. cos2_theta(v));
let sin_theta = (v: vec3) => sqrt(sin2_theta(v));
let tan_theta = (v: vec3) => sin_theta(v) /. cos_theta(v);
let tan2_theta = (v: vec3) => sin2_theta(v) /. cos2_theta(v);
let cos_phi = (v: vec3) => {
    let sin_t = sin_theta(v);
    if (sin_t == 0.0) {
        1.0
    }
    else {
        Math.clamp(v.x /. sin_t, -1.0, 1.0)
    }
};
let cos2_phi = (v: vec3) => cos_phi(v) *. cos_phi(v);
let sin_phi = (v: vec3) => {
    let sin_t = sin_theta(v);
    if (sin_t == 0.0) {
        0.0
    }
    else {
        Math.clamp(v.y /. sin_t, -1.0, 1.0)
    }
};
let sin2_phi = (v: vec3) => sin_phi(v) *. sin_phi(v);

/**
 * Determines if two vectors in the same local coordinate space are in the
 * same hemisphere.
 */
let is_local_same_hemisphere = (a: vec3, b: vec3) => {
    a.z *. b.z > 0.0
};

/**
 * Luminance of an RGB color stored in a vector.
 */
let luminance = (v: vec3) => {
    (0.21 *. v.x) +. (0.71 *. v.y) +. (0.08 *. v.z)
};

/**
 * Interprets this vector as a color; returns a version normalized by luminance
 * to isolate hue and saturation.
 */
let tint = (v: vec3) => {
    let lume = luminance(v);
    if (lume > 0.0) {
        v /^ from_scalar(lume)
    }
    else {
        one
    }
};

/**
 * Reflects a vector over a surface normal. The original and reflected vectors both
 * point away from the surface. (This produces the opposite result of GLSL reflect.)
 */
let reflect = (v: vec3, n: vec3) => {
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
let refract = (v: vec3, n: vec3, eta: float) => {
    let cos_theta_in = dot(n, v);
    let sin2_theta_in = max(0.0, 1.0 -. (cos_theta_in *. cos_theta_in));
    let sin2_theta_trans = eta *. eta *. sin2_theta_in;
    if (sin2_theta_trans >= 1.0) {
        zero
    }
    else {
        let cos_theta_trans = sqrt(1.0 -. sin2_theta_trans);
        (from_scalar(~-.eta) *^ v) +^
                (from_scalar((eta *. cos_theta_in) -. cos_theta_trans) *^ n)
    }
};

let is_finite = (v: vec3) => {
    Math.is_finite(v.x) && Math.is_finite(v.y) && Math.is_finite(v.z)
};

let lerp = (a: vec3, b: vec3, k: float) => {
    xyz(
        Math.lerp(a.x, b.x, k),
        Math.lerp(a.y, b.y, k),
        Math.lerp(a.z, b.z, k))
}
