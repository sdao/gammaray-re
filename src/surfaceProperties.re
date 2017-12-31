/** Encapsulates surface properties at the point of an intersection. */
type t = {
    normal: Vec.t,
    tangent: Vec.t,
    binormal: Vec.t,
    geom_normal: Vec.t
};

let empty = {
    normal: Vec.zero,
    tangent: Vec.zero,
    binormal: Vec.zero,
    geom_normal: Vec.zero
};

let create = (n: Vec.t, t: Vec.t, b: Vec.t, gn: Vec.t) => {
    {normal: n, tangent: t, binormal: b, geom_normal: gn}
};
