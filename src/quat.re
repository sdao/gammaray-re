type t = {
    real: float,
    im: Vec.t,
};

let identity = {real: 1.0, im: Vec.zero};

let length_squared = (a: t) => {
    (a.real *. a.real) +. Vec.dot(a.im, a.im)
};

let scale = (a: t, k: float) => {
    Vec.Ops.(
        {real: a.real *. k, im: a.im *^ Vec.from_scalar(k)}
    )
};

module Ops = {
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

    let (~-%) = (a: t) => {
        let lsq = length_squared(a);
        Vec.Ops.(
            {real: a.real /. lsq, im: (~-^a.im) /^ Vec.from_scalar(lsq)}
        )
    };
};
