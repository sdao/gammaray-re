/** 4x4 matrix in row-major order. */
type t = {
    storage: array(float) /* Row-major; use Math.index. */
};

/**
 * Infix operators for Mat module.
 * You can `open Mat::Ops` to use them without having to open the entire
 * module. The operators are all of the form ?# where the pound sign sort of looks like a matrix.
 */
module Ops = {
    let (*#) = (a: t, b: t) => {
        let data = Array.make(16, 0.0);
        data[0] = a.storage[0] *. b.storage[0] +.
                     a.storage[1] *. b.storage[4] +.
                     a.storage[2] *. b.storage[8] +.
                     a.storage[3] *. b.storage[12];
        data[1] = a.storage[0] *. b.storage[1] +.
                     a.storage[1] *. b.storage[5] +.
                     a.storage[2] *. b.storage[9] +.
                     a.storage[3] *. b.storage[13];
        data[2] = a.storage[0] *. b.storage[2] +.
                     a.storage[1] *. b.storage[6] +.
                     a.storage[2] *. b.storage[10] +.
                     a.storage[3] *. b.storage[14];
        data[3] = a.storage[0] *. b.storage[3] +.
                     a.storage[1] *. b.storage[7] +.
                     a.storage[2] *. b.storage[11] +.
                     a.storage[3] *. b.storage[15];
        data[4] = a.storage[4] *. b.storage[0] +.
                     a.storage[5] *. b.storage[4] +.
                     a.storage[6] *. b.storage[8] +.
                     a.storage[7] *. b.storage[12];
        data[5] = a.storage[4] *. b.storage[1] +.
                     a.storage[5] *. b.storage[5] +.
                     a.storage[6] *. b.storage[9] +.
                     a.storage[7] *. b.storage[13];
        data[6] = a.storage[4] *. b.storage[2] +.
                     a.storage[5] *. b.storage[6] +.
                     a.storage[6] *. b.storage[10] +.
                     a.storage[7] *. b.storage[14];
        data[7] = a.storage[4] *. b.storage[3] +.
                     a.storage[5] *. b.storage[7] +.
                     a.storage[6] *. b.storage[11] +.
                     a.storage[7] *. b.storage[15];
        data[8] = a.storage[8] *. b.storage[0] +.
                     a.storage[9] *. b.storage[4] +.
                     a.storage[10] *. b.storage[8] +.
                     a.storage[11] *. b.storage[12];
        data[9] = a.storage[8] *. b.storage[1] +.
                     a.storage[9] *. b.storage[5] +.
                     a.storage[10] *. b.storage[9] +.
                     a.storage[11] *. b.storage[13];
        data[10] = a.storage[8] *. b.storage[2] +.
                     a.storage[9] *. b.storage[6] +.
                     a.storage[10] *. b.storage[10] +.
                     a.storage[11] *. b.storage[14];
        data[11] = a.storage[8] *. b.storage[3] +.
                     a.storage[9] *. b.storage[7] +.
                     a.storage[10] *. b.storage[11] +.
                     a.storage[11] *. b.storage[15];
        data[12] = a.storage[12] *. b.storage[0] +.
                     a.storage[13] *. b.storage[4] +.
                     a.storage[14] *. b.storage[8] +.
                     a.storage[15] *. b.storage[12];
        data[13] = a.storage[12] *. b.storage[1] +.
                     a.storage[13] *. b.storage[5] +.
                     a.storage[14] *. b.storage[9] +.
                     a.storage[15] *. b.storage[13];
        data[14] = a.storage[12] *. b.storage[2] +.
                     a.storage[13] *. b.storage[6] +.
                     a.storage[14] *. b.storage[10] +.
                     a.storage[15] *. b.storage[14];
        data[15] = a.storage[12] *. b.storage[3] +.
                     a.storage[13] *. b.storage[7] +.
                     a.storage[14] *. b.storage[11] +.
                     a.storage[15] *. b.storage[15];

        {storage: data}
    };
};

/** Creates a matrix from a 1-d row-major array of length 16. */
let from_array = (data: array(float)) => {
    assert(Array.length(data) == 16);
    {storage: data}
};

/** Creates a matrix of all 0's except for k's running down the diagonal. */
let diagonal = (k: float) => {
    let data = Array.make(16, 0.0);
    data[0] = k;
    data[5] = k;
    data[10] = k;
    data[15] = k;
    {storage: data}
};

let zero = diagonal(0.0);
let identity = diagonal(1.0);

let repr = (m: t) => {
    let s = m.storage;
    Printf.sprintf("((%f, %f, %f, %f), (%f, %f, %f, %f), (%f, %f, %f, %f), (%f, %f, %f, %f))",
            s[0], s[1], s[2], s[3],
            s[4], s[5], s[6], s[7],
            s[8], s[9], s[10], s[11],
            s[12], s[13], s[14], s[15])
};

/** Creates a matrix representing a uniform scale transformation. */
let scale = (k : float) => {
    let data = Array.make(16, 0.0);
    data[0] = k;
    data[5] = k;
    data[10] = k;
    data[15] = 1.0;
    {storage: data}
};

/** Creates a matrix representing translation by the given vector direction. */
let translation = (translate: Vec.t) => {
    let data = Array.make(16, 0.0);
    data[0] = 1.0;
    data[5] = 1.0;
    data[10] = 1.0;
    data[12] = translate.x;
    data[13] = translate.y;
    data[14] = translate.z;
    data[15] = 1.0;
    {storage: data}
};

/** Creates a matrix representing rotation by the given quaternion. */
let rotation = (rotate: Quat.t) => {
    let r = rotate.real;
    let i = rotate.im;

    let data = Array.make(16, 0.0);
    data[0] = 1.0 -. 2.0 *. (i.y *. i.y +. i.z *. i.z);
    data[1] =        2.0 *. (i.x *. i.y +. i.z *.   r);
    data[2] =        2.0 *. (i.z *. i.x -. i.y *.   r);
    data[3] = 0.0;

    data[4] =        2.0 *. (i.x *. i.y -. i.z *.   r);
    data[5] = 1.0 -. 2.0 *. (i.z *. i.z +. i.x *. i.x);
    data[6] =        2.0 *. (i.y *. i.z +. i.x *.   r);
    data[7] = 0.0;

    data[8] =        2.0 *. (i.z *. i.x +. i.y *.   r);
    data[9] =        2.0 *. (i.y *. i.z -. i.x *.   r);
    data[10] = 1.0 -. 2.0 *. (i.y *. i.y +. i.x *. i.x);
    data[11] = 0.0;

    data[12] = 0.0;
    data[13] = 0.0;
    data[14] = 0.0;
    data[15] = 1.0;
    
    {storage: data}
};

/** Returns the transpose of the given matrix. */
let transpose = (m: t) => {
    let data = Array.make(16, 0.0);
    for (row in 0 to 3) {
        for (col in 0 to 3) {
            data[Math.index(row, col, 4)] = m.storage[Math.index(col, row, 4)];
        };
    };
    {storage: data}
};

/** Determinant of 3x3 submatrix given by three rows and columns. */
let determinant3 =
    (m: t, r1: int, r2: int, r3: int, c1: int, c2: int, c3: int) =>
{
    let s = m.storage;

    let r1_ = Math.index(r1, 0, 4);
    let r2_ = Math.index(r2, 0, 4);
    let r3_ = Math.index(r3, 0, 4);

    s[r1_ + c1] *. s[r2_ + c2] *. s[r3_ + c3]
	    +. s[r1_ + c2] *. s[r2_ + c3] *. s[r3_ + c1]
	    +. s[r1_ + c3] *. s[r2_ + c1] *. s[r3_ + c2]
	    -. s[r1_ + c1] *. s[r2_ + c3] *. s[r3_ + c2]
	    -. s[r1_ + c2] *. s[r2_ + c1] *. s[r3_ + c3]
        -. s[r1_ + c3] *. s[r2_ + c2] *. s[r3_ + c1]
};

/** Returns the determinant of the given matrix. */
let determinant = (m: t) =>
    ~-. m.storage[3] *. determinant3(m, 1, 2, 3, 0, 1, 2)
     +. m.storage[7] *. determinant3(m, 0, 2, 3, 0, 1, 2)
     -. m.storage[11] *. determinant3(m, 0, 1, 3, 0, 1, 2)
     +. m.storage[15] *. determinant3(m, 0, 1, 2, 0, 1, 2);

/** Returns the inversion of the given matrix. */
let invert = (m: t) => {
    let s = m.storage;
    
    let data = Array.make(16, 0.0);
    data[0] =
        s[5] *. s[10] *. s[15] -.
        s[5] *. s[11] *. s[14] -.
        s[9] *. s[6] *. s[15] +.
        s[9] *. s[7] *. s[14] +.
        s[13] *. s[6] *. s[11] -.
        s[13] *. s[7] *. s[10];
    data[4] =
     ~-.s[4] *. s[10] *. s[15] +.
        s[4] *. s[11] *. s[14] +.
        s[8] *. s[6] *. s[15] -.
        s[8] *. s[7] *. s[14] -.
        s[12] *. s[6] *. s[11] +.
        s[12] *. s[7] *. s[10];
    data[8] =
        s[4] *. s[9] *. s[15] -.
        s[4] *. s[11] *. s[13] -.
        s[8] *. s[5] *. s[15] +.
        s[8] *. s[7] *. s[13] +.
        s[12] *. s[5] *. s[11] -.
        s[12] *. s[7] *. s[9];
    data[12] =
     ~-.s[4] *. s[9] *. s[14] +.
        s[4] *. s[10] *. s[13] +.
        s[8] *. s[5] *. s[14] -.
        s[8] *. s[6] *. s[13] -.
        s[12] *. s[5] *. s[10] +.
        s[12] *. s[6] *. s[9];
    data[1] =
     ~-.s[1] *. s[10] *. s[15] +.
        s[1] *. s[11] *. s[14] +.
        s[9] *. s[2] *. s[15] -.
        s[9] *. s[3] *. s[14] -.
        s[13] *. s[2] *. s[11] +.
        s[13] *. s[3] *. s[10];
    data[5] =
        s[0] *. s[10] *. s[15] -.
        s[0] *. s[11] *. s[14] -.
        s[8] *. s[2] *. s[15] +.
        s[8] *. s[3] *. s[14] +.
        s[12] *. s[2] *. s[11] -.
        s[12] *. s[3] *. s[10];
    data[9] =
     ~-.s[0] *. s[9] *. s[15] +.
        s[0] *. s[11] *. s[13] +.
        s[8] *. s[1] *. s[15] -.
        s[8] *. s[3] *. s[13] -.
        s[12] *. s[1] *. s[11] +.
        s[12] *. s[3] *. s[9];
    data[13] =
        s[0] *. s[9] *. s[14] -.
        s[0] *. s[10] *. s[13] -.
        s[8] *. s[1] *. s[14] +.
        s[8] *. s[2] *. s[13] +.
        s[12] *. s[1] *. s[10] -.
        s[12] *. s[2] *. s[9];
    data[2] =
        s[1] *. s[6] *. s[15] -.
        s[1] *. s[7] *. s[14] -.
        s[5] *. s[2] *. s[15] +.
        s[5] *. s[3] *. s[14] +.
        s[13] *. s[2] *. s[7] -.
        s[13] *. s[3] *. s[6];
    data[6] =
     ~-.s[0] *. s[6] *. s[15] +.
        s[0] *. s[7] *. s[14] +.
        s[4] *. s[2] *. s[15] -.
        s[4] *. s[3] *. s[14] -.
        s[12] *. s[2] *. s[7] +.
        s[12] *. s[3] *. s[6];
    data[10] =
        s[0] *. s[5] *. s[15] -.
        s[0] *. s[7] *. s[13] -.
        s[4] *. s[1] *. s[15] +.
        s[4] *. s[3] *. s[13] +.
        s[12] *. s[1] *. s[7] -.
        s[12] *. s[3] *. s[5];
    data[14] =
     ~-.s[0] *. s[5] *. s[14] +.
        s[0] *. s[6] *. s[13] +.
        s[4] *. s[1] *. s[14] -.
        s[4] *. s[2] *. s[13] -.
        s[12] *. s[1] *. s[6] +.
        s[12] *. s[2] *. s[5];
    data[3] =
     ~-.s[1] *. s[6] *. s[11] +.
        s[1] *. s[7] *. s[10] +.
        s[5] *. s[2] *. s[11] -.
        s[5] *. s[3] *. s[10] -.
        s[9] *. s[2] *. s[7] +.
        s[9] *. s[3] *. s[6];
    data[7] =
        s[0] *. s[6] *. s[11] -.
        s[0] *. s[7] *. s[10] -.
        s[4] *. s[2] *. s[11] +.
        s[4] *. s[3] *. s[10] +.
        s[8] *. s[2] *. s[7] -.
        s[8] *. s[3] *. s[6];
    data[11] =
     ~-.s[0] *. s[5] *. s[11] +.
        s[0] *. s[7] *. s[9] +.
        s[4] *. s[1] *. s[11] -.
        s[4] *. s[3] *. s[9] -.
        s[8] *. s[1] *. s[7] +.
        s[8] *. s[3] *. s[5];
    data[15] =
        s[0] *. s[5] *. s[10] -.
        s[0] *. s[6] *. s[9] -.
        s[4] *. s[1] *. s[10] +.
        s[4] *. s[2] *. s[9] +.
        s[8] *. s[1] *. s[6] -.
        s[8] *. s[2] *. s[5];
    
    {storage: data}
};
