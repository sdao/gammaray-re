type mat4 = {
    storage: array(array(float))
};

/**
 * Infix operators for Matrix module.
 * You can `open Vec::Ops` to use them without having to open the entire
 * module. The operators are all of the form ?# where the pound sign sort of looks like a matrix.
 */
module Ops = {
    let (*#) = (a: mat4, b: mat4) => {
        let data = Array.make_matrix(4, 4, 0.0);
        data[0][0] = a.storage[0][0] *. b.storage[0][0] +.
                     a.storage[0][1] *. b.storage[1][0] +.
                     a.storage[0][2] *. b.storage[2][0] +.
                     a.storage[0][3] *. b.storage[3][0];
        data[0][1] = a.storage[0][0] *. b.storage[0][1] +.
                     a.storage[0][1] *. b.storage[1][1] +.
                     a.storage[0][2] *. b.storage[2][1] +.
                     a.storage[0][3] *. b.storage[3][1];
        data[0][2] = a.storage[0][0] *. b.storage[0][2] +.
                     a.storage[0][1] *. b.storage[1][2] +.
                     a.storage[0][2] *. b.storage[2][2] +.
                     a.storage[0][3] *. b.storage[3][2];
        data[0][3] = a.storage[0][0] *. b.storage[0][3] +.
                     a.storage[0][1] *. b.storage[1][3] +.
                     a.storage[0][2] *. b.storage[2][3] +.
                     a.storage[0][3] *. b.storage[3][3];
        data[1][0] = a.storage[1][0] *. b.storage[0][0] +.
                     a.storage[1][1] *. b.storage[1][0] +.
                     a.storage[1][2] *. b.storage[2][0] +.
                     a.storage[1][3] *. b.storage[3][0];
        data[1][1] = a.storage[1][0] *. b.storage[0][1] +.
                     a.storage[1][1] *. b.storage[1][1] +.
                     a.storage[1][2] *. b.storage[2][1] +.
                     a.storage[1][3] *. b.storage[3][1];
        data[1][2] = a.storage[1][0] *. b.storage[0][2] +.
                     a.storage[1][1] *. b.storage[1][2] +.
                     a.storage[1][2] *. b.storage[2][2] +.
                     a.storage[1][3] *. b.storage[3][2];
        data[1][3] = a.storage[1][0] *. b.storage[0][3] +.
                     a.storage[1][1] *. b.storage[1][3] +.
                     a.storage[1][2] *. b.storage[2][3] +.
                     a.storage[1][3] *. b.storage[3][3];
        data[2][0] = a.storage[2][0] *. b.storage[0][0] +.
                     a.storage[2][1] *. b.storage[1][0] +.
                     a.storage[2][2] *. b.storage[2][0] +.
                     a.storage[2][3] *. b.storage[3][0];
        data[2][1] = a.storage[2][0] *. b.storage[0][1] +.
                     a.storage[2][1] *. b.storage[1][1] +.
                     a.storage[2][2] *. b.storage[2][1] +.
                     a.storage[2][3] *. b.storage[3][1];
        data[2][2] = a.storage[2][0] *. b.storage[0][2] +.
                     a.storage[2][1] *. b.storage[1][2] +.
                     a.storage[2][2] *. b.storage[2][2] +.
                     a.storage[2][3] *. b.storage[3][2];
        data[2][3] = a.storage[2][0] *. b.storage[0][3] +.
                     a.storage[2][1] *. b.storage[1][3] +.
                     a.storage[2][2] *. b.storage[2][3] +.
                     a.storage[2][3] *. b.storage[3][3];
        data[3][0] = a.storage[3][0] *. b.storage[0][0] +.
                     a.storage[3][1] *. b.storage[1][0] +.
                     a.storage[3][2] *. b.storage[2][0] +.
                     a.storage[3][3] *. b.storage[3][0];
        data[3][1] = a.storage[3][0] *. b.storage[0][1] +.
                     a.storage[3][1] *. b.storage[1][1] +.
                     a.storage[3][2] *. b.storage[2][1] +.
                     a.storage[3][3] *. b.storage[3][1];
        data[3][2] = a.storage[3][0] *. b.storage[0][2] +.
                     a.storage[3][1] *. b.storage[1][2] +.
                     a.storage[3][2] *. b.storage[2][2] +.
                     a.storage[3][3] *. b.storage[3][2];
        data[3][3] = a.storage[3][0] *. b.storage[0][3] +.
                     a.storage[3][1] *. b.storage[1][3] +.
                     a.storage[3][2] *. b.storage[2][3] +.
                     a.storage[3][3] *. b.storage[3][3];

        {storage: data}
    };
};

let from_array = (data: array(array(float))) => {
    assert(Array.length(data) == 4);
    assert(Array.length(data[0]) == 4);
    assert(Array.length(data[1]) == 4);
    assert(Array.length(data[2]) == 4);
    assert(Array.length(data[3]) == 4);
    {storage: data}
};

let diagonal = (k: float) => {
    let data = Array.make_matrix(4, 4, 0.0);
    data[0][0] = k;
    data[1][1] = k;
    data[2][2] = k;
    data[3][3] = k;
    {storage: data}
};

let zero = diagonal(0.0);
let identity = diagonal(1.0);

let repr = (m: mat4) => {
    let s = m.storage;
    Printf.sprintf("((%f, %f, %f, %f), (%f, %f, %f, %f), (%f, %f, %f, %f), (%f, %f, %f, %f))",
            s[0][0], s[0][1], s[0][2], s[0][3],
            s[1][0], s[1][1], s[1][2], s[1][3],
            s[2][0], s[2][1], s[2][2], s[2][3],
            s[3][0], s[3][1], s[3][2], s[3][3])
};

let scale = (k : float) => {
    let data = Array.make_matrix(4, 4, 0.0);
    data[0][0] = k;
    data[1][1] = k;
    data[2][2] = k;
    data[3][3] = 1.0;
    {storage: data}
};

let translation = (translate: Vec.vec3) => {
    let data = Array.make_matrix(4, 4, 0.0);
    data[0][0] = 1.0;
    data[1][1] = 1.0;
    data[2][2] = 1.0;
    data[3][0] = translate.x;
    data[3][1] = translate.y;
    data[3][2] = translate.z;
    data[3][3] = 1.0;
    {storage: data}
};

let rotation = (rotate: Quat.quat) => {
    let r = rotate.real;
    let i = rotate.im;

    let data = Array.make_matrix(4, 4, 0.0);
    data[0][0] = 1.0 -. 2.0 *. (i.y *. i.y +. i.z *. i.z);
    data[0][1] =        2.0 *. (i.x *. i.y +. i.z *.   r);
    data[0][2] =        2.0 *. (i.z *. i.x -. i.y *.   r);
    data[0][3] = 0.0;

    data[1][0] =        2.0 *. (i.x *. i.y -. i.z *.   r);
    data[1][1] = 1.0 -. 2.0 *. (i.z *. i.z +. i.x *. i.x);
    data[1][2] =        2.0 *. (i.y *. i.z +. i.x *.   r);
    data[1][3] = 0.0;

    data[2][0] =        2.0 *. (i.z *. i.x +. i.y *.   r);
    data[2][1] =        2.0 *. (i.y *. i.z -. i.x *.   r);
    data[2][2] = 1.0 -. 2.0 *. (i.y *. i.y +. i.x *. i.x);
    data[2][3] = 0.0;

    data[3][0] = 0.0;
    data[3][1] = 0.0;
    data[3][2] = 0.0;
    data[3][3] = 1.0;
    
    {storage: data}
};

let transpose = (m: mat4) => {
    let data = Array.make_matrix(4, 4, 0.0);
    for (row in 0 to 3) {
        for (col in 0 to 3) {
            data[row][col] = m.storage[col][row];
        };
    };
    {storage: data}
};

/** Determinant of 3x3 submatrix given by three rows and columns. */
let determinant3 =
    (m: mat4, r1: int, r2: int, r3: int, c1: int, c2: int, c3: int) =>
{
    let s = m.storage;

    s[r1][c1] *. s[r2][c2] *. s[r3][c3]
	    +. s[r1][c2] *. s[r2][c3] *. s[r3][c1]
	    +. s[r1][c3] *. s[r2][c1] *. s[r3][c2]
	    -. s[r1][c1] *. s[r2][c3] *. s[r3][c2]
	    -. s[r1][c2] *. s[r2][c1] *. s[r3][c3]
        -. s[r1][c3] *. s[r2][c2] *. s[r3][c1]
};

let determinant = (m: mat4) =>
    ~-. m.storage[0][3] *. determinant3(m, 1, 2, 3, 0, 1, 2)
     +. m.storage[1][3] *. determinant3(m, 0, 2, 3, 0, 1, 2)
     -. m.storage[2][3] *. determinant3(m, 0, 1, 3, 0, 1, 2)
     +. m.storage[3][3] *. determinant3(m, 0, 1, 2, 0, 1, 2);

let invert = (m: mat4) => {
    let s = m.storage;
    
    let data = Array.make_matrix(4, 4, 0.0);
    data[0][0] =
        s[1][1] *. s[2][2] *. s[3][3] -.
        s[1][1] *. s[2][3] *. s[3][2] -.
        s[2][1] *. s[1][2] *. s[3][3] +.
        s[2][1] *. s[1][3] *. s[3][2] +.
        s[3][1] *. s[1][2] *. s[2][3] -.
        s[3][1] *. s[1][3] *. s[2][2];
    data[1][0] =
     ~-.s[1][0] *. s[2][2] *. s[3][3] +.
        s[1][0] *. s[2][3] *. s[3][2] +.
        s[2][0] *. s[1][2] *. s[3][3] -.
        s[2][0] *. s[1][3] *. s[3][2] -.
        s[3][0] *. s[1][2] *. s[2][3] +.
        s[3][0] *. s[1][3] *. s[2][2];
    data[2][0] =
        s[1][0] *. s[2][1] *. s[3][3] -.
        s[1][0] *. s[2][3] *. s[3][1] -.
        s[2][0] *. s[1][1] *. s[3][3] +.
        s[2][0] *. s[1][3] *. s[3][1] +.
        s[3][0] *. s[1][1] *. s[2][3] -.
        s[3][0] *. s[1][3] *. s[2][1];
    data[3][0] =
     ~-.s[1][0] *. s[2][1] *. s[3][2] +.
        s[1][0] *. s[2][2] *. s[3][1] +.
        s[2][0] *. s[1][1] *. s[3][2] -.
        s[2][0] *. s[1][2] *. s[3][1] -.
        s[3][0] *. s[1][1] *. s[2][2] +.
        s[3][0] *. s[1][2] *. s[2][1];
    data[0][1] =
     ~-.s[0][1] *. s[2][2] *. s[3][3] +.
        s[0][1] *. s[2][3] *. s[3][2] +.
        s[2][1] *. s[0][2] *. s[3][3] -.
        s[2][1] *. s[0][3] *. s[3][2] -.
        s[3][1] *. s[0][2] *. s[2][3] +.
        s[3][1] *. s[0][3] *. s[2][2];
    data[1][1] =
        s[0][0] *. s[2][2] *. s[3][3] -.
        s[0][0] *. s[2][3] *. s[3][2] -.
        s[2][0] *. s[0][2] *. s[3][3] +.
        s[2][0] *. s[0][3] *. s[3][2] +.
        s[3][0] *. s[0][2] *. s[2][3] -.
        s[3][0] *. s[0][3] *. s[2][2];
    data[2][1] =
     ~-.s[0][0] *. s[2][1] *. s[3][3] +.
        s[0][0] *. s[2][3] *. s[3][1] +.
        s[2][0] *. s[0][1] *. s[3][3] -.
        s[2][0] *. s[0][3] *. s[3][1] -.
        s[3][0] *. s[0][1] *. s[2][3] +.
        s[3][0] *. s[0][3] *. s[2][1];
    data[3][1] =
        s[0][0] *. s[2][1] *. s[3][2] -.
        s[0][0] *. s[2][2] *. s[3][1] -.
        s[2][0] *. s[0][1] *. s[3][2] +.
        s[2][0] *. s[0][2] *. s[3][1] +.
        s[3][0] *. s[0][1] *. s[2][2] -.
        s[3][0] *. s[0][2] *. s[2][1];
    data[0][2] =
        s[0][1] *. s[1][2] *. s[3][3] -.
        s[0][1] *. s[1][3] *. s[3][2] -.
        s[1][1] *. s[0][2] *. s[3][3] +.
        s[1][1] *. s[0][3] *. s[3][2] +.
        s[3][1] *. s[0][2] *. s[1][3] -.
        s[3][1] *. s[0][3] *. s[1][2];
    data[1][2] =
     ~-.s[0][0] *. s[1][2] *. s[3][3] +.
        s[0][0] *. s[1][3] *. s[3][2] +.
        s[1][0] *. s[0][2] *. s[3][3] -.
        s[1][0] *. s[0][3] *. s[3][2] -.
        s[3][0] *. s[0][2] *. s[1][3] +.
        s[3][0] *. s[0][3] *. s[1][2];
    data[2][2] =
        s[0][0] *. s[1][1] *. s[3][3] -.
        s[0][0] *. s[1][3] *. s[3][1] -.
        s[1][0] *. s[0][1] *. s[3][3] +.
        s[1][0] *. s[0][3] *. s[3][1] +.
        s[3][0] *. s[0][1] *. s[1][3] -.
        s[3][0] *. s[0][3] *. s[1][1];
    data[3][2] =
     ~-.s[0][0] *. s[1][1] *. s[3][2] +.
        s[0][0] *. s[1][2] *. s[3][1] +.
        s[1][0] *. s[0][1] *. s[3][2] -.
        s[1][0] *. s[0][2] *. s[3][1] -.
        s[3][0] *. s[0][1] *. s[1][2] +.
        s[3][0] *. s[0][2] *. s[1][1];
    data[0][3] =
     ~-.s[0][1] *. s[1][2] *. s[2][3] +.
        s[0][1] *. s[1][3] *. s[2][2] +.
        s[1][1] *. s[0][2] *. s[2][3] -.
        s[1][1] *. s[0][3] *. s[2][2] -.
        s[2][1] *. s[0][2] *. s[1][3] +.
        s[2][1] *. s[0][3] *. s[1][2];
    data[1][3] =
        s[0][0] *. s[1][2] *. s[2][3] -.
        s[0][0] *. s[1][3] *. s[2][2] -.
        s[1][0] *. s[0][2] *. s[2][3] +.
        s[1][0] *. s[0][3] *. s[2][2] +.
        s[2][0] *. s[0][2] *. s[1][3] -.
        s[2][0] *. s[0][3] *. s[1][2];
    data[2][3] =
     ~-.s[0][0] *. s[1][1] *. s[2][3] +.
        s[0][0] *. s[1][3] *. s[2][1] +.
        s[1][0] *. s[0][1] *. s[2][3] -.
        s[1][0] *. s[0][3] *. s[2][1] -.
        s[2][0] *. s[0][1] *. s[1][3] +.
        s[2][0] *. s[0][3] *. s[1][1];
    data[3][3] =
        s[0][0] *. s[1][1] *. s[2][2] -.
        s[0][0] *. s[1][2] *. s[2][1] -.
        s[1][0] *. s[0][1] *. s[2][2] +.
        s[1][0] *. s[0][2] *. s[2][1] +.
        s[2][0] *. s[0][1] *. s[1][2] -.
        s[2][0] *. s[0][2] *. s[1][1];
    
    {storage: data}
};