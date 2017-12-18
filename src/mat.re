type mat4 = {
    storage: array(array(float))
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
    0.0
};
