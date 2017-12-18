let x = 1 + 2;
let y = Printf.sprintf("blah %d", x);
print_endline(y);

open Vec.Ops;
let a = Vec.xyz(1.0, 2.0, 3.0);
let b = Vec.xyz(2.0, 3.0, 4.0);
let ab = a +^ b;
print_endline(Vec.repr(ab));

let c = Vec.x_axis;
let d = Vec.y_axis;
let cd = Vec.cross(c, d);
let cdstr = Printf.sprintf("%f %f %f", cd.x, cd.y, cd.z);
print_endline(Vec.repr(cd));

let m = Mat.from_array(Array.make_matrix(4, 5, 0.0));
