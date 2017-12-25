let s2 = Sphere.create(
    Material.create_debug(Vec.xyz(0.1, 0.2, 0.7)),
    Mat.translation(Vec.xyz(12.0, 3.0, -90.0)),
    5.0);

let s3 = Sphere.create(
    Material.create_debug(Vec.xyz(0.3, 0.7, 0.0)),
    Mat.translation(Vec.xyz(-25.0, 0.0, -50.0)),
    75.0);

let s4 = Sphere.create(
    Material.create_debug(Vec.xyz(0.4, 0.1, 0.3)),
    Mat.translation(Vec.xyz(6.0, -10.0, -90.0)),
    4.0);

let prims = [|s2, s3, s4|];

let height = 512;
let width = int_of_float(float_of_int(height) *. Camera.aspect_ratio(Camera.default));
Printf.printf("Aspect ratio: %f, Width: %d, Height: %d\n",
        Camera.aspect_ratio(Camera.default), width, height);

let stage = Stage.create(prims, Camera.default, Integrator.display_color, width, height);
Stage.trace(stage);

let exr = Exr.create(width, height);
Exr.update(exr, stage.film);

let oc = open_out_bin("output.exr");
Exr.output_exr(oc, exr);
flush(oc);
