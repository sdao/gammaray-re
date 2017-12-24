open Mat.Ops;

/** Perspective camera representation. */
type t = {
    focal_length: float, /* Distance from eye to focal plane. Longer is more magnified. */
    horizontal_aperture: float, /* Width of projector aperture. In world units, not mm. */
    vertical_aperture: float, /* Height of projector aperture. In world units, not mm. */
    f_stop: float, /* f-number or focal ratio. Larger means more depth of field. */
    xform: Xform.t
};

module Constants = {
    /* Based on the "Academy format" for 35mm which gives a 1.375:1 ratio. */
    let horizontal_aperture_35mm = 2.2;
    let vertical_aperture_35mm = 1.6;
};

/* Just a camera with some nice preset defaults. */
let default = {
    focal_length: 5.0,
    horizontal_aperture: Constants.horizontal_aperture_35mm,
    vertical_aperture: Constants.vertical_aperture_35mm,
    f_stop: 8.0,
    xform: Xform.identity
};

/**
 * Creates a camera with the given focal length, horizontal aperture, vertical aperture, f-stop,
 * rotation (as quaternion), and translation (as vector).
 */
let create = (
    focal_length: float,
    horizontal_aperture: float,
    vertical_aperture: float,
    f_stop: float,
    rotate: Quat.t,
    translate: Vec.t) =>
{
    let translate_mat = Mat.translation(translate);
    let rotate_mat = Mat.rotation(rotate);
    let xform = Xform.create(translate_mat *# rotate_mat);

    {
        focal_length: focal_length,
        horizontal_aperture: horizontal_aperture,
        vertical_aperture: vertical_aperture,
        f_stop: f_stop,
        xform: xform
    }
};

/** Pupil radius of the given camera. */
let pupil_radius = (cam: t) => 0.5 *. (cam.focal_length /. cam.f_stop);

/** Aspect ratio (width.height) of given camera. */
let aspect_ratio = (cam: t) => cam.horizontal_aperture /. cam.vertical_aperture;

/** Maximum x-y coordinates on the focal plane of the camera's window. */
let window_max = (cam: t) =>
    (cam.horizontal_aperture /. (cam.focal_length *. 2.0),
     cam.vertical_aperture /. (cam.focal_length *. 2.0));

/**
 * Computes the ray starting at the viewpoint and extending through the given window position.
 * The window position is defined in normalized coordinates in [-1, 1] where (0, 0) is the
 * center, (-1, 1) is the lower-left, and (1, 1) is the upper-right.
 * Other documentation may refer to these types of coordinates as being in "lens space".
 */
let compute_ray = (cam: t, s: float, t: float) => {
    let window_max = window_max(cam);
    let origin = Vec.zero;
    let direction = Vec.normalized(
            Vec.xyz(fst(window_max) *. s, snd(window_max) *. t, -1.0));

    let world_origin = Xform.transform_vec(cam.xform, origin);
    let world_direction = Xform.transform_dir(cam.xform, direction);

    Ray.create(world_origin, world_direction)
};
