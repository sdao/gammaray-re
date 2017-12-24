/** Represents a single pixel in the film. */
type pixel_t = {
    accum: Vec.t,
    weight: float,
};

let zero_pixel = {accum: Vec.zero, weight: 0.0};

/**
 * Represents the accumulated image as a result of raytracing multiple iterations.
 */
type t = {
    width: int,
    height: int,
    pixels: array(array(pixel_t)),
};

/** Creates an empty film with given width and height. */
let create = (width: int, height: int) => {
    {width: width, height: height, pixels: Array.make_matrix(height, width, zero_pixel)}
};

/** Creates a test film with gradient color and uniform 1.0 weight. */
let create_test = (width: int, height: int) => {
    let film = create(width, height);
    for (y in 0 to (height - 1)) {
        let g = float_of_int(y) /. float_of_int(height - 1);
        for (x in 0 to (width - 1)) {
            let b = float_of_int(x) /. float_of_int(width - 1);
            film.pixels[y][x] = {accum: Vec.xyz(0.5, g, b), weight: 1.0};
        };
    };
    film
};
