open Vec.Ops;

/**
 * Represents a single pixel in the film. 
 * Note: all fields are mutable because this should be reused for performance purposes.
 */
type pixel_t = {
    mutable accum: Vec.t,
    mutable weight: float,
};

let create_pixel = () => {accum: Vec.zero, weight: 0.0};

/**
 * Represents a sample in flight before it is committed into the film.
 * Note: all fields are mutable because this should be reused for performance purposes.
 */
type sample_t = {
    mutable color: Vec.t, /* Report sample here. */
    mutable s: float, /* Column of the sample, in lens space. May extend past [-1, 1] b/c filter. */
    mutable t: float /* Row of the sample, in lens space. May extend past [-1, 1] b/c filter. */
};

let create_sample = () => {color: Vec.zero, s: 0.0, t: 0.0};

/**
 * Represents the accumulated image as a result of raytracing multiple iterations.
 */
type t = {
    width: int,
    height: int,
    pixels: array(array(pixel_t)),
};

let _filter_width: float = 2.0;

/** Creates an empty film with given width and height. */
let create = (width: int, height: int) => {
    let tmp = create_pixel();
    let pixels = Array.make_matrix(height, width, tmp);
    for (i in 0 to height - 1) {
        for (j in 0 to width - 1) {
            pixels[i][j] = create_pixel();
        };
    };

    {width: width, height: height, pixels: pixels}
};

/** Creates a test film with gradient color and uniform 1.0 weight. */
let create_test = (width: int, height: int) => {
    let film = create(width, height);
    for (y in 0 to (height - 1)) {
        let g = float_of_int(y) /. float_of_int(height - 1);
        for (x in 0 to (width - 1)) {
            let b = float_of_int(x) /. float_of_int(width - 1);
            film.pixels[y][x].accum = Vec.xyz(0.5, g, b);
            film.pixels[y][x].weight = 1.0;
        };
    };
    film
};

let compute_sample_points = (film: t, rng: Sampling.rng_t, samples: ref(array(sample_t))) => {
    if (Array.length(samples^) != film.width * film.height) {
        let tmp = create_sample();
        samples := Array.make(film.width * film.height, tmp);
        for (i in 0 to (film.width * film.height) - 1) {
            samples^[i] = create_sample();
        };
    };

    let (widthf, heightf) = (float_of_int(film.width), float_of_int(film.height));
    for (row_discr in 0 to film.height - 1) {
        let row_cont = 0.5 +. float_of_int(row_discr);
        for (col_discr in 0 to film.width - 1) {
            let col_cont = 0.5 +. float_of_int(col_discr);

            let row_cont_jitter = row_cont +.
                    Sampling.next_float_range(rng, ~-._filter_width, _filter_width);
            let col_cont_jitter = col_cont +.
                    Sampling.next_float_range(rng, ~-._filter_width, _filter_width);

            let s = Math.lerp(-1.0, 1.0, col_cont_jitter /. widthf);
            let t = Math.lerp(-1.0, 1.0, row_cont_jitter /. heightf);

            let i = Math.index(row_discr, col_discr, film.width);
            samples^[i].s = s;
            samples^[i].t = t;
        };
    };
};

let report_samples = (film: t, samples: ref(array(sample_t))) => {
    let (widthf, heightf) = (float_of_int(film.width), float_of_int(film.height));
    let (last_col, last_row) = (film.width - 1, film.height - 1);
    Array.iter((sample: sample_t) => {
        let col_cont = Math.lerp(0.0, widthf, 0.5 *. (sample.s +. 1.0));
        let row_cont = Math.lerp(0.0, heightf, 0.5 *. (sample.t +. 1.0));
        let col_discr = col_cont -. 0.5;
        let row_discr = row_cont -. 0.5;

        let min_col = Math.clamp(
                int_of_float(ceil(col_discr -. _filter_width)), 0, last_col);
        let max_col = Math.clamp(
                int_of_float(floor(col_discr +. _filter_width)), 0, last_col);
        let min_row = Math.clamp(
                int_of_float(ceil(row_discr -. _filter_width)), 0, last_row);
        let max_row = Math.clamp(
                int_of_float(floor(row_discr +. _filter_width)), 0, last_row);
        assert(max_row < film.height);
        assert(max_col < film.width);

        for (y in min_row to max_row) {
            for (x in min_col to max_col) {
                let pixel = film.pixels[y][x];
                let weight = Math.mitchell_filter2(
                        float_of_int(x) -. col_discr,
                        float_of_int(y) -. row_discr,
                        _filter_width);

                film.pixels[y][x].accum = pixel.accum +^ (sample.color *^ Vec.from_scalar(weight));
                film.pixels[y][x].weight = pixel.weight +. weight;
            };
        };
    }, samples^);
};
