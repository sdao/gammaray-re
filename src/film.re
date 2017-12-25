open Vec.Ops;

/** Represents a single pixel in the film. */
type pixel_t = {
    mutable accum: Vec.t,
    mutable weight: float,
};

let zero_pixel = {accum: Vec.zero, weight: 0.0};

/** Represents a sample in flight before it is committed into the film. */
type sample_t = {
    mutable color: Vec.t,
    s: float, /* Column of the sample, in lens space. May extend beyond [-1, 1] due to filtering. */
    t: float /* Row of the sample, in lens space. May extend beyond [-1, 1] due to filtering. */
};

let zero_sample = {color: Vec.zero, s: 0.0, t: 0.0};

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

let compute_sample_points = (film: t, samples: ref(array(sample_t))) => {
    /* Warning: this function accesses the global RNG! */
    let filter_jitter = () => {
        Random.float(2.0 *. _filter_width) -. _filter_width
    };

    if (Array.length(samples^) != film.width * film.height) {
        samples := Array.make(film.width * film.height, zero_sample);
    };

    let (widthf, heightf) = (float_of_int(film.width), float_of_int(film.height));
    for (row_discr in 0 to film.height - 1) {
        let row_cont = 0.5 +. float_of_int(row_discr);
        for (col_discr in 0 to film.width - 1) {
            let col_cont = 0.5 +. float_of_int(col_discr);

            let row_cont_jitter = row_cont +. filter_jitter();
            let col_cont_jitter = col_cont +. filter_jitter();

            let s = Math.lerp(-1.0, 1.0, col_cont_jitter /. widthf);
            let t = Math.lerp(-1.0, 1.0, row_cont_jitter /. heightf);

            let i = Math.index(row_discr, col_discr, film.width);
            samples^[i] = {color:Vec.zero, s: s, t: t};
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

                pixel.accum = pixel.accum +^ (sample.color *^ Vec.from_scalar(weight));
                pixel.weight =  pixel.weight +. weight;
            };
        };
    }, samples^);
};
