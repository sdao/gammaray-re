type pixel_t = {
    accum: Vec.t,
    weight: float,
};

let zero_pixel = {accum: Vec.zero, weight: 0.0};

type t = {
    width: int,
    height: int,
    pixels: array(array(pixel_t)),
};

let create = (width: int, height: int) => {
    {width: width, height: height, pixels: Array.make_matrix(height, width, zero_pixel)}
};

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
