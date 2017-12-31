let pi = 4.0 *. atan(1.0);

/** Whether two numbers are within the given epsilon of each other .*/
let is_close = (a: float, b: float, eps: float) => {
    abs_float(a -. b) < eps
};

/** Whether a number is within epsilon of zero. */
let is_nearly_zero = (x: float) => {
    abs_float(x) < epsilon_float
};

/** Whether a number is positive, within a small epsilon. */
let is_positive = (x: float) => {
    x > epsilon_float
};

/** Clamps a value between the lower limit a and the upper limit b. */
let clamp = (x, a, b) =>
    if (x < a) {
        a
    }
    else if (x > b) {
        b
    }
    else {
        x
    };

/** Clamps between 0.0 and 1.0. */
let clamp_unit = (x) => clamp(x, 0.0, 1.0);

/**
 * Linearly interpolates between two floats given a scale k; when k is 0.0, the value is a,
 * when k is 1.0, the value is b; otherwise the value is interpolated between a and b, with
 * extrapolation for k < 0.0 or k > 1.0.
 */
let lerp = (a, b, k) => a +. (k *. (b -. a));

/** Whether the float is non-infinity and non-NaN. */
let is_finite = (x: float) => {
    switch (classify_float(x)) {
        | FP_normal => true
        | FP_subnormal => true
        | FP_zero => true
        | _ => false
    }
};

/** For error correction/reduction. */
let gamma = (n: float) => {
    (n *. epsilon_float) /. (1.0 -. (n *. epsilon_float))
};

/** 2-d index into row-major 1-d array. */
let index = (row: int, col: int, width: int) => {
    row * width + col
};

/**
 * Computes the 1-dimensional Mitchell filter with b = 1/3 and c = 1/3 for a
 * scaled offset from the pixel center. The values are not normalized.
 *
 * Pharr and Humphreys suggest on p. 398 of PBR that values of b and c should
 * be chosen such that b + 2C = 1.
 * GPU Gems <http://http.developer.nvidia.com/GPUGems/gpugems_ch24.html>
 * suggests the above values of b = 1/3 and c = 1/3.
 *
 * @param x the scaled x-offset from the pixel center, -1 <= x <= 1
 */
let mitchell_filter1 = (x: float) => {
    let b = 1.0 /. 3.0;
    let c = 1.0 /. 3.0;

    let twox = abs_float(2.0 *. x); /* Convert to the range [0, 2]. */

    if (twox > 1.0) {
        ((~-.b -. 6.0 *. c) *. (twox *. twox *. twox)
        +. (6.0 *. b +. 30.0 *. c) *. (twox *. twox)
        +. (~-.12.0 *. b -. 48.0 *. c) *. twox
        +. (8.0 *. b +. 24.0 *. c)) *. (1.0 /. 6.0)
    }
    else {
        ((12.0 -. 9.0 *. b -. 6.0 *. c) *. (twox *. twox *. twox)
        +. (~-.18.0 +. 12.0 *. b +. 6.0 *. c) *. (twox *. twox)
        +. (6.0 -. 2.0 *. b)) *. (1.0 /. 6.0)
    }
};

/**
 * Evaluates a 2-dimensional Mitchell filter at a specified offset from the
 * pixel center by separating and computing the 1-dimensional Mitchell
 * filter for the x- and y- offsets.
 *
 * @param x     the x-offset from the pixel center, -width <= x <= width
 * @param y     the y-offset from the pixel center, -width <= x <= width
 * @param width the maximum x- or y- offset sampled from the pixel center
 *              (A recommended default width is 2.0)
 * @returns the value of the filter
 */
let mitchell_filter2 = (x: float, y: float, width: float) => {
    mitchell_filter1(x /. width) *. mitchell_filter1(y /. width)
};
