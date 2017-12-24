let pi = 4.0 *. atan(1.0);

/** Whether two numbers are within the given epsilon of each other .*/
let is_close = (a: float, b: float, eps: float) => {
    abs_float(a -. b) < eps
};

/** Whether a number is within epsilon of zero. */
let is_nearly_zero = (x: float) => {
    abs_float(x) < epsilon_float
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
