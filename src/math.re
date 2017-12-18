let is_close = (a: float, b: float, eps: float) => {
    abs_float(a -. b) < eps
};

let is_nearly_zero = (x: float) => {
    abs_float(x) < epsilon_float
};

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

let lerp = (a, b, k) => a +. (k *. (b -. a));

let is_finite = (x: float) =>
    switch (classify_float(x)) {
        | FP_normal => true
        | FP_subnormal => true
        | FP_zero => true
        | _ => false
    };
