/** Result of sampling a lobe. */
type sample_t = {
    result: Vec.t,
    outgoing: Vec.t,
    pdf: float,
};

let zero_sample = {
    result: Vec.zero,
    outgoing: Vec.zero,
    pdf: 0.0,
};

module Kind = {
    let none         = 0b00000000;
    /** PDF is non-delta-distributed. */
    let diffuse      = 0b00000001;
    /**
     * PDF is delta-distributed.
     * Lobes with this flag must have their f() and pdf() return zero. Only sample_f() should
     * return non-zero values; the pdf when sampling should be one.
     */
    let specular     = 0b00000010;
    let glossy       = 0b00000100;
    /** Out direction is same hemisphere as in direction. */
    let reflection   = 0b00001000;
    /** Out and in direction are different hemispheres. */
    let transmission = 0b00010000;
};

/** Abstract type for a material lobe. */
type t = {
    .

    /** Combination of flags specified in Lobe.Kind module. */
    kind: int,

    /** Computes the lobe result at the given incoming and outgoing directions. */
    f: (Vec.t, Vec.t, bool) => Vec.t,

    /**
     * Computes the pdf of the sample_f method taking the given incoming
     * direction and returning the given outgoing direction.
     */
    pdf: (Vec.t, Vec.t) => float,

    /**
     * Samples an outgoing direction (and the lobe result) given the incoming
     * direction, whether we are travelling in the camera-to-light direction,
     * and an RNG.
     */
    sample_f: (Vec.t, bool, Sampling.rng_t) => sample_t,
};
