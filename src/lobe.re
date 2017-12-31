open Vec.Ops;

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

    /**
     * Computes the lobe result at the given incoming and outgoing directions, taking into
     * account whether we are travelling in the camera-to-light direction.
     */
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

/** Implementation of t.pdf using cosine sampling of the hemisphere. */
let cosine_pdf = (i: Vec.t, o: Vec.t) => {
    if (Vec.is_local_same_hemisphere(i, o)) {
        Sampling.CosineSampleHemisphere.pdf(o)
    }
    else {
        0.0
    }
};

/** Implementation of t.sample_f using cosine sampling of the hemisphere. */
let cosine_sample_f = (lobe: t, i: Vec.t, camera_to_light: bool, rng: Sampling.rng_t) => {
    /* Take a sample direction on the same side of the normal as the incoming direction. */
    let o = Sampling.CosineSampleHemisphere.sample(rng, i.z < 0.0);
    let result = lobe#f(i, o, camera_to_light);
    let pdf = lobe#pdf(i, o);

    {
        result: result,
        outgoing: o,
        pdf: pdf
    }
};

/** Implements diffuse, retro-reflection, and sheen for the Disney BRDF. */
let create_disney_diffuse_refl =
    (color: Vec.t, roughness: float, sheen: float, sheen_tint: float, diffuse_weight: float) =>
{
    let disney_diffuse_refl = {
        pri diffuse_color = color *^. diffuse_weight;
        pri sheen_color = Vec.lerp(Vec.one, Vec.tint(color), sheen_tint) *^.
                (sheen *. diffuse_weight);
        pri roughness = roughness;

        pub kind = Kind.diffuse lor Kind.reflection;

        pub f = (i: Vec.t, o: Vec.t, _: bool) => {
            let f_in = Optics.fresnel_schlick_weight(Vec.abs_cos_theta(i));
            let f_out = Optics.fresnel_schlick_weight(Vec.abs_cos_theta(o));
            let diffuse = this#diffuse_color *^.
                    ((1.0 /. Math.pi) *. (1.0 -. 0.5 *. f_in) *. (1.0 -. 0.5 *. f_out));

            let half_unnorm = i +^ o;
            if (Vec.is_exactly_zero(half_unnorm)) {
                /** Retro-reflection and sheen can't be computed. */
                diffuse
            }
            else {
                let half = Vec.normalized(half_unnorm);
                let cos_theta_d = Vec.dot(o, half); /** Note: could have used i here also. */
                let r_r = 2.0 *. this#roughness *. cos_theta_d *. cos_theta_d;

                let retro = this#diffuse_color *^.
                        ((1.0 /. Math.pi) *.
                            r_r *.
                            (f_out +. f_in +. f_out *. f_in *. (r_r -. 1.0)));
                let sheen = this#sheen_color *^. Optics.fresnel_schlick_weight(cos_theta_d);
                
                diffuse +^ retro +^ sheen
            }
        };
        pub pdf = (i: Vec.t, o: Vec.t) => cosine_pdf(i, o);
        pub sample_f = (i: Vec.t, camera_to_light: bool, rng: Sampling.rng_t) => {
            cosine_sample_f(this, i, camera_to_light, rng)
        };
    };

    disney_diffuse_refl
};
