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
    (color: Vec.t, roughness: float, sheen_frac: float, sheen_tint: float, diffuse_weight: float) =>
{
    let disney_diffuse_refl: t = {
        pri diffuse_color = color *^. diffuse_weight;
        pri sheen_color = Vec.lerp(Vec.one, Vec.tint(color), sheen_tint) *^.
                (sheen_frac *. diffuse_weight);
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

let create_standard_microfacet_refl =
    (dist: Optics.microfacet_distribution_t, fr: Optics.fresnel_t, color: Vec.t) =>
{
    let standard_microfacet_refl: t = {
        pri microfacet = dist;
        pri fresnel = fr;
        pri color = color;

        pub kind = Kind.glossy lor Kind.reflection;

        pub f = (i: Vec.t, o: Vec.t, _: bool) => {
            let cos_theta_in = Vec.abs_cos_theta(i);
            let cos_theta_out = Vec.abs_cos_theta(o);
            let half_unnorm = i +^ o;
            if (Vec.is_exactly_zero(half_unnorm) || cos_theta_in == 0.0 || cos_theta_out == 0.0) {
                Vec.zero
            }
            else {
                let half = Vec.normalized(half_unnorm);
                let fresnel = this#fresnel(Vec.dot(o, half));
                let d = this#microfacet#d(half);
                let g = this#microfacet#g(i, o);
                (this#color *^ fresnel) *^. (d *. g /. (1.0 *. cos_theta_out *. cos_theta_in))
            }
        };
        pub pdf = (i: Vec.t, o: Vec.t) => {
            if (Vec.is_local_same_hemisphere(i, o)) {
                let half = Vec.normalized(i +^ o);
                this#microfacet#pdf(i, half) /. (4.0 *. Vec.dot(i, half))
            }
            else {
                0.0
            }
        };
        pub sample_f = (i: Vec.t, camera_to_light: bool, rng: Sampling.rng_t) => {
            /* Sample microfacet orientation (half) and reflected direction (o). */
            if (i.z == 0.0) {
                zero_sample
            }
            else {
                let half = this#microfacet#sample_half(i, rng);
                let o = Vec.reflect(i, half);
                if (!Vec.is_local_same_hemisphere(i, o)) {
                    zero_sample
                }
                else {
                    /* Compute PDF of outoing vector for microfacet reflection. */
                    let result = this#f(i, o, camera_to_light);
                    let pdf = this#microfacet#pdf(i, half) /. (4.0 *. Vec.dot(i, half));
                    {
                        result: result,
                        outgoing: o,
                        pdf: pdf
                    }
                }
            }
        };
    };

    standard_microfacet_refl
};

let create_disney_specular_refl_aniso =
    (color: Vec.t, roughness: float, anisotropic: float, ior: float, specular_tint: float,
    metallic: float) =>
{
    /* Note: the color will be computed by the Disney fresnel function, so we just set it to
     * white on the lobe itself. */
    let ior_adjusted = max(ior, 1.01);
    create_standard_microfacet_refl(
            Optics.create_ggx(roughness, anisotropic),
            Optics.create_disney_fresnel(ior_adjusted, color, specular_tint, metallic),
            color)
};

let create_disney_specular_refl =
    (color: Vec.t, roughness: float, ior: float, specular_tint: float, metallic: float) =>
{
    create_disney_specular_refl_aniso(color, roughness, ior, specular_tint, metallic)
};

let create_disney_clearcoat_refl = (clearcoat: float, clearcoat_gloss: float) => {
    /* Note: Disney BRDF: (ior = 1.0 -> F0 = 0.04).
     * Disney also scales the clearcoat amount by 0.25. */
    create_standard_microfacet_refl(
            Optics.create_gtr1(clearcoat_gloss),
            Optics.create_schlick_fresnel(Vec.from_scalar(0.04)),
            Vec.from_scalar(0.25 *. clearcoat))
};

/* This implementation is derived from the MicrofacetTransmission in PBRT 3e. */
let create_disney_specular_trans_aniso =
    (color: Vec.t, roughness: float, anisotropic: float, ior: float) =>
{
    let ior_adjusted = max(ior, 1.01);
    let disney_specular_trans: t = {
        pri microfacet = Optics.create_ggx(roughness, anisotropic);
        pri fresnel = Optics.create_dielectric_fresnel(ior_adjusted);
        pri ior = ior_adjusted;
        pri spec_color = color;

        pub kind = Kind.glossy lor Kind.transmission;

        pub f = (i: Vec.t, o: Vec.t, camera_to_light: bool) => {
            /* This is defined for transmission only. */
            if (Vec.is_local_same_hemisphere(i, o)) {
                Vec.zero
            }
            else {
                let cos_theta_in = Vec.cos_theta(i);
                let cos_theta_out = Vec.cos_theta(o);
                if (cos_theta_in == 0.0 || cos_theta_out == 0.0) {
                    Vec.zero
                }
                else {
                    let eta = if (cos_theta_in > 0.0) {
                        /* Entering */
                        this#ior
                    }
                    else {
                        /* Exiting */
                        1.0 /. this#ior
                    };

                    let half_unnorm = i +^ (o *^. eta);
                    let half = if (half_unnorm.z > 0.0) {
                        Vec.normalized(half_unnorm)
                    }
                    else {
                        ~-^Vec.normalized(half_unnorm)
                    };

                    assert(Vec.is_finite(i));
                    assert(Vec.is_finite(o));
                    assert(Vec.is_finite(half));

                    let fresnel = this#fresnel(Vec.dot(o, half));
                    let d = this#microfacet#d(half);
                    let g = this#microfacet#g(i, o);

                    let sqrt_denom = Vec.dot(i, half) +. eta *. Vec.dot(o, half);
                    let factor = if (camera_to_light) { 1.0 } else { eta };
                    let fresnel_inverse = Vec.one -^ fresnel; /* Amount transmitted! */

                    (this#spec_color *^ fresnel_inverse) *^.
                            abs_float(
                                d *. g *. factor *. factor *. abs_float(Vec.dot(o, half)) *.
                                abs_float(Vec.dot(i, half)) /.
                                (cos_theta_out *. cos_theta_in *. sqrt_denom *. sqrt_denom))
                }
            }
        };
        pub pdf = (i: Vec.t, o: Vec.t) => {
            if (Vec.is_local_same_hemisphere(i, o)) {
                0.0
            }
            else {
                let eta = if (Vec.cos_theta(i) > 0.0) {
                    /* Entering. */
                    this#ior
                }
                else {
                    /* Exiting. */
                    1.0 /. this#ior
                };

                /* Compute half from i and o for microfacet transmission. */
                let half_unnorm = i +^ (o *^. eta);
                let half = if (half_unnorm.z > 0.0) {
                    half_unnorm
                }
                else {
                    ~-^Vec.normalized(half_unnorm)
                };

                /* Compute change of variables for microfacet transmission. */
                let sqrt_denom = Vec.dot(i, half) +. eta *. Vec.dot(o, half);
                let dwh_dwi = abs_float((eta *. eta *. Vec.dot(o, half)) /.
                        (sqrt_denom *. sqrt_denom));
                this#microfacet#pdf(i, half) *. dwh_dwi
            }
        };
        pub sample_f = (i: Vec.t, camera_to_light: bool, rng: Sampling.rng_t) => {
            /* Sample microfacet orientation (half) and reflected direction (o). */
            if (i.z == 0.0) {
                zero_sample
            }
            else {
                let half = this#microfacet#sample_half(i, rng);
                let eta = if (Vec.cos_theta(i) > 0.0) {
                    /* Entering. */
                    1.0 /. this#ior
                }
                else {
                    /* Exiting. */
                    this#ior
                };

                let o = Vec.refract(i, half, eta);
                assert(Vec.is_finite(o));

                if (Vec.is_exactly_zero(o)) {
                    zero_sample
                }
                else {
                    /* Compute PDF of outoing vector for microfacet transmission. */
                    let result = this#f(i, o, camera_to_light);
                    let pdf = this#pdf(i, o);
                    assert(Vec.is_finite(result));

                    {
                        result: result,
                        outgoing: o,
                        pdf: pdf
                    }
                }
            }
        };
    };

    disney_specular_trans
};

let create_disney_specular_trans = (color: Vec.t, roughness: float, ior: float) => {
    create_disney_specular_trans_aniso(color, roughness, 0.0, ior)
};
