/**
 * Contains utilities for computing physical optics via various Fresnel and microfacet
 * models.
 */
open Vec.Ops;

/** Assuming that we're coming from air into the material. */
let fresnel_schlick_weight = (cos_theta: float) => {
    let x = Math.clamp_unit(1.0 -. cos_theta);
    x *. x *. x *. x *. x
};

let fresnel_schlick = (cos_theta: float, r0: Vec.t) => {
    Vec.lerp(r0, Vec.one, fresnel_schlick_weight(cos_theta))
};

let fresnel_dielectric = (cos_theta_in: float, ior: float) => {
    /** Potentially swap indices of refraction. */
    let entering = cos_theta_in > 0.0;
    let (eta_i, eta_t, cos_theta_in_clamped) = if (entering) {
        (1.0, ior, Math.clamp_unit(cos_theta_in))
    }
    else {
        (ior, 1.0, Math.clamp_unit(~-.cos_theta_in))
    };

    /* Compute cos_theta_trans using Snell's law. */
    let sin_theta_in = sqrt(max(0.0, 1.0 -. cos_theta_in_clamped *. cos_theta_in_clamped));
    let sin_theta_trans = eta_i /. eta_t *. sin_theta_in;

    /* Handle total internal reflection. */
    if (sin_theta_trans >= 1.0) {
        1.0
    }
    else {
        let cos_theta_trans = sqrt(max(0.0, 1.0 -. sin_theta_trans *. sin_theta_trans));
        let r_parl = ((eta_t *. cos_theta_in_clamped) -. (eta_i *. cos_theta_trans)) /.
                     ((eta_t *. cos_theta_in_clamped) +. (eta_i *. cos_theta_trans));
        let r_perp = ((eta_i *. cos_theta_in_clamped) -. (eta_t *. cos_theta_trans)) /.
                     ((eta_i *. cos_theta_in_clamped) +. (eta_t *. cos_theta_trans));
        (r_parl *. r_parl +. r_perp *. r_perp) /. 2.0
    }
};

/** Computes the Fresnel reflection with the given cos(theta) of the ray angle. */
type fresnel_t = float => Vec.t;

let create_schlick_fresnel = (r0: Vec.t) => {
    let fr: fresnel_t = (cos_theta: float) => {
        fresnel_schlick(cos_theta, r0)
    };
    fr
};

let create_dielectric_fresnel = (ior: float) => {
    let fr: fresnel_t = (cos_theta: float) => {
        Vec.from_scalar(fresnel_dielectric(cos_theta, ior))
    };
    fr
};

let create_disney_fresnel = (ior: float, color: Vec.t, specular_tint: float, metallic: float) => {
    let spec_color = Vec.lerp(Vec.one, Vec.tint(color), specular_tint);
    let fr: fresnel_t = (cos_theta: float) => {
        let dielectric = spec_color *^. fresnel_dielectric(cos_theta, ior);
        let conductor = fresnel_schlick(cos_theta, color);
        Vec.lerp(dielectric, conductor, metallic)
    };
    fr
};

type microfacet_distribution_t = {
    .

    /** D(half) */
    d: Vec.t => float,
    /** G(i, o) */
    g: (Vec.t, Vec.t) => float,
    /** Samples a half-angle given i, rng. */
    sample_half: (Vec.t, Sampling.rng_t) => Vec.t,
    /** PDF(i, half) */
    pdf: (Vec.t, Vec.t) => float
};

/**
 * This is based off the TrowbridgeReitzDistribution in PBRT 3e and the
 * Disney BRDF shader source at:
 * https://github.com/wdas/brdf/blob/master/src/brdfs/disney.brdf
 */
let create_ggx = (roughness: float, anisotropic: float) => {
    let aspect = sqrt(1.0 -. anisotropic *. 0.9);

    let ggx: microfacet_distribution_t = {
        pri ax = max(0.001, roughness *. roughness /. aspect);
        pri ay = max(0.001, roughness *. roughness *. aspect);

        pri lambda = (v: Vec.t) => {
            let abs_tan_theta = abs_float(Vec.tan_theta(v));
            if (Math.is_finite(abs_tan_theta)) {
                let alpha = sqrt(
                        Vec.cos2_phi(v) *. (this#ax *. this#ax) +.
                        Vec.sin2_phi(v) *. (this#ay *. this#ay));
                let alpha2_tan2_theta = (alpha *. abs_tan_theta) *. (alpha *. abs_tan_theta);
                (~-.1.0 +. sqrt(1.0 +. alpha2_tan2_theta)) *. 0.5
            }
            else {
                0.0
            }
        };
        pri g1 = (v: Vec.t) => {
            1.0 /. (1.0 +. this#lambda(v))
        };
        pri sample11 = (cos_theta: float, u1: float, u2: float) => {
            /* Special case (normal incidence). */
            if (cos_theta > 0.9999) {
                let r = sqrt(u1 /. (1.0 -. u1));
                let phi = 2.0 *. Math.pi *. u2;
                (r *. cos(phi), r *. sin(phi))
            }
            else {
                let sin_theta = sqrt(max(0.0, 1.0 -. cos_theta *. cos_theta));
                let tan_theta = sin_theta /. cos_theta;
                let g1 = 2.0 /. (1.0 +. sqrt(1.0 +. tan_theta *. tan_theta));

                /* Sample x-slope. */
                let a = 2.0 *. u1 /. g1 -. 1.0;
                let tmp = min(1.0 /. (a *. a -. 1.0), 1e10);
                let b = tan_theta;
                let d = sqrt(max(b *. b *. tmp *. tmp -. (a *. a -. b *. b) *. tmp, 0.0));
                let slope_x_1 = b *. tmp -. d;
                let slope_x_2 = b *. tmp +. d;
                let slope_x = if (a < 0.0 || slope_x_2 > 1.0 /. tan_theta) {
                    slope_x_1
                }
                else {
                    slope_x_2
                };
                assert(Math.is_finite(slope_x));

                /* Sample y-slope. */
                let (s, u) = if (u2 > 0.5) {
                    (1.0, 2.0 *. (u2 -. 0.5))
                }
                else {
                    (~-.1.0, 2.0 *. (0.5 -. u2))
                };
                let z =
                        (u *. (u *. (u *. 0.27385 -. 0.73369) +. 0.46341)) /.
                        (u *. (u *. (u *. 0.093073 +. 0.309420) -. 1.000000) +. 0.597999);
                let slope_y = s *. z *. sqrt(1.0 +. slope_x *. slope_x);
                assert(Math.is_finite(slope_y));

                (slope_x, slope_y)
            }
        };

        pub d = (half: Vec.t) => {
            let tan2_theta = Vec.tan2_theta(half);
            if (Math.is_finite(tan2_theta)) {
                let cos4_theta = Vec.cos2_theta(half) *. Vec.cos2_theta(half);
                let e = (Vec.cos2_phi(half) /. (this#ax *. this#ax) +.
                        Vec.sin2_phi(half) /. (this#ay *. this#ay))
                        *. tan2_theta;
                1.0 /. (Math.pi *. this#ax *. this#ay *. cos4_theta *. (1.0 +. e) *. (1.0 +. e))
            } else {
                0.0
            }
        };
        pub g = (i: Vec.t, o: Vec.t) => 1.0 /. (1.0 +. this#lambda(i) +. this#lambda(o));
        pub sample_half = (i: Vec.t, rng: Sampling.rng_t) => {
            /* Flip coordinates so that we're on the same side as the normal. */
            let flip = i.z < 0.0;
            let i_flipped = if (flip) { ~-^i } else { i };

            /* 1. Stretch incoming vector. */
            let i_stretched = Vec.normalized(Vec.xyz(
                    this#ax *. i_flipped.x, this#ay *. i_flipped.y, i_flipped.z));

            /* 2. Simulate P22. */
            let cos_theta = Vec.cos_theta(i_stretched);
            let u1 = Sampling.next_float_unit(rng);
            let u2 = Sampling.next_float_unit(rng);
            let (slope_x, slope_y) = this#sample11(cos_theta, u1, u2);

            /* 3. Rotate and 4. Unstretch. */
            let cos_phi = Vec.cos_phi(i_stretched);
            let sin_phi = Vec.sin_phi(i_stretched);
            let slope_x_rot = this#ax *. (cos_phi *. slope_x -. sin_phi *. slope_y);
            let slope_y_rot = this#ay *. (sin_phi *. slope_x +. cos_phi *. slope_y);

            /* 5. Compute normal. */
            let half = Vec.normalized(Vec.xyz(~-.slope_x_rot, ~-.slope_y_rot, 1.0));

            /* Flip coordinates back if necessary. */
            if (flip) { ~-^half } else { half }
        };
        pub pdf = (i: Vec.t, half: Vec.t) => {
            let cos_theta = Vec.cos_theta(i);
            if (cos_theta == 0.0) {
                0.0
            }
            else {
                this#d(half) *. this#g1(i) *. abs_float(Vec.dot(i, half)) /. abs_float(cos_theta)
            }
        };
    };

    ggx
};

let create_gtr1 = (clearcoat_gloss: float) => {
    let gtr1: microfacet_distribution_t = {
        pri alpha = Math.lerp(0.1, 0.001, clearcoat_gloss);

        pri lambda = (v: Vec.t) => {
            let alpha_g = 0.25; /* According to Disney's BRDF, the Gr term uses alpha=0.25. */
            let cos_theta = Vec.abs_cos_theta(v);

            let alpha2 = alpha_g *. alpha_g;
            let cos_theta2 = cos_theta *. cos_theta;

            1.0 /. (cos_theta +. sqrt(alpha2 +. cos_theta2 -. (alpha2 *. cos_theta2)))
        };

        pub d = (half: Vec.t) => {
            let alpha2 = this#alpha *. this#alpha;
            let cos_theta = Vec.abs_cos_theta(half);
            (alpha2 -. 1.0) /.
                    (Math.pi *.
                    log(alpha2) *.
                    (1.0 +. (alpha2 -. 1.0) *. cos_theta *. cos_theta))
        };
        pub g = (i: Vec.t, o: Vec.t) => 1.0 /. (1.0 +. this#lambda(i) +. this#lambda(o));
        pub sample_half = (i: Vec.t, rng: Sampling.rng_t) => {
            let alpha2 = this#alpha *. this#alpha;
            let phi = 2.0 *. Math.pi *. Sampling.next_float_unit(rng);
            let cos_theta = sqrt(Math.clamp_unit(
                    (1.0 -.
                    (alpha2 ** (1.0 -. Sampling.next_float_unit(rng))) /. (1.0 -. alpha2))));
            let h = Vec.from_spherical(cos_theta, phi);
            if (Vec.is_local_same_hemisphere(h, i)) {
                h
            }
            else {
                ~-^h
            }
        };
        pub pdf = (_: Vec.t, half: Vec.t) => {
            /* Sampling exactly follows GTR1, so the pdf is the same as the value. */
            this#d(half)
        };
    };

    gtr1
};
