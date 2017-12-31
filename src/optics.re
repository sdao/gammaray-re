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

let fresnel_schlick = (cos_theta: float, r0: Vec.t) => Vec.zero;

let fresnel_dielectric = (cos_theta_in: float, ior: float) => 0.0;

/** Computes the Fresnel reflection with the given cos(theta) of the ray angle. */
type fresnel_t = float => Vec.t;

let create_schlick_fresnel = (r0: Vec.t) => {
    (cos_theta: float) => {
        fresnel_schlick(cos_theta, r0)
    }
};

let create_disney_fresnel = (ior: float, color: Vec.t, specular_tint: float, metallic: float) => {
    let spec_color = Vec.lerp(Vec.one, Vec.tint(color), specular_tint);
    (cos_theta: float) => {
        let dielectric = spec_color *^. fresnel_dielectric(cos_theta, ior);
        let conductor = fresnel_schlick(cos_theta, color);
        Vec.lerp(dielectric, conductor, metallic)
    }
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

let create_ggx = (roughness: float, anisotropic: float) => {
    let ggx: microfacet_distribution_t = {
        pub d = (half: Vec.t) => 0.0;
        pub g = (i: Vec.t, o: Vec.t) => 0.0;
        pub sample_half = (i: Vec.t, rng: Sampling.rng_t) => Vec.zero;
        pub pdf = (i: Vec.t, half: Vec.t) => 0.0;
    };

    ggx
};

let create_gtr1 = (clearcoat_gloss: float) => {
    let gtr1: microfacet_distribution_t = {
        pub d = (half: Vec.t) => 0.0;
        pub g = (i: Vec.t, o: Vec.t) => 0.0;
        pub sample_half = (i: Vec.t, rng: Sampling.rng_t) => Vec.zero;
        pub pdf = (i: Vec.t, half: Vec.t) => 0.0;
    };

    gtr1
};