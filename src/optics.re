/**
 * Contains utilities for computing physical optics via various Fresnel and microfacet
 * models.
 */

/** Assuming that we're coming from air into the material. */
let fresnel_schlick_weight = (cos_theta: float) => {
    let x = Math.clamp_unit(1.0 -. cos_theta);
    x *. x *. x *. x *. x
};
