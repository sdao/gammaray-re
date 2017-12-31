open Vec.Ops;

/** Lobe-based representation of a material. */
type t = {
    display_color: Vec.t,
    light: option(Light.t),
    lobes: array(Lobe.t),
};

let create_debug = (display_color: Vec.t) => {
    {
        display_color: display_color,
        light: None,
        lobes: [||]
    }
};

let create_diffuse_light = (incandescence: Vec.t) => {
    {
        display_color: incandescence,
        light: Some(Light.create_diffuse_area_light(incandescence)),
        lobes: [||]
    }
};

let create_disney = (
    ~base_color=Vec.one,
    ~roughness=0.5,
    ~anisotropic=0.0,
    ~ior=1.5,
    ~metallic=0.0,
    ~specular_trans=0.0,
    ~specular_tint=0.0,
    ~sheen=0.0,
    ~sheen_tint=0.5,
    ~clearcoat=0.0,
    ~clearcoat_gloss=0.1,
    ()
) =>
{
    /* Combo of three models: diffuse_weight + trans_weight + metallic = 1.0 */
    let diffuse_weight = (1.0 -. metallic) *. (1.0 -. specular_trans);
    let trans_weight = (1.0 -. metallic) *. specular_trans;
    let lobes_list = ref([]);
    
    /* Diffuse, retro-reflection, and sheen */
    if (diffuse_weight > 0.0) {
        lobes_list := [Lobe.create_disney_diffuse_refl(
                base_color, roughness, sheen, sheen_tint, diffuse_weight), ...lobes_list^];
    };

    /* Specular reflection */
    if (ior > 1.0) {
        lobes_list := [Lobe.create_disney_specular_refl_aniso(
                base_color, roughness, anisotropic, ior, specular_tint, metallic), ...lobes_list^];
    };

    /* Clearcoat (second specular lobe) */
    if (clearcoat > 0.0) {
        lobes_list := [Lobe.create_disney_clearcoat_refl(
                clearcoat, clearcoat_gloss), ...lobes_list^];
    };

    /* Specular transmission */
    if (trans_weight > 0.0) {
        /* PBRT suggests that we take scale up the base color to its sqrt
         * for art-direction purposes; it makes it so that light that enters and exits
         * will have the base color instead of being darker. */
        let specular_trans_color = Vec.comp_sqrt(base_color) *^. trans_weight;
        lobes_list := [Lobe.create_disney_specular_trans_aniso(
                specular_trans_color, roughness, anisotropic, ior), ...lobes_list^];
    };

    {
        display_color: base_color,
        light: None,
        lobes: Array.of_list(List.rev(lobes_list^))
    }
};
