open Vec.Ops;

/** Lobe-based representation of a material. */
type t = {
    display_color: Vec.t,
    light: option(Light.t),
    lobes: array(Lobe.t),
};

let empty = {
    display_color: Vec.zero,
    light: None,
    lobes: [||],
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

type sample_t = {
    emission: Vec.t,
    radiance: Vec.t,
    outgoing: Vec.t,
    pdf: float,
    kind: int,
};

/**
 * Samples material properties and behavior with the given incoming world-space vector and
 * the specified surface properties on the geometry surface.
 * See PBRT 3e, page 832.
 * Args:
 *   incoming_world should face away from the intersection point.
 *   surface_props should be in world-space.
 */
let sample_world = (material: t,
    incoming_world: Vec.t,
    surface_props: SurfaceProperties.t,
    camera_to_light: bool,
    rng: Sampling.rng_t) =>
{
    assert(Math.is_close(Vec.magnitude(surface_props.normal), 1.0, 1e-3));
    assert(Math.is_close(Vec.magnitude(surface_props.tangent), 1.0, 1e-3));
    assert(Math.is_close(Vec.magnitude(surface_props.binormal), 1.0, 1e-3));

    /* Convert from world-space to local space. */
    let incoming_local = Vec.world_to_local(incoming_world,
            surface_props.tangent, surface_props.binormal, surface_props.normal);
    assert(Vec.is_finite(incoming_world));
    assert(Vec.is_finite(incoming_local));

    /* Calculate emission. This doesn't depend on reflecting an outgoing ray.
     * Note that lighting isn't computed using the shading space (since it doesn't depend on
     * shading normals/tangents/binormals). */
    let emission = switch material.light {
        | Some(light) => light#l_world(incoming_world, surface_props)
        | None => Vec.zero
    };

    if (Array.length(material.lobes) == 0) {
        {
            emission: emission,
            radiance: Vec.zero,
            outgoing: Vec.zero,
            pdf: 1.0,
            kind: Lobe.Kind.none,
        }
    }
    else {
        /* Choose a lobe and sample it. */
        let r = Sampling.next_int_range(rng, Array.length(material.lobes));
        let lobe = material.lobes[r];
        let sample = lobe#sample_f(incoming_local, camera_to_light, rng);

        let outgoing_world = Vec.local_to_world(sample.outgoing,
                surface_props.tangent, surface_props.binormal, surface_props.normal);
        let radiance = ref(sample.result);
        let pdf = ref(sample.pdf);

        /* Compute overall PDF over all lobes (if the chosen lobe wasn't specular). */
        if ((lobe#kind land Lobe.Kind.specular) == 0) {
            Array.iteri((idx, lobe) => {
                if (idx != r) {
                    pdf := pdf^ +. lobe#pdf(incoming_local, sample.outgoing);
                };
            }, material.lobes);
        };
        pdf := pdf^ /. float_of_int(Array.length(material.lobes));

        /* Compute overall BSDF over all lobes (if the chosen lobe wasn't specular). */
        if ((lobe#kind land Lobe.Kind.specular) == 0) {
            /* Whether we're evalauting BTDFs or BRDFs should actually be based on geom normal,
             * not shading normal. */
            let reflect = (Vec.dot(incoming_world, surface_props.geom_normal) *.
                    Vec.dot(outgoing_world, surface_props.geom_normal)) > 0.0;
            Array.iteri((idx, lobe) => {
                if (idx != r &&
                        ((reflect && (lobe#kind land Lobe.Kind.reflection) != 0) ||
                        (!reflect && (lobe#kind land Lobe.Kind.transmission) != 0))) {
                    radiance := radiance^ +^
                            lobe#f(incoming_local, sample.outgoing, camera_to_light);
                };
            }, material.lobes);
        };

        /* Normalize; if pdf is zero, then make the radiance black to be safe. */
        if (pdf^ == 0.0) {
            {
                emission: emission,
                radiance: Vec.zero,
                outgoing: outgoing_world,
                pdf: 1.0,
                kind: Lobe.Kind.none,
            }
        }
        else {
            assert(Vec.is_finite(radiance^));
            assert(Math.is_finite(pdf^));
            assert(pdf^ > 0.0);

            {
                emission: emission,
                radiance: radiance^,
                outgoing: outgoing_world,
                pdf: pdf^,
                kind: lobe#kind,
            }
        }
    }
};
