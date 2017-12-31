/** Abstract type for lights (emissive surface behavior). */
type t = {
    .

    /**
     * Returns the light emitted along the given incoming (camera-to-light) ray
     * assuming the given surface properties of the geometry.
     */
    l_world: (Vec.t, SurfaceProperties.t) => Vec.t,
};

/** Creates a perfectly diffuse area light with the given emission color. */
let create_diffuse_area_light = (color: Vec.t) => {
    let light: t = {
        pri color = color;

        pub l_world = (i: Vec.t, surface_props: SurfaceProperties.t) => {
            /* Only emit light if the vector is on the same side as the normal. */
            if (Vec.dot(i, surface_props.geom_normal) > 0.0) {
                this#color
            }
            else {
                Vec.zero
            }
        };
    };

    light
};
