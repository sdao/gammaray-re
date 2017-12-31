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

/** XXX unimplemented */
