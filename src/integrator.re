type t = {
    .
    integrate: (Ray.t, Bvh.t, Sampling.rng_t) => (Vec.t)
};

let display_color: t = {
    pub integrate = (initial_ray: Ray.t, bvh: Bvh.t, _: Sampling.rng_t) => {
        switch (Bvh.intersect(bvh, initial_ray)) {
            | Hit(_: float, _: Prim.surface_properties_t, prim_index: int) => {
                Bvh.prim(bvh, prim_index)#material.display_color
            }
            | NoHit => {
                Vec.zero
            }
        }
    };
    pri _suppress_warning = this; /* XXX: is there a better way to suppress the unused warning? */
};
