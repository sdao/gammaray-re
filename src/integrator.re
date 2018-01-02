open Vec.Ops;

/** Signature for an integration function. */
type t = (Ray.t, Bvh.t, Sampling.rng_t) => (Vec.t);

let constant_color: t = (_: Ray.t, _: Bvh.t, _: Sampling.rng_t) => {
    Vec.xyz(1.0, 0.0, 1.0)
};

let display_color: t = (initial_ray: Ray.t, bvh: Bvh.t, _: Sampling.rng_t) => {
    switch (Bvh.intersect(bvh, initial_ray)) {
        | Hit(_: float, _: SurfaceProperties.t, prim: Prim.t) => {
            prim#material.display_color
        }
        | NoHit => {
            Vec.zero
        }
    }
};

let _russian_roulette_depth = 10;
let _russian_roulette_depth_agressive = 20;

let rec _path_tracer = (current_ray: Ray.t, bvh: Bvh.t, rng: Sampling.rng_t, depth: int,
    light: Vec.t, throughput: Vec.t) =>
{
    if (Vec.is_exactly_zero(throughput)) {
        light
    }
    else {
        let (current_ray_, light_, throughput_) = switch (Bvh.intersect(bvh, current_ray)) {
            | Hit(dist: float, surface_props: SurfaceProperties.t, prim: Prim.t) => {
                /* Check for scattering (reflection/transmission).
                 * Note: the material pipeline expects the incoming direction to face away from
                 * the hit point (i.e. toward the previous hit point or eye). */
                let incoming_world = ~-^current_ray.dir;
                let sample = Material.sample_world(prim#material,
                        incoming_world, surface_props, true, rng);

                /* Add illumination first, and then update throughput. */
                let new_light = light +^ (throughput *^ sample.emission);
                let new_throughput = throughput *^ (
                        (sample.radiance *^.
                        (abs_float(Vec.dot(surface_props.normal, sample.outgoing)) /. sample.pdf)));
                let new_ray = Ray.nudge(Ray.create(Ray.at(current_ray, dist), sample.outgoing));

                /* Do Russian Roulette if this path is "old". */
                let rr_throughput =
                    if (depth > _russian_roulette_depth || Vec.is_nearly_zero(new_throughput)) {
                        let rv = Sampling.next_float_unit(rng);

                        let prob_live = if (depth > _russian_roulette_depth_agressive) {
                            Math.clamped_lerp(0.10, 0.75, Vec.luminance(new_throughput))
                        }
                        else {
                            Math.clamped_lerp(0.25, 1.00, Vec.luminance(new_throughput))
                        };

                        if (rv < prob_live) {
                            /* The ray lives (more energy = more likely to live).
                            * Increase its energy to balance out probabilities. */
                            new_throughput /^. prob_live;
                        }
                        else {
                            /* The ray dies. */
                            Vec.zero
                        }
                    }
                    else {
                        /* No Russian Roulette. */
                        new_throughput
                    };
                
                (new_ray, new_light, rr_throughput)
            }
            | NoHit => {
                (current_ray, light, Vec.zero)
            }
        };

        _path_tracer(current_ray_, bvh, rng, depth + 1, light_, throughput_)
    }
};

let path_tracer: t = (initial_ray: Ray.t, bvh: Bvh.t, rng: Sampling.rng_t) => {
    _path_tracer(initial_ray, bvh, rng, 0, Vec.zero, Vec.one)
};