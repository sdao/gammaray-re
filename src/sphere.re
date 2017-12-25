open Vec.Ops;

/** Creates a new sphere with given material, world transform, and radius. */
let create = (material: Material.t, xf: Mat.t, radius: float) => {
    let sphere: Prim.t = {
        pri radius = radius;
        pri origin = Xform.transform_vec(Xform.create(xf), Vec.zero);

        pri compute_surface_props = (pt: Vec.t) => {
            /* Example: normal = (1, 0, 0)
                        tangent = (0, 0, -1)
                        binormal: (0, -1, 0) */
            let normal = Vec.normalized(pt -^ this#origin);
            if (Math.is_nearly_zero(normal.x) && Math.is_nearly_zero(normal.z)) {
                /* Singularity at top or bottom. */
                let tangent = Vec.x_axis;
                let binormal = Vec.cross(normal, tangent);
                Prim.create_surface_props(normal, tangent, binormal, normal)
            }
            else {
                /* Normal point. */
                let tangent = Vec.normalized(Vec.xyz(~-.normal.z, 0.0, normal.x));
                let binormal = Vec.cross(normal, tangent);
                Prim.create_surface_props(normal, tangent, binormal, normal)
            }
        };

        pub num_components = 1;
        pub display_color = material.display_color;
        pub material = material;

        pub bbox_world = (_: int) => {
            Bbox.from_min_max(
                Vec.xyz(
                    this#origin.x -. this#radius,
                    this#origin.y -. this#radius,
                    this#origin.z -. this#radius),
                Vec.xyz(
                    this#origin.x +. this#radius,
                    this#origin.y +. this#radius,
                    this#origin.z +. this#radius)
            )
        };

        pub intersect_world = (ray: Ray.t, _: int) => {
            let origin = ray.origin -^ this#origin;
            let l = ray.dir;

            /* See Wikipedia: <http://en.wikipedia.org/wiki/Line%E2%80%93sphere_intersection> */
            let a = Vec.dot(l, l);
            let b = Vec.dot(l, origin);
            let c = Vec.dot(origin, origin) -. (this#radius *. this#radius);

            let discriminant = (b *. b) -. (a *. c);

            if (discriminant > 0.0) {
                let sqrt_discriminant = sqrt(discriminant);
                /* Quadratic has at most 2 results. */
                let res_pos = ~-.b +. sqrt_discriminant;
                let res_neg = ~-.b -. sqrt_discriminant;

                /* Neg before pos because we want to return closest isect first. */
                if (Math.is_positive(res_neg)) {
                    let pt = Ray.at(ray, res_neg);
                    (res_neg, this#compute_surface_props(pt));
                }
                else if (Math.is_positive(res_pos)) {
                    let pt = Ray.at(ray, res_pos);
                    (res_pos, this#compute_surface_props(pt));
                }
                else {
                    /* Intersection was behind us. */
                    (0.0, Prim.empty_surface_props)
                }
            }
            else {
                /* No intersection was found. */
                (0.0, Prim.empty_surface_props)
            }
        };

        pub sample_world = (rng: Sampling.rng_t) => {
            let pt = this#origin +^
                    (Sampling.UniformSampleSphere.sample(rng) *^ Vec.from_scalar(this#radius));
            let surface_props = this#compute_surface_props(pt);
            let pdf = 1.0 /. (4.0 *. Math.pi *. this#radius *. this#radius);
            (pt, surface_props, pdf)
        };
    };

    sphere
};
