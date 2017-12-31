/** Abstract type for geometry primitives. */
type t = {
    .
    num_components: int,
    display_color: Vec.t,
    material: Material.t,

    /**
     * Returns the bounding box in world space for the geometry associated with the given component.
     * It's OK to compute this on demand (and not cache the bounding box) because it is the
     * responsibility of callers (such as acceleration structures) to cache the value.
     *
     * `bbox_world component` returns the bounding box of the given component.
     */
    bbox_world: int => Bbox.t,

    /**
     * Intersects the given ray in world space with the prim component, and returns the distance
     * along the ray and the surface properties at the point of intersection.
     * Implementations should be able to handle cases where the incoming ray is not unit length.
     * Implementations also do not have to return unit-length vectors in the SurfaceProperties,
     * although it is recommended.
     *
     * `intersect_world ray component` intersects the ray with the given component.
     */
    intersect_world: (Ray.t, int) => (float, SurfaceProperties.t),
    
    /**
     * Sample a random point in world space on the prim, with respect to the area of the prim.
     * Returns the position, surface properties, and pdf at the sampled point.
     *
     * `sample_world rng` uses the given rng to sample a random point on the prim.
     */
    sample_world: (Sampling.rng_t) => (Vec.t, SurfaceProperties.t, float),
};

/**
 * Sample a random ray starting from a random point on the prim.
 * Returns the ray, surface properties at the origin, the pdf of the origin position, and the
 * pdf of the ray direction.
 */
let sample_ray_world = (prim: t, rng: Sampling.rng_t) => {
    let (point, surface_props, point_pdf) = prim#sample_world(rng);

    let dir = Sampling.CosineSampleHemisphere.sample(rng, false);
    let dir_pdf = Sampling.CosineSampleHemisphere.pdf(dir);

    let (tangent, binormal) = Vec.coord_system(surface_props.geom_normal);
    let dir_world = Vec.local_to_world(dir, tangent, binormal, surface_props.geom_normal);

    let light_ray = Ray.create(point, dir_world);
    (light_ray, surface_props, point_pdf, dir_pdf)
};
