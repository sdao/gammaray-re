/** Encapsulates all data needed for raytracing. */
type t = {
    bvh: Bvh.t,
    camera: Camera.t,
    integrator: Integrator.t,
    film: Film.t,
    sample_storage: ref(array(Film.sample_t))
};

let create = (prims: array(Prim.t), cam: Camera.t, integrator: Integrator.t, w: int, h: int) => {
    {
        bvh: Bvh.build(prims),
        camera: cam,
        integrator: integrator,
        film: Film.create(w, h),
        sample_storage: ref([||])
    }
};

let trace = (stage: t) => {
    let rng = Sampling.create_rng();
    Film.compute_sample_points(stage.film, rng, stage.sample_storage);
    Array.iter((sample: Film.sample_t) => {
        let ray = Camera.compute_ray(stage.camera, sample.s, sample.t);
        let thread_rng = Sampling.create_seeded_rng(rng);
        sample.color = stage.integrator#integrate(ray, stage.bvh, thread_rng);
    }, stage.sample_storage^);
    Film.report_samples(stage.film, stage.sample_storage);
};
