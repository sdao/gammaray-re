/** Encapsulates all data needed for raytracing. */
type t = {
    bvh: Bvh.t,
    sample_storage: ref(array(Film.sample_t))
};

let create = (prims: array(Prim.t)) => {
    {bvh: Bvh.build(prims), sample_storage: ref([||])}
};

let trace = (stage: t, camera: Camera.t, integrator: Integrator.t, film: Film.t) => {
    Film.compute_sample_points(film, stage.sample_storage);
    Array.iter((sample: Film.sample_t) => {
        let ray = Camera.compute_ray(camera, sample.s, sample.t);
        let rng = Sampling.create_rng();
        sample.color = integrator#integrate(ray, stage.bvh, rng);
    }, stage.sample_storage^);
    Film.report_samples(film, stage.sample_storage);
};
