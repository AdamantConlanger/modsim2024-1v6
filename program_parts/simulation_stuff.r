perform_suite <- function(compiled_configuration, data_handler, logging_handler) {
    # get the relevant stuff from the compiled configuration object
    suite_size <- compiled_configuration$suite_size
    generate_sim_config <- compiled_configuration$generate_sim_config

    # get the relevant stuff from the data handler object
    save_sim_store <- data_handler$save_sim_store
    save_suite_store <- data_handler$save_suite_store

    # get the relevant stuff from the logging handler object
    print_to_log <- logging_handler$print_to_log

    # log start of simulations
    print_to_log(paste0("Starting simulation suite of size ", suite_size, "."))

    # save the suite settings
    save_suite_store(compiled_configuration)

    # run simulations
    for (sim_index in sane_sequence(from = 1, to = suite_size)) {
        # generate simulation-specific configuration
        sim_config <- generate_sim_config(sim_index)

        # run simulation and receive its raw data store (truly raw; absolutely no filtering)
        sim_store <- run_simulation(sim_config)

        # save the simulation info and data store to data bank
        save_sim_store(sim_store, sim_index)
    }

    # log end of simulations
    print_to_log(paste0("Finished simulation suite of size ", suite_size, "."))
}
