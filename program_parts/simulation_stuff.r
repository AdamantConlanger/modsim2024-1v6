source("./program_parts/global_functions_stuff.r")

# performs a suite of simulations
perform_suite <- function(configuration) {
    # get the relevant stuff from the configuration
    suite_size <- configuration$suite_size

    configuration$data_store$globals <- list(
        contestants = configuration$contestants,
        suite_size = configuration$suite_size,
        rounds_per_simulation = configuration$rounds_per_simulation,
        omega_size = configuration$omega_size,
        use_seed = configuration$use_seed,
        use_predetermined_realities = configuration$use_predetermined_realities
    )
    if (configuration$use_seed) {
        configuration$data_store$globals$custom_seed <- configuration$custom_seed
    }
    if (configuration$use_predetermined_realities) {
        configuration$data_store$globals$predetermined_realities <- configuration$predetermined_realities
    }

    # log start of simulations
    print(paste0("Starting simulation suite of size ", suite_size, "."))

    # run simulations
    for (simulation_index in sane_sequence(from = 1, to = suite_size)) {
        # run simulation and receive its raw data
        configuration$data_store$simulations[[simulation_index]] <- run_simulation(configuration)
    }

    # log end of simulations
    print(paste0("Finished simulation suite of size ", suite_size, "."))

    # return the configuration with the suite data
    return(configuration)
}

# performs a Brier game simulation
run_simulation <- function(configuration) {
    # get simulation-initial base values for contestants and their parameters
    contestants <- configuration$contestants

    # set up simulation data object
    simulation_data <- list()

    # run rounds
    for (round_index in sane_sequence(from = 1, to = configuration$rounds_per_simulation)) {
        # run round and receive its resulting output
        round_result_object <- run_round(configuration, round_index, contestants)

        # update contestants
        contestants <- round_result_object$contestants

        # put the data in the simulation data
        simulation_data[[round_index]] <- round_result_object$round_data
    }

    # return the simulation data
    return(simulation_data)
}

# performs a round of the Brier game
run_round <- function(configuration, round_index, contestants) {
    # get guess executor function from configuration
    guess_executor <- configuration$guess_executor

    # set up round data object
    round_data <- list()

    # generate (or read) reality
    if (configuration$use_predetermined_realities) {
        round_data$reality <- configuration$predetermined_realities[[round_index]]
    } else {
        round_data$reality <- sample.int(configuration$omega_size, size = 1)
    }

    # let each of the contestants guess
    for (contestant_index in seq_along(contestants)) {
        # get the correct guessing function for the playstyle of this contestant
        guessing_functions <- configuration$guessing_functions
        guessing_function <- guessing_functions[[contestants[[contestant_index]]$playstyle]]

        # get the parameters of this contestant
        parameters <- contestants[[contestant_index]]$parameters

        # call the guessing function
        contestant_result_output <- guessing_function(
            parameters,
            contestant_index,
            configuration,
            round_data,
            round_index
        )

        # update the contestant's parameters
        contestants[[contestant_index]]$parameters <- contestant_result_output$parameters

        # put the guess and updated parameters in the round data
        round_data$guesses[[contestant_index]] <- contestant_result_output$guess
        round_data$parameters[[contestant_index]] <- contestant_result_output$parameters
    }

    # return updated contestants and round data
    return(list(contestants = contestants, round_data = round_data))
}
