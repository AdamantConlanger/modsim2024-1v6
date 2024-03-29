source("./program_parts/global_functions_stuff.r")

# performs a suite of simulations
perform_suite <- function(configuration) {
    # get the relevant stuff from the configuration
    suite_size <- configuration$suite_size

    # TODO: store simulation-initial values to the data_store here, like base_contestants
    # TODO: store global settings to the data_store here, like omega_size

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
    contestants <- configuration$base_contestants

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
    if (configuration$use_user_realities) {
        round_data$reality <- configuration$user_realities[[round_index]]
    } else {
        round_data$reality <- sample.int(configuration$omega_size, size = 1)
    }

    # let each of the contestants guess
    for (contestant_index in seq_along(contestants)) {
        # get the guessing functions from the environment where this function is defined
        guessing_functions <- configuration$compiled_playstyles_guessing_functions

        # get the correct guessing function for the playstyle of this contestant
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
        round_data$contestant_guesses[[contestant_index]] <- contestant_result_output$guess
        round_data$contestant_parameters[[contestant_index]] <- contestant_result_output$parameters
    }

    # return updated contestants and round data
    return(list(contestants = contestants, round_data = round_data))
}
