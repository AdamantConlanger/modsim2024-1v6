source("auxiliary_systems/global_functions.r")

perform_suite <- function(configuration) {
    # get the relevant stuff from the compiled configuration object
    suite_size <- configuration$suite_size
    generate_sim_config <- configuration$generate_sim_config

    # log start of simulations
    print(paste0("Starting simulation suite of size ", suite_size, "."))

    # save the suite settings
    save_suite_store(configuration)

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
    print(paste0("Finished simulation suite of size ", suite_size, "."))
}

run_simulation <- function(sim_config) {
    round_output_data <- list()
    result <- list()
    result$collected_data <- list()
    result$round_config_default <- sim_config$round_config_default
    result$rounds_total <- sim_config$rounds_total
    round_config <- sim_config$round_config_default
    for (round_index in sane_sequence(from = 1, to = sim_config$rounds_total)) {
        prev_round_data <- round_output_data
        round_config$round_settings$round_index <- round_index
        round_config$prev_round_data <- prev_round_data
        round_output_data <- run_round(round_config)
        result$collected_data[[paste("round", round_index)]] <- round_output_data
    }
    return(result)
}

run_round <- function(round_config) {
    cur_round_data <- round_config$round_data_scaffold
    prev_round_data <- round_config$prev_round_data
    round_settings <- round_config$round_settings
    guess_executor <- round_config$guess_executor

    cur_round_data$reality <- generate_reality(round_settings)

    for (contestant_index in seq_along(prev_round_data$contestants)) {
        contestant_output <- guess_executor(
            prev_round_data$contestants[[contestant_index]],
            contestant_index,
            sane_modify_list(round_config, list(cur_round_data = cur_round_data))
        )
        cur_round_data$contestant_guesses[[contestant_index]] <- contestant_output$guess
        cur_round_data$contestants[[contestant_index]] <- contestant_output$contestant

        loss_delta <- calculate_loss_delta(contestant_output$guess, cur_round_data$reality)
        cur_round_data$contestant_loss_deltas[[contestant_index]] <- loss_delta
        loss_so_far <- prev_round_data$contestant_loss_cumulates[[contestant_index]]
        cur_round_data$contestant_loss_cumulates[[contestant_index]] <- loss_so_far + loss_delta
    }

    return(cur_round_data)
}


# reality generation for Brier game
generate_reality <- function(round_settings) {
    if (round_settings$use_user_realities) {
        return(round_settings$user_realities[[round_settings$round_index]])
    }
    return(sample.int(round_settings$omega_size, size = 1))
} # uniformly chooses an element of omega
