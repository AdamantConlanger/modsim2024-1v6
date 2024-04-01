source("./program_parts/global_functions_stuff.r")

print_data <- function(configuration) {
    # get data store and json filename from configuration
    data_store <- configuration$data_store
    json_filename <- configuration$json_filename

    # construct list of contestant playstyles
    short_contestants <- list()
    for (contestant_index in seq_along(configuration$contestants)) {
        short_contestants[[contestant_index]] <- configuration$contestants[[contestant_index]]$playstyle
    }

    # summarize out all contestant parameters and children if the user config tells us to do so
    if (configuration$use_short_contestants) {
        data_store$globals$contestants <- short_contestants
    }

    # add contestant indices to parameters in each round, and remove empty parameters
    for (simulation_index in seq_along(data_store$simulations)) {
        the_simulation <- data_store$simulations[[simulation_index]]

        for (round_index in seq_along(the_simulation)) {
            the_round <- the_simulation[[round_index]]

            names(the_round$parameters) <- paste("contestant", seq_along(the_round$parameters))

            the_new_parameters <- list()
            for (contestant_index_name in names(the_round$parameters)) {
                if (!identical(the_round$parameters[[contestant_index_name]], list())) {
                    the_new_parameters[[contestant_index_name]] <- the_round$parameters[[contestant_index_name]]
                }
            }
            the_round$parameters <- the_new_parameters

            if (!configuration$keep_parameters) {
                if (!configuration$keep_final_parameters || round_index != configuration$rounds_per_simulation) {
                    the_round <- the_round[names(the_round) != "parameters"]
                }
            }

            # TODO: remove bodge
            if (configuration$contestants[[1]]$playstyle == "house") {
                the_list <- list()
                the_list[["player"]] <- the_round$parameters[["contestant 7"]]
                the_round$parameters <- the_list
            }

            the_simulation[[round_index]] <- the_round
        }

        data_store$simulations[[simulation_index]] <- the_simulation
    }

    # add nice round and simulation labels for readability
    for (simulation_index in seq_along(data_store$simulations)) {
        the_simulation <- data_store$simulations[[simulation_index]]
        names(the_simulation) <- paste("round", seq_along(the_simulation))
        data_store$simulations[[simulation_index]] <- the_simulation
    }
    names(data_store$simulations) <- paste("simulation", seq_along(data_store$simulations))


    # jsonify the data store and write it to the json file
    print(paste0("We're about to print the data to ", json_filename, "."))
    jsonlite::write_json(data_store, path = json_filename, pretty = TRUE)

    # put the data store back in the configuration
    configuration$data_store <- data_store


    return(configuration)
}
