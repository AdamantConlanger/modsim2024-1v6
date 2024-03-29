source("./program_parts/global_functions_stuff.r")

analyze_data <- function(configuration) {
    data_store <- configuration$data_store

    # we calculate the lambdas, losses, and regrets.
    # so first, for every simulation:
    for (simulation_index in seq_along(data_store$simulations)) {
        the_simulation <- data_store$simulations[[simulation_index]]

        # set the default value for the losses and regrets of the contestants before the first round of a game.
        losses <- rep.int(c(0), times = length(configuration$contestants))
        regrets <- rep.int(c(0), times = length(configuration$contestants))

        # for every round:
        for (round_index in seq_along(the_simulation)) {
            the_round <- the_simulation[[round_index]]

            # we get the round data we've logged during the simulation
            reality <- the_round$reality
            guesses <- the_round$guesses
            parameters <- the_round$contestant_parameters

            # we calculate the lambdas of the contestants
            lambdas <- c()
            for (contestant_index in seq_along(configuration$contestants)) {
                tmp <- calculate_lambda(guesses[[contestant_index]], reality)
                lambdas <- c(lambdas, calculate_lambda(guesses[[contestant_index]], reality))
            }
            empty_statement <- 1
            lambdas <- unlist(lambdas) # because NULL[[1]] <- 1 yields a list when some_vector[[n]] <- 1 doesn't...

            if (!identical(losses, c())) {
                # we add to the losses thus far
                losses <- losses + lambdas

                # we calculate the (global) regrets; so losses of contestants further on are also considered
                minimum_loss <- min(losses)
                regrets <- losses - minimum_loss
            }

            # we put these vectors in the data store again
            the_round$lambdas <- lambdas
            the_round$losses <- losses
            the_round$regrets <- regrets

            the_simulation[[round_index]] <- the_round
        }

        # TODO: perform simulation analysis here

        data_store$simulations[[simulation_index]] <- the_simulation
    }

    configuration$data_store <- data_store

    # TODO: perform suite analysis here

    return(configuration)
}
