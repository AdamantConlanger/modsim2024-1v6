source("./program_parts/global_functions_stuff.r")

visualize_data <- function(configuration) {
    library(ggplot2)

    # make sure there's a player at the end
    contestants <- configuration$contestants
    if ((length(contestants) == 0) || (contestants[[length(contestants)]]$playstyle != "player")) {
        return(configuration)
    }

    if (!hasName(configuration$persistent_stuff, "plottables")) {
        configuration$persistent_stuff$plottables <- list()
    }

    # we get the data we'll visualize and we get the persistent data we kept about the previous suites
    sim_data <- configuration$data_store$simulations[["simulation 1"]]
    plottables <- configuration$persistent_stuff$plottables

    # gather the data we want to visualize from this round
    plottable <- c()
    for (round_name in names_or_seq_along(sim_data)) {
        round_data <- sim_data[[round_name]]
        reality <- round_data$reality
        player_index <- length(contestants)
        player_probability <- round_data$guesses[[player_index]][[reality]]
        plottable <- c(plottable, player_probability)
    }

    # get the number of each important contestant type here
    number_of_cheaters <- length(contestants[contestants == "cheater"])
    number_of_wrongs <- length(contestants[contestants == "wrong"])
    number_of_uniforms <- length(contestants[contestants == "uniform"])

    # create a label for this plot line
    the_series_label <- paste0("Ω=", as.character(configuration$omega_size))
    the_series_label <- paste0(the_series_label, ", #Ch=", as.character(number_of_cheaters))
    the_series_label <- paste0(the_series_label, ", #Wr=", as.character(number_of_wrongs))
    the_series_label <- paste0(the_series_label, ", #Un=", as.character(number_of_uniforms))

    # add the data to the plottables
    plottables[[the_series_label]] <- plottable

    # save the new plottables in the persistent stuff
    configuration$persistent_stuff$plottables <- plottables

    # convert it to an actually plottable format
    the_series <- names(plottables)
    plottables_dataframe <- as.data.frame(plottables)
    plottables_dataframe$rounds <- 1:configuration$rounds_per_simulation
    df <- reshape2::melt(plottables_dataframe, id.vars = "rounds", variable.name = "series")

    # plot it
    pictureplot <- ggplot(df, aes(rounds, value)) +
        geom_line(aes(colour = series))

    # print it
    print(pictureplot)

    # return the configuration
    return(configuration)
}
