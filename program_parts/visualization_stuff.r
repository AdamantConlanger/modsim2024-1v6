source("./program_parts/global_functions_stuff.r")

visualize_data <- function(configuration) {
    # TODO: make this at least somewhat functioning.
    return(configuration)
    library(ggplot2)

    collected_data <- display_data_bank$sims$`sim 1`$collected_data

    the_y_axis <- c()
    for (round_name in names_or_seq_along(collected_data)) {
        round_data <- collected_data[[round_name]]

        reality <- round_data$reality
        the_player_index <- length(display_data_bank$suite$compiled_contestants)

        # The first of these two is for if you save all contestant guesses,
        # The second of the two is for if you only have the player guess
        # the_player_prob_for_reality <- round_data$contestant_guesses[[the_player_index]][[reality]]
        the_player_prob_for_reality <- round_data$contestant_guesses[[1]][[reality]]

        the_y_axis <- c(the_y_axis, the_player_prob_for_reality)
    }

    if (!exists("the_y_axes", inherits = TRUE)) {
        the_y_axes <<- list()
    }

    contestants <- display_data_bank$suite$compiled_contestants
    number_of_normals <- length(contestants[contestants == "normalm"])
    number_of_wrongs <- length(contestants[contestants == "wrong"])
    number_of_uniforms <- length(contestants[contestants == "uniform"])

    the_series_label <- paste0("Ω|=", as.character(display_data_bank$suite$omega_size))
    the_series_label <- paste0(the_series_label, ",\n#No=", as.character(number_of_normals))
    the_series_label <- paste0(the_series_label, ",\n#Wr=", as.character(number_of_wrongs))
    the_series_label <- paste0(the_series_label, ",\n#Un=", as.character(number_of_uniforms))

    the_y_axes[[the_series_label]] <- the_y_axis
    # print(the_y_axes)
    the_series <- names(the_y_axes)
    y_dataframe <- as.data.frame(the_y_axes)
    # names(y_dataframe) <- sub(",", ",\n ", names(y_dataframe))
    y_dataframe$rounds <- 1:display_data_bank$suite$rounds_per_simulation
    df <- reshape2::melt(y_dataframe, id.vars = "rounds", variable.name = "series")
    pictureplot <- ggplot(df, aes(rounds, value)) +
        geom_line(aes(colour = series))
    # pictureplot <- pictureplot + scale_colour_manual(breaks=unlist(the_series), values = colors())
    # pictureplot <- pictureplot + theme(legend.position="bottom")
    # pictureplot <- pictureplot + scale_colour_discrete(labels = function(x) stringr::str_wrap(x, width = 5))
    print(pictureplot)
    the_y_axes <<- the_y_axes
}
