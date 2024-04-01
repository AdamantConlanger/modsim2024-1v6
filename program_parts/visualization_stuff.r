source("./program_parts/global_functions_stuff.r")

visualize_data <- function(configuration) {
    library(ggplot2)

    # make sure there's a player at the end
    contestants <- configuration$contestants
    if ((length(contestants) == 0) || (contestants[[length(contestants)]]$playstyle != "player")) {
        return(configuration)
    }

    # make sure we have short contestant names in our own contestants object
    # note that configuration$contestants always uses non-short contestants usually
    short_contestants <- list()
    for (contestant_index in seq_along(contestants)) {
        short_contestants[[contestant_index]] <- contestants[[contestant_index]]$playstyle
    }
    contestants <- short_contestants

    if (!hasName(configuration$recovered_stuff, "plottables")) {
        configuration$recovered_stuff$plottables <- list()
    }

    # we get the data we'll visualize and we get the persistent data we kept about the previous suites
    sim_data <- configuration$data_store$simulations[["simulation 1"]]
    plottables <- configuration$recovered_stuff$plottables

    # gather the data we want to visualize from this round
    plottable <- c()
    for (round_name in names_or_seq_along(sim_data)) {
        round_data <- sim_data[[round_name]]
        reality <- round_data$reality
        player_index <- length(contestants)
        player_probability <- round_data$guesses[[player_index]][[reality]]
        if (identical(plottable, c())) {
            plottable <- c(player_probability)
        } else {
            plottable <- c(plottable, player_probability)
        }
    }

    # create a label for this plot line
    the_series_label <- paste0("|Ω|=", as.character(configuration$omega_size))
    if (length(contestants[contestants == "cheater"]) != 0) {
        number_of_cheaters <- length(contestants[contestants == "cheater"])
        the_series_label <- paste0(the_series_label, ", #Ch=", as.character(number_of_cheaters))
    }
    if (length(contestants[contestants == "wrong"]) != 0) {
        number_of_wrongs <- length(contestants[contestants == "wrong"])
        the_series_label <- paste0(the_series_label, ", #Wr=", as.character(number_of_wrongs))
    }
    if (length(contestants[contestants == "uniform"]) != 0) {
        number_of_uniforms <- length(contestants[contestants == "uniform"])
        the_series_label <- paste0(the_series_label, ", #Un=", as.character(number_of_uniforms))
    }
    if (length(contestants[contestants == "consistent"]) != 0) {
        number_of_consistents <- length(contestants[contestants == "consistent"])
        the_series_label <- paste0(the_series_label, ", #Cs=", as.character(number_of_consistents))
    }
    if (length(contestants[contestants == "confident"]) != 0) {
        number_of_confidents <- length(contestants[contestants == "confident"])
        the_series_label <- paste0(the_series_label, ", #Cf=", as.character(number_of_confidents))
    }
    if (length(contestants[contestants == "house"]) != 0) {
        number_of_houses <- length(contestants[contestants == "house"])
        the_series_label <- paste0(the_series_label, ", #Ho=", as.character(number_of_houses))
    }
    if (length(contestants[contestants == "toggle_cheater_wrong"]) != 0) {
        number_of_togglecws <- length(contestants[contestants == "toggle_cheater_wrong"])
        the_series_label <- paste0(the_series_label, ", #TCW=", as.character(number_of_togglecws))
    }
    if (length(contestants[contestants == "toggle_wrong_cheater"]) != 0) {
        number_of_togglewcs <- length(contestants[contestants == "toggle_wrong_cheater"])
        the_series_label <- paste0(the_series_label, ", #TWC=", as.character(number_of_togglewcs))
    }

    # add the data to the plottables
    if (hasName(plottables, the_series_label)) {
        the_original_series_label <- the_series_label
        label_index <- 1
        the_series_label <- paste(the_original_series_label, label_index)
    }
    while (hasName(plottables, the_series_label)) {
        label_index <- label_index + 1
        the_series_label <- paste(the_original_series_label, label_index)
    }
    plottables[[the_series_label]] <- plottable

    # save the new plottables in the persistent stuff
    configuration$persistent_stuff$plottables <- plottables

    # convert it to an actually plottable format
    the_series <- names(plottables)
    for (column in plottables) {
        if (length(column) != configuration$rounds_per_simulation) {
            stop("Incompatible past data; you should delete resources/persistent_stuff.rds.")
        }
    }
    plottables_dataframe <- as.data.frame(plottables)
    plottables_dataframe$rounds <- 1:configuration$rounds_per_simulation
    df <- reshape2::melt(plottables_dataframe, id.vars = "rounds", variable.name = "series")

    gg_color_hue <- function(n) {
        hues <- seq(15, 375, length = n + 1)
        hcl(h = hues, l = 65, c = 100)[1:n]
    }
    my_colors <- gg_color_hue(length(plottables))

    the_title <- "Plot of the player's predicted probability for the\ncorrect outcome, for various configurations"

    # plot it
    pictureplot <- ggplot(df, aes(rounds, value)) +
        geom_line(aes(colour = series)) +
        ggtitle(the_title) +
        ylab("probability") +
        scale_color_manual(
            name = "series",
            labels = the_series,
            values = my_colors
        )

    # print it
    print(pictureplot)

    # return the configuration
    return(configuration)
}


visualize_weights <- function(configuration) {
    library(ggplot2)

    # make sure there's a player at the end
    contestants <- configuration$contestants
    if ((length(contestants) == 0) || (contestants[[length(contestants)]]$playstyle != "player")) {
        return(configuration)
    }

    # make sure we have short contestant names in our own contestants object
    # note that configuration$contestants always uses non-short contestants usually
    short_contestants <- list()
    for (contestant_index in seq_along(contestants)) {
        short_contestants[[contestant_index]] <- contestants[[contestant_index]]$playstyle
    }
    contestants <- short_contestants

    if (!hasName(configuration$recovered_stuff, "plottables")) {
        configuration$recovered_stuff$plottables <- list()
    }

    # we get the data we'll visualize
    sim_data <- configuration$data_store$simulations[["simulation 1"]]

    # gather the data we want to visualize from this round
    plottables <- rep.int(list(c()), times = length(contestants) - 1)
    names(plottables) <- list("B365", "BW", "IW", "PS", "VC", "WH")
    for (round_name in names_or_seq_along(sim_data)) {
        round_data <- sim_data[[round_name]]
        reality <- round_data$reality
        player_index <- length(round_data$parameters)

        weights <- round_data$parameters[[player_index]]$weights
        if (length(weights) == 0) {
            next
        }
        if (max(weights) == 0) {
            stop("all weights are zero; can't make plot.")
        }
        weights <- weights / max(weights)

        for (competitor_index in sane_sequence(from = 1, to = length(contestants) - 1)) {
            if (identical(plottables[[competitor_index]], c())) {
                plottables[[competitor_index]] <- c(weights[[competitor_index]])
            } else {
                plottables[[competitor_index]] <- c(plottables[[competitor_index]], weights[[competitor_index]])
            }
        }
    }

    # convert it to an actually plottable format
    the_series <- names(plottables)
    plottables_dataframe <- as.data.frame(plottables)
    plottables_dataframe$rounds <- 1:configuration$rounds_per_simulation
    df <- reshape2::melt(plottables_dataframe, id.vars = "rounds", variable.name = "series")

    gg_color_hue <- function(n) {
        hues <- seq(15, 375, length = n + 1)
        hcl(h = hues, l = 65, c = 100)[1:n]
    }
    my_colors <- gg_color_hue(length(plottables))

    the_title <- "Plot of the player's weights for various contestants,\nnormalized by round maximum, using soccer data"

    # plot it
    pictureplot <- ggplot(df, aes(rounds, value)) +
        geom_line(aes(colour = series)) +
        ggtitle(the_title) +
        ylab("weight") +
        scale_color_manual(
            name = "series",
            labels = the_series,
            values = my_colors
        )

    # print it
    print(pictureplot)

    # return the configuration
    return(configuration)
}


visualize_lambdas <- function(configuration) {
    library(ggplot2)

    # make sure there's a player at the end
    contestants <- configuration$contestants
    if ((length(contestants) == 0) || (contestants[[length(contestants)]]$playstyle != "player")) {
        return(configuration)
    }

    # make sure we have short contestant names in our own contestants object
    # note that configuration$contestants always uses non-short contestants usually
    short_contestants <- list()
    for (contestant_index in seq_along(contestants)) {
        short_contestants[[contestant_index]] <- contestants[[contestant_index]]$playstyle
    }
    contestants <- short_contestants

    if (!hasName(configuration$recovered_stuff, "plottables")) {
        configuration$recovered_stuff$plottables <- list()
    }

    # we get the data we'll visualize
    sim_data <- configuration$data_store$simulations[["simulation 1"]]

    # gather the data we want to visualize from this round
    plottables <- rep.int(list(c()), times = length(contestants) - 1)
    names(plottables) <- list("B365", "BW", "IW", "PS", "VC", "WH")
    for (round_name in names_or_seq_along(sim_data)) {
        round_data <- sim_data[[round_name]]

        lambdas <- round_data$lambdas
        if (length(lambdas) == 0) {
            next
        }

        for (competitor_index in sane_sequence(from = 1, to = length(contestants) - 1)) {
            if (identical(plottables[[competitor_index]], c())) {
                plottables[[competitor_index]] <- c(lambdas[[competitor_index]])
            } else {
                plottables[[competitor_index]] <- c(plottables[[competitor_index]], lambdas[[competitor_index]])
            }
        }
    }

    # convert it to an actually plottable format
    the_series <- names(plottables)
    plottables_dataframe <- as.data.frame(plottables)
    plottables_dataframe$rounds <- 1:configuration$rounds_per_simulation
    df <- reshape2::melt(plottables_dataframe, id.vars = "rounds", variable.name = "series")

    gg_color_hue <- function(n) {
        hues <- seq(15, 375, length = n + 1)
        hcl(h = hues, l = 65, c = 100)[1:n]
    }
    my_colors <- gg_color_hue(length(plottables))

    the_title <- "Plot of contestant lambdas, using soccer data"

    # plot it
    pictureplot <- ggplot(df, aes(rounds, value)) +
        geom_point(aes(colour = series)) +
        ggtitle(the_title) +
        ylab("lambda") +
        scale_color_manual(
            name = "series",
            labels = the_series,
            values = my_colors
        )

    # print it
    print(pictureplot)

    # return the configuration
    return(configuration)
}

visualize_losses_excess <- function(configuration) {
    library(ggplot2)

    # make sure there's a player at the end
    contestants <- configuration$contestants
    if ((length(contestants) == 0) || (contestants[[length(contestants)]]$playstyle != "player")) {
        return(configuration)
    }

    # make sure we have short contestant names in our own contestants object
    # note that configuration$contestants always uses non-short contestants usually
    short_contestants <- list()
    for (contestant_index in seq_along(contestants)) {
        short_contestants[[contestant_index]] <- contestants[[contestant_index]]$playstyle
    }
    contestants <- short_contestants

    if (!hasName(configuration$recovered_stuff, "plottables")) {
        configuration$recovered_stuff$plottables <- list()
    }

    # we get the data we'll visualize
    sim_data <- configuration$data_store$simulations[["simulation 1"]]

    # gather the data we want to visualize from this round
    plottables <- rep.int(list(c()), times = length(contestants) - 1)
    names(plottables) <- list("B365", "BW", "IW", "PS", "VC", "WH")
    for (round_name in names_or_seq_along(sim_data)) {
        round_data <- sim_data[[round_name]]

        losses <- round_data$losses
        if (length(losses) == 0) {
            next
        }
        excesses <- losses - min(losses[sane_sequence(from = 1, to = length(contestants) - 1)])


        for (competitor_index in sane_sequence(from = 1, to = length(contestants) - 1)) {
            if (identical(plottables[[competitor_index]], c())) {
                plottables[[competitor_index]] <- c(excesses[[competitor_index]])
            } else {
                plottables[[competitor_index]] <- c(plottables[[competitor_index]], excesses[[competitor_index]])
            }
        }
    }

    # convert it to an actually plottable format
    the_series <- names(plottables)
    plottables_dataframe <- as.data.frame(plottables)
    plottables_dataframe$rounds <- 1:configuration$rounds_per_simulation
    df <- reshape2::melt(plottables_dataframe, id.vars = "rounds", variable.name = "series")

    gg_color_hue <- function(n) {
        hues <- seq(15, 375, length = n + 1)
        hcl(h = hues, l = 65, c = 100)[1:n]
    }
    my_colors <- gg_color_hue(length(plottables))

    the_title <- "Plot of betting house losses exceeding the round\nminimum excluding the player, using soccer data"

    # plot it
    pictureplot <- ggplot(df, aes(rounds, value)) +
        geom_line(aes(colour = series)) +
        ggtitle(the_title) +
        ylab("loss excess") +
        scale_color_manual(
            name = "series",
            labels = the_series,
            values = my_colors
        )

    # print it
    print(pictureplot)

    # return the configuration
    return(configuration)
}


visualize_losses_excess_with_player <- function(configuration) {
    library(ggplot2)

    # make sure there's a player at the end
    contestants <- configuration$contestants
    if ((length(contestants) == 0) || (contestants[[length(contestants)]]$playstyle != "player")) {
        return(configuration)
    }

    # make sure we have short contestant names in our own contestants object
    # note that configuration$contestants always uses non-short contestants usually
    short_contestants <- list()
    for (contestant_index in seq_along(contestants)) {
        short_contestants[[contestant_index]] <- contestants[[contestant_index]]$playstyle
    }
    contestants <- short_contestants

    if (!hasName(configuration$recovered_stuff, "plottables")) {
        configuration$recovered_stuff$plottables <- list()
    }

    # we get the data we'll visualize
    sim_data <- configuration$data_store$simulations[["simulation 1"]]

    # gather the data we want to visualize from this round
    plottables <- rep.int(list(c()), times = length(contestants))
    names(plottables) <- list("B365", "BW", "IW", "PS", "VC", "WH", "player")
    for (round_name in names_or_seq_along(sim_data)) {
        round_data <- sim_data[[round_name]]

        losses <- round_data$losses
        if (length(losses) == 0) {
            next
        }
        excesses <- losses - min(losses[sane_sequence(from = 1, to = length(contestants))])


        for (competitor_index in sane_sequence(from = 1, to = length(contestants))) {
            if (identical(plottables[[competitor_index]], c())) {
                plottables[[competitor_index]] <- c(excesses[[competitor_index]])
            } else {
                plottables[[competitor_index]] <- c(plottables[[competitor_index]], excesses[[competitor_index]])
            }
        }
    }

    # convert it to an actually plottable format
    the_series <- names(plottables)
    plottables_dataframe <- as.data.frame(plottables)
    plottables_dataframe$rounds <- 1:configuration$rounds_per_simulation
    df <- reshape2::melt(plottables_dataframe, id.vars = "rounds", variable.name = "series")

    gg_color_hue <- function(n) {
        hues <- seq(15, 375, length = n + 1)
        hcl(h = hues, l = 65, c = 100)[1:n]
    }
    my_colors <- gg_color_hue(length(plottables))

    the_title <- paste(
        "Plot of contestant losses exceeding the round minimum,\nusing soccer data"
    )

    # plot it
    pictureplot <- ggplot(df, aes(rounds, value)) +
        geom_line(aes(colour = series)) +
        ggtitle(the_title) +
        ylab("loss excess") +
        scale_color_manual(
            name = "series",
            labels = the_series,
            values = my_colors
        )

    # print it
    print(pictureplot)

    # return the configuration
    return(configuration)
}

visualize_lambdas_excess_with_player <- function(configuration) {
    library(ggplot2)

    # make sure there's a player at the end
    contestants <- configuration$contestants
    if ((length(contestants) == 0) || (contestants[[length(contestants)]]$playstyle != "player")) {
        return(configuration)
    }

    # make sure we have short contestant names in our own contestants object
    # note that configuration$contestants always uses non-short contestants usually
    short_contestants <- list()
    for (contestant_index in seq_along(contestants)) {
        short_contestants[[contestant_index]] <- contestants[[contestant_index]]$playstyle
    }
    contestants <- short_contestants

    if (!hasName(configuration$recovered_stuff, "plottables")) {
        configuration$recovered_stuff$plottables <- list()
    }

    # we get the data we'll visualize
    sim_data <- configuration$data_store$simulations[["simulation 1"]]

    # gather the data we want to visualize from this round
    plottables <- rep.int(list(c()), times = length(contestants))
    names(plottables) <- list("B365", "BW", "IW", "PS", "VC", "WH", "player")
    for (round_name in names_or_seq_along(sim_data)) {
        round_data <- sim_data[[round_name]]

        lambdas <- round_data$lambdas
        if (length(lambdas) == 0) {
            next
        }
        excesses <- lambdas / min(lambdas)


        for (competitor_index in sane_sequence(from = 1, to = length(contestants))) {
            if (identical(plottables[[competitor_index]], c())) {
                plottables[[competitor_index]] <- c(excesses[[competitor_index]])
            } else {
                plottables[[competitor_index]] <- c(plottables[[competitor_index]], excesses[[competitor_index]])
            }
        }
    }

    # convert it to an actually plottable format
    the_series <- names(plottables)
    plottables_dataframe <- as.data.frame(plottables)
    plottables_dataframe$rounds <- 1:configuration$rounds_per_simulation
    df <- reshape2::melt(plottables_dataframe, id.vars = "rounds", variable.name = "series")

    gg_color_hue <- function(n) {
        hues <- seq(15, 375, length = n + 1)
        hcl(h = hues, l = 65, c = 100)[1:n]
    }
    my_colors <- gg_color_hue(length(plottables))

    the_title <- paste(
        "Plot of soccer data contestant lambdas,",
        "rescaled relative to round minima,",
        "with loess trends, cut off above at 1.25"
    )

    # plot it
    pictureplot <- ggplot(df, aes(rounds, value)) +
        geom_point(aes(colour = series), alpha = 0.75, size = 1) +
        geom_smooth(aes(colour = series), method = "loess", fill = NA) +
        ggtitle(the_title) +
        coord_cartesian(ylim = c(1, 1.25)) +
        ylab("lambda") +
        scale_color_manual(
            name = "series",
            labels = the_series,
            values = my_colors
        )

    # print it
    print(pictureplot)

    # return the configuration
    return(configuration)
}

visualize_losses_for_sim_analysis <- function(configuration) {
    library(ggplot2)

    # make sure there's a player at the end
    contestants <- configuration$contestants
    if ((length(contestants) == 0) || (contestants[[length(contestants)]]$playstyle != "player")) {
        return(configuration)
    }

    # make sure we have short contestant names in our own contestants object
    # note that configuration$contestants always uses non-short contestants usually
    short_contestants <- list()
    for (contestant_index in seq_along(contestants)) {
        short_contestants[[contestant_index]] <- contestants[[contestant_index]]$playstyle
    }
    contestants <- short_contestants

    # we get the data we'll visualize
    sim_data <- configuration$data_store$simulations[["simulation 1"]]

    # gather the data we want to visualize from this round
    plottables <- rep.int(list(c()), times = length(contestants) + 1)
    names(plottables) <- list("well", "poor", "learner", paste("min loss + ln", as.character(length(contestants) - 1)))
    for (round_name in names_or_seq_along(sim_data)) {
        round_data <- sim_data[[round_name]]

        losses <- round_data$losses
        if (length(losses) == 0) {
            next
        }

        for (contestant_index in sane_sequence(from = 1, to = length(contestants))) {
            if (identical(plottables[[contestant_index]], c())) {
                plottables[[contestant_index]] <- c(losses[[contestant_index]])
            } else {
                plottables[[contestant_index]] <- c(plottables[[contestant_index]], losses[[contestant_index]])
            }
        }
        only_experts <- losses[sane_sequence(from = 1, to = length(contestants) - 1)]
        min_loss <- min(only_experts)
        if (identical(plottables[[length(contestants) + 1]], c())) {
            plottables[[length(contestants) + 1]] <- c(min_loss + log(length(contestants) - 1))
        } else {
            new_vector <- c(plottables[[length(contestants) + 1]], min_loss + log(length(contestants) - 1))
            plottables[[length(contestants) + 1]] <- new_vector
        }
    }

    # convert it to an actually plottable format
    the_series <- names(plottables)
    plottables_dataframe <- as.data.frame(plottables)
    plottables_dataframe$rounds <- 1:configuration$rounds_per_simulation
    df <- reshape2::melt(plottables_dataframe, id.vars = "rounds", variable.name = "series")

    series_names <- c("well", "poor", "learner", "min loss + ln 2")

    gg_color_hue <- function(n) {
        hues <- seq(15, 375, length = n + 1)
        hcl(h = hues, l = 65, c = 100)[1:n]
    }

    my_colors <- gg_color_hue(length(plottables))

    # plot it
    pictureplot <- ggplot(df, aes(rounds, value)) +
        geom_line(aes(colour = series)) +
        ggtitle("Plot of losses for the binary simulation") +
        coord_cartesian(ylim = c(0, 0.75)) +
        ylab("loss") +
        scale_color_manual(
            name = "series",
            labels = series_names,
            values = my_colors
        )

    # print it
    print(pictureplot)

    # return the configuration
    return(configuration)
}
