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

  x_tick_vector <- c(1, 2, 3, 4, 5, 6, 7, 8)

  # # plot it
  # pictureplot <- ggplot(df, aes(rounds, value, linewidth = series, linetype = series)) +
  #   geom_line(aes(colour = series)) +
  #   ggtitle(the_title) +
  #   ylab("probability") +
  #   scale_color_manual(
  #     name = "series",
  #     labels = the_series,
  #     values = my_colors
  #   )+
  #   scale_linewidth_manual(values = c(1, 1, 1, 1), guide = "none") +
  #   scale_linetype_manual(values = c("45", "2223", "2421", "27"), guide = "none")+
  #   scale_x_continuous(breaks=x_tick_vector)

  # plot it
  pictureplot <- ggplot(df, aes(rounds, value)) +
    geom_line(aes(colour = series)) +
    ggtitle(the_title) +
    ylab("probability") +
    scale_color_manual(
      name = "series",
      labels = the_series,
      values = my_colors
    ) +
    scale_x_continuous(breaks = x_tick_vector)

  # print it
  print(pictureplot)

  # return the configuration
  return(configuration)
}


visualize_weights_normalized <- function(configuration) {
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

  x_tick_vector <- c(1, 2, 3, 4, 5, 6, 7, 8)

  # plot it
  pictureplot <- ggplot(df, aes(rounds, value)) +
    geom_line(aes(colour = series)) +
    ggtitle(the_title) +
    ylab("normalized weight") +
    scale_color_manual(
      name = "series",
      labels = the_series,
      values = my_colors
    ) +
    scale_x_continuous(breaks = x_tick_vector)

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

  x_tick_vector <- c(1, 2, 3, 4, 5, 6, 7, 8)

  # plot it
  pictureplot <- ggplot(df, aes(rounds, value)) +
    geom_point(aes(colour = series)) +
    ggtitle(the_title) +
    ylab("lambda") +
    scale_color_manual(
      name = "series",
      labels = the_series,
      values = my_colors
    ) +
    scale_x_continuous(breaks = x_tick_vector)

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

  x_tick_vector <- c(1, 2, 3, 4, 5, 6, 7, 8)

  # plot it
  pictureplot <- ggplot(df, aes(rounds, value)) +
    geom_line(aes(colour = series)) +
    ggtitle(the_title) +
    ylab("loss excess") +
    scale_color_manual(
      name = "series",
      labels = the_series,
      values = my_colors
    ) +
    scale_x_continuous(breaks = x_tick_vector)

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

  x_tick_vector <- c(1, 2, 3, 4, 5, 6, 7, 8)

  # plot it
  pictureplot <- ggplot(df, aes(rounds, value)) +
    geom_line(aes(colour = series)) +
    ggtitle(the_title) +
    ylab("loss excess") +
    scale_color_manual(
      name = "series",
      labels = the_series,
      values = my_colors
    ) +
    scale_x_continuous(breaks = x_tick_vector)

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
    "rescaled relative to\nround minima,",
    "with loess trends, cut off above at 1.25"
  )

  x_tick_vector <- c(1, 2, 3, 4, 5, 6, 7, 8)

  # plot it
  pictureplot <- ggplot(df, aes(rounds, value)) +
    # geom_point(aes(colour = series), alpha = 0.75, size = 1) +
    geom_point(aes(colour = series), alpha = 0.75, size = 0.75) +
    geom_smooth(aes(colour = series), method = "loess", fill = NA) +
    ggtitle(the_title) +
    coord_cartesian(ylim = c(1, 1.25)) +
    ylab("lambda") +
    scale_color_manual(
      name = "series",
      labels = the_series,
      values = my_colors
    ) +
    scale_x_continuous(breaks = x_tick_vector)

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
  names(plottables) <- list("well-performing", "poorly performing", "learner", paste("minimum loss + ln", as.character(length(contestants) - 1)))
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

  series_names <- c("well-performing", "poorly performing", "learner", paste("minimum loss + ln", as.character(length(contestants) - 1)))


  gg_color_hue <- function(n) {
    hues <- seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }

  my_colors <- gg_color_hue(length(plottables))

  x_tick_vector <- c(1, 2, 3, 4, 5, 6, 7, 8)

  # plot it
  pictureplot <- ggplot(df, aes(rounds, value)) +
    geom_line(aes(colour = series)) +
    ggtitle("Plot of losses for the binary simulation,\ncut off vertically at 3 for clarity's sake") +
    # coord_cartesian(ylim = c(0, 0.75)) +
    coord_cartesian(ylim = c(0, 3)) +
    ylab("loss") +
    scale_color_manual(
      name = "series",
      labels = series_names,
      values = my_colors
    ) +
    scale_x_continuous(breaks = x_tick_vector)

  # print it
  print(pictureplot)

  # return the configuration
  return(configuration)
}

visualize_regret_for_sim_analysis <- function(configuration) {
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

  # gather the data we want to visualize from this round
  plottable <- c()
  for (round_name in names_or_seq_along(sim_data)) {
    round_data <- sim_data[[round_name]]

    regrets <- round_data$regrets
    if (length(regrets) == 0) {
      next
    }

    if (identical(plottable, c())) {
      plottable <- c(regrets[[length(contestants)]])
    } else {
      plottable <- c(plottable, regrets[[length(contestants)]])
    }
  }

  # add the data to the plottables
  # if (hasName(plottables, the_series_label)) {
  #   the_original_series_label <- the_series_label
  #   label_index <- 1
  #   the_series_label <- paste(the_original_series_label, label_index)
  # }
  # while (hasName(plottables, the_series_label)) {
  #   label_index <- label_index + 1
  #   the_series_label <- paste(the_original_series_label, label_index)
  # }
  plottables[[the_series_label]] <- plottable

  # save the new plottables in the persistent stuff
  configuration$persistent_stuff$plottables <- plottables

  # convert it to an actually plottable format
  the_series <- names(plottables)
  plottables_dataframe <- as.data.frame(plottables)
  plottables_dataframe$rounds <- 1:configuration$rounds_per_simulation
  df <- reshape2::melt(plottables_dataframe, id.vars = "rounds", variable.name = "series")

  series_names <- names(plottables)

  gg_color_hue <- function(n) {
    hues <- seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }

  my_colors <- gg_color_hue(length(plottables))

  x_tick_vector <- c(1, 2, 3, 4, 5, 6, 7, 8)

  # plot it
  pictureplot <- ggplot(df, aes(rounds, value)) +
    geom_line(aes(colour = series)) +
    ggtitle("Plot of regrets for various situations") +
    ylab("regret") +
    coord_cartesian(xlim = c(1, 8)) +
    scale_color_manual(
      name = "series",
      labels = series_names,
      values = my_colors
    ) +
    scale_x_continuous(breaks = x_tick_vector)

  # print it
  print(pictureplot)

  # return the configuration
  return(configuration)
}


visualize_lambdas_with_player <- function(configuration) {
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


    for (competitor_index in sane_sequence(from = 1, to = length(contestants))) {
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

  the_title <- paste(
    "Plot of soccer data contestant lambdas,",
    "with 20x exaggerated loess trends."
  )

  x_tick_vector <- c(1, 2, 3, 4, 5, 6, 7, 8)

  # plot it
  pictureplot <- ggplot(df, aes(rounds, value)) +
    # geom_point(aes(colour = series), alpha = 0.75, size = 1) +
    geom_point(aes(colour = series), alpha = 0.75, size = 0.75) +
    geom_smooth(aes(colour = series), method = "loess", fill = NA) +
    ggtitle(the_title) +
    ylab("lambda") +
    scale_color_manual(
      name = "series",
      labels = the_series,
      values = my_colors
    ) +
    scale_x_continuous(breaks = x_tick_vector)

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
  # names(plottables) <- list("B365", "BW", "IW", "PS", "VC", "WH")
  names(plottables) <- list("uniform 1", "uniform 2")
  for (round_name in names_or_seq_along(sim_data)) {
    round_data <- sim_data[[round_name]]
    reality <- round_data$reality
    player_index <- length(round_data$parameters)

    weights <- round_data$parameters[[player_index]]$weights
    if (length(weights) == 0) {
      next
    }

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

  the_title <- "Plot of the player's weights for various contestants"

  x_tick_vector <- c(1, 2, 3, 4, 5, 6, 7, 8)

  # plot it
  pictureplot <- ggplot(df, aes(rounds, value, linetype = series, linewidth = series)) +
    geom_line(aes(colour = series)) +
    ggtitle(the_title) +
    ylab("weight") +
    scale_color_manual(
      name = "series",
      labels = the_series,
      values = my_colors
    ) +
    scale_linewidth_manual(values = c(1, 1), guide = "none") +
    scale_linetype_manual(values = c(1, 2), guide = "none") +
    scale_x_continuous(breaks = x_tick_vector)

  # print it
  print(pictureplot)

  # return the configuration
  return(configuration)
}


visualize_weights_binary <- function(configuration) {
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
  names(plottables) <- list("cheater", "wrong")
  for (round_name in names_or_seq_along(sim_data)) {
    round_data <- sim_data[[round_name]]
    reality <- round_data$reality
    player_index <- length(round_data$parameters)

    weights <- round_data$parameters[[player_index]]$weights
    if (length(weights) == 0) {
      next
    }

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

  the_title <- "Plot of the player's weights for various contestants"

  x_tick_vector <- c(1, 2, 3, 4, 5, 6, 7, 8)

  # plot it
  pictureplot <- ggplot(df, aes(rounds, value, linewidth = series)) +
    geom_line(aes(colour = series)) +
    ggtitle(the_title) +
    ylab("weight") +
    scale_color_manual(
      name = "series",
      labels = the_series,
      values = my_colors
    ) +
    scale_linewidth_manual(values = c(1, 1), guide = "none") +
    scale_x_continuous(breaks = x_tick_vector)

  # print it
  print(pictureplot)

  # return the configuration
  return(configuration)
}




visualize_losses_for_sim_analysis_no_crop <- function(configuration) {
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
  names(plottables) <- list("well-performing", "poorly performing", "learner", paste("minimum loss + ln", as.character(length(contestants) - 1)))
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

  series_names <- c("well-performing", "poorly performing", "learner", paste("minimum loss + ln", as.character(length(contestants) - 1)))

  gg_color_hue <- function(n) {
    hues <- seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }

  my_colors <- gg_color_hue(length(plottables))

  x_tick_vector <- c(1, 2, 3, 4, 5, 6, 7, 8)

  # plot it
  pictureplot <- ggplot(df, aes(rounds, value)) +
    geom_line(aes(colour = series)) +
    ggtitle("Plot of losses for the binary simulation") +
    ylab("loss") +
    scale_color_manual(
      name = "series",
      labels = series_names,
      values = my_colors
    ) +
    scale_x_continuous(breaks = x_tick_vector)

  # print it
  print(pictureplot)

  # return the configuration
  return(configuration)
}



visualize_data_toggles <- function(configuration) {
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

  long_contestants <- configuration$contestants

  # create a label for this plot line
  the_series_label <- paste0("|Ω|=", as.character(configuration$omega_size))
  threshold_tcw <- long_contestants[contestants == "toggle_cheater_wrong"][[1]]$parameters$T
  the_series_label <- paste0(the_series_label, ",\nwell-to-poor threshold=", as.character(threshold_tcw))
  threshold_twc <- long_contestants[contestants == "toggle_wrong_cheater"][[1]]$parameters$T
  the_series_label <- paste0(the_series_label, ",\npoor-to-well threshold=", as.character(threshold_twc))

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

  the_title <- "Plot of the player's predicted probability for the\ncorrect outcome, with one poor-to-well toggling\nexpert and one well-to-poor toggling expert"

  x_tick_vector <- c(1, 20, 40, 60, 80, 100)

  # plot it
  pictureplot <- ggplot(df, aes(rounds, value)) +
    geom_line(aes(colour = series)) +
    ggtitle(the_title) +
    ylab("probability") +
    scale_color_manual(
      name = "series",
      labels = the_series,
      values = my_colors
    ) +
    scale_x_continuous(breaks = x_tick_vector)

  # print it
  print(pictureplot)

  # return the configuration
  return(configuration)
}

visualize_excess_losses_normals <- function(configuration) {
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

  names_beforehand <- c(contestants[c(1, 2)], "learner", paste("minimum loss + ln", as.character(length(contestants) - 1)))
  long_contestants <- configuration$contestants

  if (contestants[[2]] == "normal") {
    normal_sd <- long_contestants[[2]]$parameters$sd
    names_beforehand[[2]] <- paste0("normal (sd=", as.character(normal_sd), ")")
  }
  if (contestants[[2]] == "normal_worsening") {
    normal_sd <- long_contestants[[2]]$parameters$sd
    names_beforehand[[2]] <- paste0("worsening normal\n(initial sd=", as.character(normal_sd), ")")
  }
  if (contestants[[2]] == "normal_improving") {
    normal_sd <- long_contestants[[2]]$parameters$sd
    names_beforehand[[2]] <- paste0("improving normal\n(initial sd=", as.character(normal_sd), ")")
  }
  if (contestants[[2]] == "toggle_cheater_wrong") {
    the_T <- long_contestants[[2]]$parameters$T
    names_beforehand[[2]] <- paste0("well-to-poor, T=", as.character(the_T))
  }
  if (contestants[[2]] == "toggle_wrong_cheater") {
    the_T <- long_contestants[[2]]$parameters$T
    names_beforehand[[2]] <- paste0("poor-to-well, T=", as.character(the_T))
  }
  if (contestants[[1]] == "normal") {
    normal_sd <- long_contestants[[1]]$parameters$sd
    names_beforehand[[1]] <- paste0("normal (sd=", as.character(normal_sd), ")")
  }
  if (contestants[[1]] == "normal_worsening") {
    normal_sd <- long_contestants[[1]]$parameters$sd
    names_beforehand[[1]] <- paste0("worsening normal\n(initial sd=", as.character(normal_sd), ")")
  }
  if (contestants[[1]] == "normal_improving") {
    normal_sd <- long_contestants[[1]]$parameters$sd
    names_beforehand[[1]] <- paste0("improving normal\n(initial sd=", as.character(normal_sd), ")")
  }
  if (contestants[[1]] == "toggle_cheater_wrong") {
    the_T <- long_contestants[[1]]$parameters$T
    names_beforehand[[1]] <- paste0("well-to-poor, T=", as.character(the_T))
  }
  if (contestants[[1]] == "toggle_wrong_cheater") {
    the_T <- long_contestants[[1]]$parameters$T
    names_beforehand[[1]] <- paste0("poor-to-well, T=", as.character(the_T))
  }

  # gather the data we want to visualize from this round
  plottables <- rep.int(list(c()), times = length(contestants) + 1)
  names(plottables) <- names_beforehand
  for (round_name in names_or_seq_along(sim_data)) {
    round_data <- sim_data[[round_name]]

    losses <- round_data$losses
    if (length(losses) == 0) {
      next
    }
    only_experts <- losses[sane_sequence(from = 1, to = length(contestants) - 1)]
    min_loss <- min(only_experts)

    losses <- c(losses, min_loss + log(length(contestants) - 1))

    excesses <- losses - min_loss
    # excesses <- losses
    # excesses <- losses - losses[[1]]

    for (contestant_index in sane_sequence(from = 1, to = length(contestants) + 1)) {
      if (identical(plottables[[contestant_index]], c())) {
        plottables[[contestant_index]] <- c(excesses[[contestant_index]])
      } else {
        plottables[[contestant_index]] <- c(plottables[[contestant_index]], excesses[[contestant_index]])
      }
    }
  }

  # convert it to an actually plottable format
  the_series <- names(plottables)
  plottables_dataframe <- as.data.frame(plottables)
  plottables_dataframe$rounds <- 1:configuration$rounds_per_simulation
  df <- reshape2::melt(plottables_dataframe, id.vars = "rounds", variable.name = "series")

  series_names <- unlist(names_beforehand)

  gg_color_hue <- function(n) {
    hues <- seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }

  my_colors <- gg_color_hue(length(plottables))

  x_tick_vector <- c(1, 20, 40, 60, 80, 100)
  if (configuration$rounds_per_simulation == 100) {
    x_tick_vector <- c(1, 20, 40, 60, 80, 100)
  } else if (configuration$rounds_per_simulation == 1000) {
    x_tick_vector <- c(1, 200, 400, 600, 800, 1000)
  } else if (configuration$rounds_per_simulation == 10) {
    x_tick_vector <- c(1, 2, 4, 6, 8, 10)
  } else if (configuration$rounds_per_simulation == 8) {
    x_tick_vector <- 1:8
  }

  # plot it
  pictureplot <- ggplot(df, aes(rounds, value)) +
    geom_line(aes(colour = series)) +
    ggtitle("Plot of loss excess beyond round minimum,\nfor various configurations") +
    ylab("loss excess") +
    scale_color_manual(
      name = "series",
      labels = series_names,
      values = my_colors
    ) +
    # coord_cartesian(ylim = c(0, 3)) +
    scale_x_continuous(breaks = x_tick_vector)

  # print it
  print(pictureplot)

  # return the configuration
  return(configuration)
}


visualize_weights_normals <- function(configuration) {
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

  names_beforehand <- contestants[c(1, 2)]
  long_contestants <- configuration$contestants

  if (contestants[[2]] == "normal") {
    normal_sd <- long_contestants[[2]]$parameters$sd
    names_beforehand[[2]] <- paste0("normal (sd=", as.character(normal_sd), ")")
  }
  if (contestants[[2]] == "normal_worsening") {
    normal_sd <- long_contestants[[2]]$parameters$sd
    names_beforehand[[2]] <- paste0("worsening normal\n(initial sd=", as.character(normal_sd), ")")
  }
  if (contestants[[2]] == "normal_improving") {
    normal_sd <- long_contestants[[2]]$parameters$sd
    names_beforehand[[2]] <- paste0("improving normal\n(initial sd=", as.character(normal_sd), ")")
  }
  if (contestants[[1]] == "normal") {
    normal_sd <- long_contestants[[1]]$parameters$sd
    names_beforehand[[1]] <- paste0("normal (sd=", as.character(normal_sd), ")")
  }
  if (contestants[[1]] == "normal_worsening") {
    normal_sd <- long_contestants[[1]]$parameters$sd
    names_beforehand[[1]] <- paste0("worsening normal\n(initial sd=", as.character(normal_sd), ")")
  }
  if (contestants[[1]] == "normal_improving") {
    normal_sd <- long_contestants[[1]]$parameters$sd
    names_beforehand[[1]] <- paste0("improving normal\n(initial sd=", as.character(normal_sd), ")")
  }

  # gather the data we want to visualize from this round
  plottables <- rep.int(list(c()), times = length(contestants) - 1)
  names(plottables) <- names_beforehand
  for (round_name in names_or_seq_along(sim_data)) {
    round_data <- sim_data[[round_name]]
    player_index <- length(round_data$parameters)

    weights <- round_data$parameters[[player_index]]$weights
    if (length(weights) == 0) {
      next
    }

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

  series_names <- unlist(names_beforehand)

  gg_color_hue <- function(n) {
    hues <- seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }

  my_colors <- gg_color_hue(length(plottables))

  x_tick_vector <- c(1, 20, 40, 60, 80, 100)
  if (configuration$rounds_per_simulation == 100) {
    x_tick_vector <- c(1, 20, 40, 60, 80, 100)
  } else if (configuration$rounds_per_simulation == 1000) {
    x_tick_vector <- c(1, 200, 400, 600, 800, 1000)
  } else if (configuration$rounds_per_simulation == 10) {
    x_tick_vector <- c(1, 2, 4, 6, 8, 10)
  } else if (configuration$rounds_per_simulation == 8) {
    x_tick_vector <- 1:8
  }

  # plot it
  pictureplot <- ggplot(df, aes(rounds, value)) +
    geom_line(aes(colour = series)) +
    ggtitle("Plot of learner's weights for contestants,\nfor various configurations") +
    ylab("weight") +
    scale_color_manual(
      name = "series",
      labels = series_names,
      values = my_colors
    ) +
    # coord_cartesian(ylim = c(0, 3)) +
    scale_x_continuous(breaks = x_tick_vector)

  # print it
  print(pictureplot)

  # return the configuration
  return(configuration)
}


visualize_weights_normalized_normals <- function(configuration) {
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

  names_beforehand <- contestants[c(1, 2)]
  long_contestants <- configuration$contestants

  if (contestants[[2]] == "normal") {
    normal_sd <- long_contestants[[2]]$parameters$sd
    names_beforehand[[2]] <- paste0("normal (sd=", as.character(normal_sd), ")")
  }
  if (contestants[[2]] == "normal_worsening") {
    normal_sd <- long_contestants[[2]]$parameters$sd
    names_beforehand[[2]] <- paste0("worsening normal\n(initial sd=", as.character(normal_sd), ")")
  }
  if (contestants[[2]] == "normal_improving") {
    normal_sd <- long_contestants[[2]]$parameters$sd
    names_beforehand[[2]] <- paste0("improving normal\n(initial sd=", as.character(normal_sd), ")")
  }
  if (contestants[[1]] == "normal") {
    normal_sd <- long_contestants[[1]]$parameters$sd
    names_beforehand[[1]] <- paste0("normal (sd=", as.character(normal_sd), ")")
  }
  if (contestants[[1]] == "normal_worsening") {
    normal_sd <- long_contestants[[1]]$parameters$sd
    names_beforehand[[1]] <- paste0("worsening normal\n(initial sd=", as.character(normal_sd), ")")
  }
  if (contestants[[1]] == "normal_improving") {
    normal_sd <- long_contestants[[1]]$parameters$sd
    names_beforehand[[1]] <- paste0("improving normal\n(initial sd=", as.character(normal_sd), ")")
  }

  # gather the data we want to visualize from this round
  plottables <- rep.int(list(c()), times = length(contestants) - 1)
  names(plottables) <- names_beforehand
  for (round_name in names_or_seq_along(sim_data)) {
    round_data <- sim_data[[round_name]]
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

  series_names <- unlist(names_beforehand)

  gg_color_hue <- function(n) {
    hues <- seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }

  my_colors <- gg_color_hue(length(plottables))

  x_tick_vector <- c(1, 20, 40, 60, 80, 100)
  if (configuration$rounds_per_simulation == 100) {
    x_tick_vector <- c(1, 20, 40, 60, 80, 100)
  } else if (configuration$rounds_per_simulation == 1000) {
    x_tick_vector <- c(1, 200, 400, 600, 800, 1000)
  } else if (configuration$rounds_per_simulation == 10) {
    x_tick_vector <- c(1, 2, 4, 6, 8, 10)
  } else if (configuration$rounds_per_simulation == 8) {
    x_tick_vector <- 1:8
  }

  # plot it
  pictureplot <- ggplot(df, aes(rounds, value)) +
    geom_line(aes(colour = series)) +
    ggtitle("Plot of learner's weights for contestants, normalized\nby round maximum, for various configurations") +
    ylab("normalized weight") +
    scale_color_manual(
      name = "series",
      labels = series_names,
      values = my_colors
    ) +
    # coord_cartesian(ylim = c(0, 3)) +
    scale_x_continuous(breaks = x_tick_vector)

  # print it
  print(pictureplot)

  # return the configuration
  return(configuration)
}



visualize_data_normals <- function(configuration) {
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


  names_beforehand <- c(contestants[c(1, 2)], "learner")
  long_contestants <- configuration$contestants

  if (contestants[[2]] == "normal") {
    normal_sd <- long_contestants[[2]]$parameters$sd
    names_beforehand[[2]] <- paste0("normal (sd=", as.character(normal_sd), ")")
  }
  if (contestants[[2]] == "normal_worsening") {
    normal_sd <- long_contestants[[2]]$parameters$sd
    names_beforehand[[2]] <- paste0("worsening normal\n(initial sd=", as.character(normal_sd), ")")
  }
  if (contestants[[2]] == "normal_improving") {
    normal_sd <- long_contestants[[2]]$parameters$sd
    names_beforehand[[2]] <- paste0("improving normal\n(initial sd=", as.character(normal_sd), ")")
  }
  if (contestants[[1]] == "normal") {
    normal_sd <- long_contestants[[1]]$parameters$sd
    names_beforehand[[1]] <- paste0("normal (sd=", as.character(normal_sd), ")")
  }
  if (contestants[[1]] == "normal_worsening") {
    normal_sd <- long_contestants[[1]]$parameters$sd
    names_beforehand[[1]] <- paste0("worsening normal\n(initial sd=", as.character(normal_sd), ")")
  }
  if (contestants[[1]] == "normal_improving") {
    normal_sd <- long_contestants[[1]]$parameters$sd
    names_beforehand[[1]] <- paste0("improving normal\n(initial sd=", as.character(normal_sd), ")")
  }


  # gather the data we want to visualize from this round
  plottables <- rep.int(list(c()), times = length(contestants))
  names(plottables) <- names_beforehand
  for (round_name in names_or_seq_along(sim_data)) {
    round_data <- sim_data[[round_name]]
    reality <- round_data$reality
    for (contestant_index in seq_along(round_data$guesses)) {
      the_prob <- round_data$guesses[[contestant_index]][[reality]]
      if (identical(plottables[[contestant_index]], c())) {
        plottables[[contestant_index]] <- c(the_prob)
      } else {
        plottables[[contestant_index]] <- c(plottables[[contestant_index]], the_prob)
      }
    }
  }

  # convert it to an actually plottable format
  the_series <- names(plottables)
  plottables_dataframe <- as.data.frame(plottables)
  plottables_dataframe$rounds <- 1:configuration$rounds_per_simulation
  df <- reshape2::melt(plottables_dataframe, id.vars = "rounds", variable.name = "series")

  series_names <- unlist(names_beforehand)

  gg_color_hue <- function(n) {
    hues <- seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }

  my_colors <- gg_color_hue(length(plottables))

  x_tick_vector <- c(1, 20, 40, 60, 80, 100)
  if (configuration$rounds_per_simulation == 100) {
    x_tick_vector <- c(1, 20, 40, 60, 80, 100)
  } else if (configuration$rounds_per_simulation == 1000) {
    x_tick_vector <- c(1, 200, 400, 600, 800, 1000)
  } else if (configuration$rounds_per_simulation == 10) {
    x_tick_vector <- c(1, 2, 4, 6, 8, 10)
  } else if (configuration$rounds_per_simulation == 8) {
    x_tick_vector <- 1:8
  }

  # plot it
  pictureplot <- ggplot(df, aes(rounds, value)) +
    geom_line(aes(colour = series)) +
    ggtitle("lot of the contestants' predicted probabilities for the\ncorrect outcome, for various configurations") +
    ylab("probability") +
    scale_color_manual(
      name = "series",
      labels = series_names,
      values = my_colors
    ) +
    scale_x_continuous(breaks = x_tick_vector)

  # print it
  print(pictureplot)

  # return the configuration
  return(configuration)
}



# TODO: write a script that simulates worsening-vs-static, improving-vs-static, ---
#   --- and improving-vs-worsening for tons of std dev combinations, ---
#   --- plotting a color map/3d graph of ln(2) - [eventual loss excess of ---
#   --- learner after 2000 rounds] for all those combinations, to see if ---
#   --- to see if we can find any pattern.
# TODO: weight graphs should pretend there are n+1 contestants for color purposes
# TODO: implement https://stackoverflow.com/a/52304499/18375328
