source("./program_parts/global_functions_stuff.r")
source("./program_parts/vovk_zhdanov_algorithm_stuff.r")

# always guesses a point mass on reality
cheater_guess <- function(parameters,
                          contestant_index,
                          configuration,
                          round_data,
                          round_index) {
    omega_size <- configuration$omega_size
    reality <- round_data$reality

    # create a sequence of zeros, and simply make the correct outcome a one
    guess <- rep.int(c(0), times = omega_size)
    guess[[reality]] <- 1

    return(list(guess = guess, parameters = parameters))
}

# always guesses a random point mass
confident_guess <- function(parameters,
                            contestant_index,
                            configuration,
                            round_data,
                            round_index) {
    omega_size <- configuration$omega_size

    # create a sequence of zeros, and put a one in some random outcome
    guess <- rep.int(c(0), times = omega_size)
    guess[[sample.int(omega_size, size = 1)]] <- 1

    return(list(guess = guess, parameters = parameters))
}

# always guesses a random point mass that's not centered at reality
wrong_guess <- function(parameters,
                        contestant_index,
                        configuration,
                        round_data,
                        round_index) {
    omega_size <- configuration$omega_size
    reality <- round_data$reality

    # if omega_size == 1, there aren't any wrong outcomes to choose from,
    # so we need to handle that case separately
    if (omega_size == 1) {
        return(list(guess = c(1), parameters = parameters))
    }

    # create a sequence of zeros, and put a one in some random non-correct outcome
    guess <- rep.int(c(0), times = omega_size)
    random_index <- sample.int(omega_size - 1, size = 1)
    if (random_index >= reality) {
        random_index <- random_index + 1
    }
    guess[[random_index]] <- 1

    return(list(guess = guess, parameters = parameters))
}

# always guesses the uniform distribution
uniform_guess <- function(parameters,
                          contestant_index,
                          configuration,
                          round_data,
                          round_index) {
    omega_size <- configuration$omega_size

    # create uniform sequence
    guess <- rep.int(c(1 / omega_size), times = omega_size)

    return(list(guess = guess, parameters = parameters))
}

# starts off using cheater, then switches to using wrong a certain number of rounds
toggle_cheater_wrong_guess <- function(parameters,
                                       contestant_index,
                                       configuration,
                                       round_data,
                                       round_index) {
    omega_size <- configuration$omega_size
    reality <- round_data$reality
    T_val <- parameters$T

    # toggle between cheater and wrong based on the round index
    if (round_index < T_val) {
        # create a sequence of zeros, and put a one in some random outcome
        guess <- rep.int(c(0), times = omega_size)
        guess[[sample.int(omega_size, size = 1)]] <- 1

        return(list(guess = guess, parameters = parameters))
    } else {
        # if omega_size == 1, there aren't any wrong outcomes to choose from,
        # so we need to handle that case separately
        if (omega_size == 1) {
            return(list(guess = c(1), parameters = parameters))
        }

        # create a sequence of zeros, and put a one in some random non-correct outcome
        guess <- rep.int(c(0), times = omega_size)
        random_index <- sample.int(omega_size - 1, size = 1)
        if (random_index >= reality) {
            random_index <- random_index + 1
        }
        guess[[random_index]] <- 1

        return(list(guess = guess, parameters = parameters))
    }
}

# starts off using wrong, then switches to using cheater a certain number of rounds
toggle_wrong_cheater_guess <- function(parameters,
                                       contestant_index,
                                       configuration,
                                       round_data,
                                       round_index) {
    omega_size <- configuration$omega_size
    reality <- round_data$reality
    T_val <- parameters$T

    # toggle between cheater and wrong based on the round index
    if (round_index < T_val) {
        # if omega_size == 1, there aren't any wrong outcomes to choose from,
        # so we need to handle that case separately
        if (omega_size == 1) {
            return(list(guess = c(1), parameters = parameters))
        }

        # create a sequence of zeros, and put a one in some random non-correct outcome
        guess <- rep.int(c(0), times = omega_size)
        random_index <- sample.int(omega_size - 1, size = 1)
        if (random_index >= reality) {
            random_index <- random_index + 1
        }
        guess[[random_index]] <- 1

        return(list(guess = guess, parameters = parameters))
    } else {
        # create a sequence of zeros, and put a one in some random outcome
        guess <- rep.int(c(0), times = omega_size)
        guess[[sample.int(omega_size, size = 1)]] <- 1

        return(list(guess = guess, parameters = parameters))
    }
}

# performs a fixed sequence of guesses
house_guess <- function(parameters,
                        contestant_index,
                        configuration,
                        round_data,
                        round_index) {
    house_probabilities <- parameters$house_probabilities

    # get the guess from the list of guesses and make sure it's a vector
    guess <- unlist(house_probabilities[[round_index]])

    return(list(guess = guess, parameters = parameters))
}

# consistently guesses a given guess
consistent_guess <- function(parameters,
                             contestant_index,
                             configuration,
                             round_data,
                             round_index) {
    guess <- parameters$guess

    # rescale the guess to normalize it
    guess <- guess / sum(guess)

    return(list(guess = guess, parameters = parameters))
}



#######################################################################################################################



stop_playstyle <- function(msg, contestant_index) {
    extra_msg <- paste0("Exception while compiling the ", contestant_index, "th contestant:\n")
    msg <- paste0(extra_msg, msg)
    stop(msg)
}

warn_playstyle <- function(msg, contestant_index) {
    extra_msg <- paste0("Warning while compiling the ", contestant_index, "th contestant:\n")
    msg <- paste0(extra_msg, msg)
    warning(msg)
}

needs_no_parameters_stuff <- function(playstyle_name, guessing_function) {
    result <- list()

    result$guessing_function <- guessing_function

    result$parameter_checker <- function(parameters, contestant_index, configuration) {
        playstyle_name <- get("playstyle_name", envir = parent.env(environment()), inherits = FALSE)
        if (!identical(parameters, list())) {
            msg <- paste("A(n)", playstyle_name, "contestant shouldn't take any parameters")
            stop_playstyle(msg, contestant_index)
        }
        return(list(playstyle = playstyle_name, parameters = parameters))
    }

    result$parameter_defaulter <- function(contestant_index, configuration) {
        playstyle_name <- get("playstyle_name", envir = parent.env(environment()), inherits = FALSE)
        return(list(playstyle = playstyle_name, parameters = list()))
    }

    return(result)
}

player_playstyle_stuff <- list(
    guessing_function = vovk_algorithm,
    parameter_checker = function(parameters, contestant_index, configuration) {
        if (!is.list(parameters)) {
            msg <- "Contestants shouldn't have a non-list as parameters entry."
            stop_playstyle(msg, contestant_index)
        }
        if (length(parameters) == 1) {
            parameters <- list(weights = unlist(parameters[[1]]))
        } else {
            parameters <- list(weights = unlist(parameters))
        }
        if (length(parameters$weights) == 0) {
            parameters$weights <- rep.int(c(1), contestant_index - 1)
        }
        if (length(parameters$weights) != contestant_index - 1) {
            msg <- "A player contestant should have exactly as many weights as there are contestants before it."
            stop_playstyle(msg, contestant_index)
        }
        if (!is.numeric(parameters$weights)) {
            msg <- "A player contestant should only have numeric weights."
            stop_playstyle(msg, contestant_index)
        }
        if (min(parameters$weights) < 0 || max(parameters$weights) <= 0 || max(parameters$weights) > 1) {
            msg <- "player weights should lie in the interval [0, 1] and should not all be zero."
            stop_playstyle(msg, contestant_index)
        }
        return(list(playstyle = "player", parameters = parameters))
    },
    parameter_defaulter = function(contestant_index, configuration) {
        return(list(
            playstyle = "player",
            parameters = list(weights = rep.int(c(1), contestant_index - 1))
        ))
    }
)

toggle_cheater_wrong_playstyle_stuff <- list(
    guessing_function = toggle_cheater_wrong_guess,
    parameter_checker = function(parameters, contestant_index, configuration) {
        if (!is.list(parameters)) {
            msg <- "Contestants shouldn't have a non-list as parameters entry"
            stop_playstyle(msg, contestant_index)
        }
        if (length(parameters) == 1) {
            parameters <- list(T = parameters[[1]])
        } else {
            msg <- "A toggle contestant must have exactly one parameter."
            stop_playstyle(msg, contestant_index)
        }
        if (!is.numeric(parameters$T)) {
            msg <- "A toggle contestant should have numeric T."
            stop_playstyle(msg, contestant_index)
        }
        if (parameters$T < 1 || parameters$T > configuration$rounds_per_simulation + 1) {
            msg <- "A toggle contestant should have 1 <= T <= rounds_per_simulation + 1."
            stop_playstyle(msg, contestant_index)
        }
        if (length(parameters$T) != 1) {
            msg <- "A toggle contestant should only have a single T"
            stop_playstyle(msg, contestant_index)
        }
        return(list(playstyle = "toggle_cheater_wrong", parameters = parameters))
    },
    parameter_defaulter = function(contestant_index, configuration) {
        return(list(
            playstyle = "toggle_cheater_wrong",
            parameters = list(T = configuration$rounds_per_simulation / 2)
        ))
    }
)

toggle_wrong_cheater_playstyle_stuff <- list(
    guessing_function = toggle_wrong_cheater_guess,
    parameter_checker = function(parameters, contestant_index, configuration) {
        if (!is.list(parameters)) {
            msg <- "Contestants shouldn't have a non-list as parameters entry"
            stop_playstyle(msg, contestant_index)
        }
        if (length(parameters) == 1) {
            parameters <- list(T = parameters[[1]])
        } else {
            msg <- "A toggle contestant must have exactly one parameter."
            stop_playstyle(msg, contestant_index)
        }
        if (!is.numeric(parameters$T)) {
            msg <- "A toggle contestant should have numeric T."
            stop_playstyle(msg, contestant_index)
        }
        if (parameters$T < 1 || parameters$T > configuration$rounds_per_simulation + 1) {
            msg <- "A toggle contestant should have 1 <= T <= rounds_per_simulation + 1."
            stop_playstyle(msg, contestant_index)
        }
        if (length(parameters$T) != 1) {
            msg <- "A toggle contestant should only have a single T"
            stop_playstyle(msg, contestant_index)
        }
        return(list(playstyle = "toggle_wrong_cheater", parameters = parameters))
    },
    parameter_defaulter = function(contestant_index, configuration) {
        return(list(
            playstyle = "toggle_wrong_cheater",
            parameters = list(T = configuration$rounds_per_simulation / 2)
        ))
    }
)

house_playstyle_stuff <- list(
    guessing_function = house_guess,
    parameter_checker = function(parameters, contestant_index, configuration) {
        if (!is.list(parameters)) {
            msg <- "Contestants shouldn't have a non-list as parameters entry"
            stop_playstyle(msg, contestant_index)
        }
        if (length(parameters) == 1) {
            parameters <- list(house_probabilities = parameters[[1]])
        } else {
            parameters <- list(house_probabilities = parameters)
        }
        if (length(parameters$house_probabilities) == 0) {
            msg <- "a House contestant must have one parameter."
            stop_playstyle(msg, contestant_index)
        }
        if (is.character(parameters$house_probabilities)) {
            parameters <- list(house_probabilities = readRDS(parameters$house_probabilities))
        }
        if (!is.list(parameters$house_probabilities)) {
            msg <- "A house contestant should have a list of house probabilities or a filename"
            msg <- paste0(msg, " with a list of house properties as its parameter.")
            stop_playstyle(msg, contestant_index)
        }
        if (length(parameters$house_probabilities) < configuration$rounds_per_simulation) {
            msg <- "A house contestant should have probabilities for every round of the simulation."
            stop_playstyle(msg, contestant_index)
        }
        if (length(parameters$house_probabilities) > configuration$rounds_per_simulation) {
            msg <- "There are more house probabilities than there are rounds in the simulation.\n"
            msg <- paste0(msg, "Consider increasing simulation size.")
            warn_playstyle(msg, contestant_index)
        }
        if (configuration$rounds_per_simulation > 0) {
            if (!is.numeric(parameters$house_probabilities[[1]]) || is.list(parameters$house_probabilities[[1]])) {
                msg <- "A house contestant should have only numerical vectors"
                msg <- paste(msg, " in its list of house probabilities.", sep = "")
                stop_playstyle(msg, contestant_index)
            }
            if (length(parameters$house_probabilities[[1]]) != configuration$omega_size) {
                msg <- "A house contestant should have as many probabilities for every"
                msg <- paste(msg, " round as there are outcomes in omega.", sep = "")
                stop_playstyle(msg, contestant_index)
            }
        }
        return(list(playstyle = "house", parameters = parameters))
    },
    parameter_defaulter = function(contestant_index, configuration) {
        msg <- "You can't leave the parameter for a house contestant unspecified."
        stop_playstyle(msg, contestant_index)
    }
)

consistent_playstyle_stuff <- list(
    guessing_function = consistent_guess,
    parameter_checker = function(parameters, contestant_index, configuration) {
        if (!is.list(parameters)) {
            msg <- "Contestants shouldn't have a non-list as parameters entry"
            stop_playstyle(msg, contestant_index)
        }
        if (length(parameters) == 1) {
            parameters <- list(guess = unlist(parameters[[1]]))
        } else {
            parameters <- list(guess = unlist(parameters))
        }
        if (length(parameters$guess) != configuration$omega_size) {
            msg <- "A consistent contestant should have as many guess entries as there are outcomes."
            stop_playstyle(msg, contestant_index)
        }
        if (!is.numeric(parameters$guess)) {
            msg <- "A consistent contestant should only have numeric guess entries."
            stop_playstyle(msg, contestant_index)
        }
        if (min(parameters$guess) < 0 || max(parameters$guess) <= 0) {
            msg <- "consistent contestant guess entries should be nonnegative and at not all zero."
            stop_playstyle(msg, contestant_index)
        }
        return(list(playstyle = "consistent", parameters = parameters))
    },
    parameter_defaulter = function(contestant_index, configuration) {
        msg <- "You can't leave the parameter for a consistent contestant unspecified."
        stop_playstyle(msg, contestant_index)
    }
)

initial_playstyles <- list(
    player = player_playstyle_stuff,
    cheater = needs_no_parameters_stuff("cheater", cheater_guess),
    confident = needs_no_parameters_stuff("confident", confident_guess),
    wrong = needs_no_parameters_stuff("wrong", wrong_guess),
    uniform = needs_no_parameters_stuff("uniform", uniform_guess),
    toggle_cheater_wrong = toggle_cheater_wrong_playstyle_stuff,
    toggle_wrong_cheater = toggle_wrong_cheater_playstyle_stuff,
    house = house_playstyle_stuff,
    consistent = consistent_playstyle_stuff
)
