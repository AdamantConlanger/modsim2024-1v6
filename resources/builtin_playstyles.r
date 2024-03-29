source("./program_parts/global_functions_stuff.r")

# always guesses a point mass on reality
cheater_guess <- function(parameters, contestant_index, round_config) {
    omega_size <- round_config$round_settings$omega_size
    reality <- round_config$cur_round_data$reality

    # create a sequence of zeros, and simply make the correct outcome a one
    guess <- rep.int(c(0), times = omega_size)
    guess[[reality]] <- 1

    return(list(guess = guess, parameters = parameters))
}

# always guesses a random point mass
confident_guess <- function(parameters, contestant_index, round_config) {
    omega_size <- round_config$round_settings$omega_size

    # create a sequence of zeros, and put a one in some random outcome
    guess <- rep.int(c(0), times = omega_size)
    guess[[sample.int(omega_size, size = 1)]] <- 1

    return(list(guess = guess, parameters = parameters))
}

# always guesses a random point mass that's not centered at reality
wrong_guess <- function(parameters, contestant_index, round_config) {
    omega_size <- round_config$round_settings$omega_size
    reality <- round_config$cur_round_data$reality

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
    resulting_guess[[random_index]] <- 1

    return(list(guess = guess, parameters = parameters))
}

# always guesses the uniform distribution
uniform_guess <- function(parameters, contestant_index, round_config) {
    omega_size <- round_config$round_settings$omega_size

    # create uniform sequence
    guess <- rep.int(c(1 / omega_size), times = omega_size)

    return(list(guess = guess, parameters = parameters))
}

# starts off using cheater, then switches to using wrong a certain number of rounds
toggle_cheater_wrong_guess <- function(parameters, contestant_index, round_config) {
    omega_size <- round_config$round_settings$omega_size
    round_index <- round_config$round_settings$round_index
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
        resulting_guess[[random_index]] <- 1

        return(list(guess = guess, parameters = parameters))
    }
}

# starts off using wrong, then switches to using cheater a certain number of rounds
toggle_wrong_cheater_guess <- function(parameters, contestant_index, round_config) {
    omega_size <- round_config$round_settings$omega_size
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
        resulting_guess[[random_index]] <- 1

        return(list(guess = guess, parameters = parameters))
    } else {
        # create a sequence of zeros, and put a one in some random outcome
        guess <- rep.int(c(0), times = omega_size)
        guess[[sample.int(omega_size, size = 1)]] <- 1

        return(list(guess = guess, parameters = parameters))
    }
}

# performs a fixed sequence of guesses
house_guess <- function(parameters, contestant_index, round_config) {
    round_index <- round_config$round_settings$round_index
    house_probabilities <- parameters$house_probabilities

    # get the guess from the list of guesses and make sure it's a vector
    guess <- unlist(house_probabilities[[round_index]])

    return(list(guess = guess, parameters = parameters))
}

# consistently guesses a given guess
consistent_guess <- function(parameters, contestant_index, round_config) {
    guess <- parameters$guess

    # rescale the guess to normalize it
    guess <- guess / sum(guess)

    return(list(guess = guess, parameters = parameters))
}
