source("./program_parts/global_functions_stuff.r")

# TODO: make sure everything is rendered as vector in other files too.

# Vovk-Zhdanov algorith for optimal strategy in the Brier game
vovk_algorithm <- function(parameters, player_index, configuration, round_data, round_index) {
    omega_size <- configuration$omega_size
    omega <- sane_sequence(from = 1, to = omega_size)
    contestants_so_far <- sane_sequence(from = 1, to = player_index - 1)
    current_weights <- parameters$weights
    contestant_guesses <- round_data$guesses
    reality <- round_data$reality # for updating weights afterwards

    # if there are no contestants before the player, return a uniform guess
    if (player_index == 1) {
        guess <- rep.int(c(1 / omega_size), times = omega_size)
        return(list(guess = guess, parameters = parameters))
    }

    # set up G values and make efficient use of computations to also calculate updated weights

    # set up G values; one for every outcome o, based on the formula:
    # G[o] := -ln(sum(i in contestants so far) {previous_weights[i] * exp(-lambda(contestant_guesses[i], o))}).
    # also update weights, because we'd have to calculate stuff multiple times if we only did that at the end.
    # for the weights, we use the formula
    # next_weight[i] := current_weight[i] * exp(-lambda(contestant_guesses[i], reality)).
    g_values <- c()
    next_weights <- c()
    for (outcome in omega) {
        # we calculate what the lambdas of the preceding contestants would be if the outcome were the reality
        lambdas <- c()
        for (contestant_index in contestants_so_far) {
            lambdas <- c(lambdas, calculate_lambda(contestant_guesses[[contestant_index]], outcome))
        }
        # and we use those values to calculate the G value for this outcome
        g_values <- c(g_values, -log(sum(current_weights * exp(-lambdas))))

        # if reality == outcome, we need those lambdas for updating weights, so why not just do it now?
        if (outcome == reality) {
            next_weights <- current_weights * exp(-lambdas)
        }
    }

    # calculate the optimal solution s for the equation
    # sum(o in omega) {max(0, s - G[o])} == 2
    s <- calculate_optimal_s(g_values, omega)

    # create the distribution where outcome o gets probability max(0, s - G[o]) / 2.
    guess <- pmax(0, s - g_values) / 2

    # replace the weights in the parameters
    parameters$weights <- next_weights

    # return the correct object to return
    return(list(guess = guess, parameters = parameters))
} # Vovk-Zhdanov algorithm. https://www.jmlr.org/papers/volume10/vovk09a/vovk09a.pdf, alg. 1.

# function for finding an optimal solution to a certain problem relevant to the Vovk-Zhdanov algorithm.
calculate_optimal_s <- function(g_values, omega_subset) {
    # the idea is as follows: we want
    # f(s) := sum(o in omega) {max(0, s - G[o])}
    # to be exactly 2.

    # deal with malformed edge case where omega_subset is empty
    if (identical(omega_subset, NULL)) {
        return(Inf)
    }

    # find optimal solution s under assumption that it will satisfy s >= max(G)
    s <- get_average(g_values, omega_subset) + 2 / length(omega_subset)

    # check whether we actually have s >= max(G). If so, that's the true optimal solution,
    # because we then have f(s) == sum(o in omega) {s - G[o]} == 2.
    if (s >= get_maximum(g_values, omega_subset)) {
        return(s)
    }

    # discard all elements o in omega for which we have s < G[o],
    # because we have max(0, s - G[o]) == 0 for all such o.
    # We do this by constructing {o in omega_subset | s >= G[o]},
    # because the nature of this algorithm ensures that we have
    # s < G[o] whenever o is not an element of omega_subset.
    trimmed_omega_subset <- c()
    for (outcome in omega_subset) {
        if (s >= g_values[[outcome]]) {
            trimmed_omega_subset <- c(trimmed_omega_subset, outcome)
        }
    }

    # deal with edge case where we're left with an empty set due to floating point error
    if (identical(trimmed_omega_subset, NULL)) {
        return(s)
    }

    # iteratively/recursively apply algorithm to this trimmed subset.
    # The new_s found by this next iteration using the trimmed subset
    # will be no greater than the s found above using the original omega_subset,
    # and thus we still have new_s < G[o] whenever s < G[o], for all o in omega_subset.
    # Thus, there's no possibility of discarded elements of omega_subset becoming relevant again in a later iteration.
    # So the algorithm is valid.
    return(calculate_optimal_s(g_values, trimmed_omega_subset))
} # recursively finds and then returns s that solves sum(o in omega_subset) {max(0, s - G[o])} == 2

# helper functions for Vovk-Zhdanov algorithm
get_average <- function(g_values, omega_subset) {
    running_average <- 0
    for (outcome in omega_subset) {
        running_average <- running_average + g_values[[outcome]] / length(omega_subset)
    }
    return(running_average)
} # computes the average of G over a subset of omega, with 0 if it's empty

get_maximum <- function(g_values, omega_subset) {
    running_maximum <- -Inf
    for (outcome in omega_subset) {
        if (running_maximum < g_values[[outcome]]) {
            running_maximum <- g_values[[outcome]]
        }
    }
    return(running_maximum)
} # computes the maximum of G over a subset of omega, with -Inf if it's empty.
