source("./program_parts/global_functions_stuff.r")

# turns a list of bare-bones playstyles into a more useful format for the configuration module
playstyles_compiler <- function(initial_playstyles, working_configuration) {
    # compiles the initial_playstyles into the following:
    # a list "compiled_aliases" with all substitution macros e.g. "clown_cubed" (including descriptions).
    # a list "compiled_playstyles_guessing_functions" with the guessing functions of the non-alias playstyles.
    # a list "compiled_playstyles_both_defaulters" with functions for defaulting children and parameters.
    # a list "compiled_playstyles_children_defaulters" with functions for defaulting children based on parameters.
    # a list "compiled_playstyles_parameters_defaulters" with functions for defaulting parameters based on children.
    # a list "compiled_playstyles_neither_defaulters" with functions for defaulting in other cases.

    # make sure every playstyle is actually named, cause, well, it needs to be identifiable
    the_names <- names(initial_playstyles)
    if (identical(the_names, NULL) && !identical(initial_playstyles, list())) {
        stop("No playstyle should be without name. But one of them is.")
    }
    if ("" %in% the_names) {
        stop("No playstyle should be without name. But one of them is.")
    }

    # declare the lists we'll be returning
    compiled_aliases <- list()
    compiled_playstyles_guessing_functions <- list()
    compiled_playstyles_both_defaulters <- list()
    compiled_playstyles_children_defaulters <- list()
    compiled_playstyles_parameters_defaulters <- list()
    compiled_playstyles_neither_defaulters <- list()

    # go through all playstyles and put the right things in the right lists
    for (name in the_names) {
        # make sure the playstyle description is a list with at least one element
        if (!is.list(initial_playstyles[[name]])) {
            stop(paste0("Playstyle \"", name, "\" was uncompilable."))
        }
        if (length(initial_playstyles[[name]]) == 0) {
            stop(paste0("Playstyle \"", name, "\" was uncompilable."))
        }

        # check whether the first element is a string. If so, we're dealing with an alias.
        if (is.character(initial_playstyles[[name]][[1]])) {
            # we're dealing with an alias
            compiled_aliases[[name]] <- initial_playstyles[[name]]
        } else {
            # we're dealing with a non-alias
            compiled_playstyles_guessing_functions[[name]] <- initial_playstyles[[name]]$guessing_function
            compiled_playstyles_both_defaulters[[name]] <- initial_playstyles[[name]]$both_default_function
            compiled_playstyles_children_defaulters[[name]] <- initial_playstyles[[name]]$children_default_function
            compiled_playstyles_parameters_defaulters[[name]] <- initial_playstyles[[name]]$parameters_default_function
            compiled_playstyles_neither_defaulters[[name]] <- initial_playstyles[[name]]$neither_default_function
        }
    }

    # put all the lists in one single list and return it
    return(list(
        compiled_aliases = compiled_aliases,
        compiled_playstyles_guessing_functions = compiled_playstyles_guessing_functions,
        compiled_playstyles_both_defaulters = compiled_playstyles_both_defaulters,
        compiled_playstyles_children_defaulters = compiled_playstyles_children_defaulters,
        compiled_playstyles_parameters_defaulters = compiled_playstyles_parameters_defaulters,
        compiled_playstyles_neither_defaulters = compiled_playstyles_neither_defaulters
    ))
} # compiles initial_playstyles into separate lists of things

# turns a list of user-declared contestants into a more useful format for the configuration module
contestants_primer <- function(initial_contestants,
                               compiled_aliases,
                               compiled_playstyles_both_defaulters,
                               compiled_playstyles_children_defaulters,
                               compiled_playstyles_parameters_defaulters,
                               compiled_playstyles_neither_defaulters,
                               working_configuration) {
    # declare list of contestants we'll eventually return
    resulting_contestants <- list()

    # read the current proper names of the contestants. Blame Milan for this.
    initial_contestants_proper_names <- names(initial_contestants)
    if (identical(initial_contestants_proper_names, NULL)) {
        initial_contestants_proper_names <- rep.int(list(""), length(initial_contestants))
    }

    # for each contestant:
    for (contestant_index in seq_along(initial_contestants)) {
        # prime the current contestant
        contestant <- initial_contestants[[contestant_index]]
        resulting_contestant <- contestant_primer(
            contestant,
            contestant_index,
            compiled_aliases,
            compiled_playstyles_both_defaulters,
            compiled_playstyles_children_defaulters,
            compiled_playstyles_parameters_defaulters,
            compiled_playstyles_neither_defaulters,
            working_configuration,
            1
        )

        # give the current contestant a proper name if it doesn't have one yet. Blame Milan for this.
        contestant_proper_name <- initial_contestants_proper_names[[contestant_index]]
        if (contestant_proper_name == "") {
            contestant_proper_name <- create_random_proper_name(
                names(resulting_contestants),
                initial_contestants_proper_names
            )
        }
        list_to_add <- list(resulting_contestant)
        names(list_to_add) <- c(contestant_proper_name)
        resulting_contestants <- c(resulting_contestants, list_to_add)
    }

    # return the compiled contestant list
    return(resulting_contestants)
} # primes (i.e. compiles and give a proper name to) every contestant in a list and returns the list.

# creates a random proper name for the contestants_primer. Blame Milan for this.
create_random_proper_name <- function(resulting_contestants_names,
                                      initial_contestants_proper_names) {
    # TODO: make proper name be a parameter or something instead of the label in the list
    names_so_far <- c(initial_contestants_proper_names, resulting_contestants_names)
    source("./resources/proper_names.r", local = TRUE) # loads proper_names_list
    source("./resources/proper_names_custom.r", local = TRUE) # loads proper_names_custom_list
    proper_names_custom_list <- unique(proper_names_custom_list)
    proper_names_list <- union(proper_names_list, proper_names_custom_list) # auto-applies unique()
    proper_names_list <- setdiff(proper_names_list, names_so_far) # auto-applies unique() to everything
    if (length(proper_names_list) == 0) {
        stop("No more proper names to choose from. Exiting.")
    }
    ratio <- length(proper_names_custom_list) / length(proper_names_list)
    prob <- ifelse(length(proper_names_custom_list) > 0, max(ratio, 0.05), 0)
    if (runif(1) < prob) {
        return(proper_names_custom_list[[sample.int(length(proper_names_custom_list), size = 1)]])
    }
    return(proper_names_list[[sample.int(length(proper_names_list), size = 1)]])
} # returns a random proper name that hasn't been picked before.

contestant_primer <- function(contestant,
                              contestant_index,
                              compiled_aliases,
                              compiled_playstyles_both_defaulters,
                              compiled_playstyles_children_defaulters,
                              compiled_playstyles_parameters_defaulters,
                              compiled_playstyles_neither_defaulters,
                              working_configuration,
                              level) {
    # contestant: the contestant to prime (in its entirety).
    # contestant_index: the index of the progenitor contestant in the contestant list,
    #  --- i.e. the index at which the current contestant will be asked to bring out their guess.
    # compiled_aliases e.d.: objects specifying the way playstyles should be compiled.
    # working_configuration: the currently determined application settings.
    # level: the current level of recursion of the primer function.


    # if level is above 100, we're probably in some recursive loop. Let's error in that case, shall we?
    if (level > 100) {
        stop(paste("Reached a recursive level of", level, "in priming contestants. Exiting."))
    }

    empty_statement <- TRUE

    # if the contestant is a playstyle identifier, replace it by a full-fledged contestant
    if (is.character(contestant)) {
        identifier <- contestant
        if (hasName(compiled_aliases, identifier)) {
            # we're dealing with an alias playstyle
            contestant <- contestant_primer(
                compiled_aliases[[identifier]],
                contestant_index,
                compiled_aliases,
                compiled_playstyles_both_defaulters,
                compiled_playstyles_children_defaulters,
                compiled_playstyles_parameters_defaulters,
                compiled_playstyles_neither_defaulters,
                working_configuration,
                level + 1
            )
        } else if (hasName(compiled_playstyles_both_defaulters, identifier)) {
            # we're dealing with a non-alias playstyle
            contestant <- list(playstyle = identifier)
        } else {
            # we're dealing with an unknown playstyle
            stop(paste("The playstyle", identifier, "is not recognized."))
        }
    }

    # if the contestant isn't a list or doesn't specify a playstyle, error
    if (!is.list(contestant)) {
        msg <- "Woah, woah, woah. You're trying to prime a contestant that's neither in list nor an identifier."
        try(msg <- paste0(msg, "\nThe contestant was: ", contestant, "."))
        stop(msg)
    } else if (length(contestant) == 0) {
        msg <- "Woah, woah, woah. You're trying to prime a contestant that doesn't have a playstyle."
        try(msg <- paste0(msg, "\nThe contestant was: ", contestant, "."))
        stop(msg)
    } else if (!is.character(contestant[[1]])) {
        msg <- "Woah, woah, woah. You're trying to prime a contestant that doesn't have a playstyle."
        try(msg <- paste0(msg, "\nThe contestant was: ", contestant, "."))
        stop(msg)
    }

    # if the playstyle is not a named list entry, make it one
    if (!hasName(contestant, "playstyle")) {
        if (identical(names(contestant), NULL)) {
            names(contestant) <- rep.int(list(""), times = length(contestant))
        }
        names(contestant)[[1]] <- "playstyle"
    }

    # depending on the combination of children and parameters given or not given, set the defaults accordingly.

    # first, we determine whether or not the contestant has children cq. parameters
    has_children <- hasName(contestant, "children")
    if (has_children) {
        has_children <- is.list(contestant$children) && length(contestant$children) != 0
    }
    has_parameters <- hasName(contestant, "parameters")
    if (has_parameters) {
        has_parameters <- is.list(contestant$parameters) && length(contestant$parameters) != 0
    }

    empty_statement <- TRUE

    # then, we call the relevant defaulter function on the contestant
    if (has_children && has_parameters) {
        # the contestant has both children and parameters
        contestant <- compiled_playstyles_neither_defaulters[[contestant$playstyle]](
            contestant$children,
            contestant$parameters,
            contestant_index,
            working_configuration
        )
    } else if (has_children && !has_parameters) {
        # the contestant has children, but no parameters
        contestant <- compiled_playstyles_parameters_defaulters[[contestant$playstyle]](
            contestant$children,
            contestant_index,
            working_configuration
        )
    } else if (!has_children && has_parameters) {
        # the contestant has parameters, but no children
        contestant <- compiled_playstyles_children_defaulters[[contestant$playstyle]](
            contestant$parameters,
            contestant_index,
            working_configuration
        )
    } else {
        # the contestant has neither children nor parameters
        contestant <- compiled_playstyles_both_defaulters[[contestant$playstyle]](
            contestant_index,
            working_configuration
        )
    }

    # just to assure that the parameters list and children list are actual lists and not vectors:
    contestant$children <- as.list(contestant$children)
    contestant$parameters <- as.list(contestant$parameters)

    # recursively prime all children
    for (child_index in seq_along(contestant$children)) {
        contestant$children[[child_index]] <- contestant_primer(
            contestant$children[[child_index]],
            contestant_index,
            compiled_aliases,
            compiled_playstyles_both_defaulters,
            compiled_playstyles_children_defaulters,
            compiled_playstyles_parameters_defaulters,
            compiled_playstyles_neither_defaulters,
            working_configuration,
            level + 1
        )
    }

    # return the fully compiled and primed contestant
    return(contestant)
}

parse_configuration <- function() {
    # define default values for the settings
    default_configuration <- list(
        suite_size = 1,
        rounds_per_simulation = 10,
        omega_size = 2,
        data_export_filename = "outputs/exported_data.rds",
        display_output_filename = "outputs/display_output.json",
        use_seed = FALSE,
        custom_seed = 4219 * 0106 * 413,
        show_contestant_overview = FALSE,
        regret_type = "both",
        use_user_realities = FALSE,
        user_realities = list()
    )
    # define default playstyles (variable called default_playstyles)
    source("./resources/builtin_playstyles.r") # TODO: make this define the right stuff

    # source "configuration" and "contestants" from the user config file
    source("./inputs/user_config.r", local = TRUE)

    # give the objects sourced from the user config file names reflecting their origin
    if (exists("configuration", inherits = FALSE)) {
        sourced_configuration <- configuration
    } else {
        sourced_configuration <- list()
    }
    if (exists("contestants", inherits = FALSE)) {
        sourced_contestants <- contestants
    } else {
        sourced_contestants <- list()
    }

    # merge the default and sourced objects together
    initial_configuration <- sane_modify_list(default_configuration, sourced_configuration)
    initial_contestants <- sourced_contestants
    initial_playstyles <- default_playstyles

    # create provisional resultant compiled_configuration object
    working_configuration <- list()

    # port over the information we'll need for further compiling into the provisional object
    working_configuration$suite_size <- initial_configuration$suite_size
    working_configuration$rounds_per_simulation <- initial_configuration$rounds_per_simulation
    working_configuration$omega_size <- initial_configuration$omega_size
    working_configuration$data_export_filename <- initial_configuration$data_export_filename
    working_configuration$display_output_filename <- initial_configuration$display_output_filename
    working_configuration$show_contestant_overview <- initial_configuration$show_contestant_overview
    working_configuration$use_seed <- initial_configuration$use_seed
    working_configuration$custom_seed <- initial_configuration$custom_seed
    working_configuration$regret_type <- initial_configuration$regret_type
    working_configuration$use_user_realities <- initial_configuration$use_user_realities
    working_configuration$user_realities <- initial_configuration$user_realities

    # sanity checking the settings provided
    if (working_configuration$suite_size < 0) {
        stop("suite_size must be nonnegative.")
    }
    if (working_configuration$rounds_per_simulation < 0) {
        stop("rounds_per_simulation must be nonnegative.")
    }
    if (working_configuration$omega_size < 1) {
        stop("omega_size must be strictly positive.")
    }
    if (!is.character(working_configuration$data_export_filename)) {
        stop("data_export_filename must be a filename.")
    }
    if (!is.character(working_configuration$display_output_filename)) {
        stop("display_output_filename must be a filename.")
    }
    if (!is.logical(working_configuration$show_contestant_overview)) {
        stop("show_contestant_overview must be a logical value.")
    }
    if (!is.logical(working_configuration$use_seed)) {
        stop("use_seed must be a logical value.")
    }
    if (!sane_is_integer(working_configuration$custom_seed)) {
        stop("custom_seed must be an integer.")
    }
    if (!(working_configuration$regret_type %in% c("diachronic", "synchronic", "relative", "global", "both"))) {
        stop("regret_type must be diachronic, synchronic, relative, global, or both (with former two deprecated).")
    }
    if (working_configuration$regret_type == "diachronic") working_configuration$regret_type <- "relative"
    if (working_configuration$regret_type == "synchronic") working_configuration$regret_type <- "global"
    if (!is.logical(working_configuration$use_user_realities)) {
        stop("use_user_realities must be a logical value.")
    }
    if (working_configuration$use_user_realities) {
        if (
            !is.list(working_configuration$user_realities) ||
                length(working_configuration$user_realities) != working_configuration$rounds_per_simulation
        ) {
            stop("user_realities must be a list with one outcome as reality for every round.")
        }
        if (
            working_configuration$rounds_per_simulation > 0 &&
                (
                    !all(sane_is_integer(unlist(working_configuration$user_realities))) ||
                        max(unlist(working_configuration$user_realities)) > working_configuration$omega_size ||
                        min(unlist(working_configuration$user_realities)) < 1
                )
        ) {
            stop("user_realities must be a list with one outcome as reality for every round.")
        }
    }

    # set random seed
    if (working_configuration$use_seed) {
        set.seed(working_configuration$custom_seed)
    } else {
        set.seed(NULL)
    }

    # compile the initial_playstyles into the following:
    # a list "compiled_aliases" with all substitution macros e.g. "clown_cubed" (including descriptions).
    # a list "compiled_playstyles_guessing_functions" with the guessing functions of the non-alias playstyles.
    # a list "compiled_playstyles_both_defaulters" with functions for defaulting children and parameters.
    # a list "compiled_playstyles_children_defaulters" with functions for defaulting children based on parameters.
    # a list "compiled_playstyles_parameters_defaulters" with functions for defaulting parameters based on children.
    # a list "compiled_playstyles_neither_defaulters" with functions for defaulting in other cases.
    playstyles_compiler_result <- playstyles_compiler(initial_playstyles, working_configuration)
    # TODO: make the playstyle compiling happen in the playstyles file
    compiled_aliases <- playstyles_compiler_result$compiled_aliases
    compiled_playstyles_guessing_functions <- playstyles_compiler_result$compiled_playstyles_guessing_functions
    compiled_playstyles_both_defaulters <- playstyles_compiler_result$compiled_playstyles_both_defaulters
    compiled_playstyles_children_defaulters <- playstyles_compiler_result$compiled_playstyles_children_defaulters
    compiled_playstyles_parameters_defaulters <- playstyles_compiler_result$compiled_playstyles_parameters_defaulters
    compiled_playstyles_neither_defaulters <- playstyles_compiler_result$compiled_playstyles_neither_defaulters

    # prime the initial_contestants i.e. construct a fully compiled list "compiled_contestants"
    compiled_contestants <- contestants_primer(
        initial_contestants,
        compiled_aliases,
        compiled_playstyles_both_defaulters,
        compiled_playstyles_children_defaulters,
        compiled_playstyles_parameters_defaulters,
        compiled_playstyles_neither_defaulters,
        working_configuration
    )

    # perform another compatibility check
    if (!identical(names(initial_contestants), unique(names(initial_contestants)))) {
        stop("No two contestants should share the same name.")
    }

    # define what data to keep in the purging
    # TODO: make this be better
    if (!exists("data_to_keep", inherits = FALSE)) {
        # create a list "data_to_keep" specifying what collected data to keep before analysis
        data_to_keep <- list(
            # the reality outcome in every round of every simulation
            list("sims", "FORALL", "collected_data", "FORALL", "reality", "PRESERVE"),
            # the lambda of every contestant in every round of every simulation
            list("sims", "FORALL", "collected_data", "FORALL", "contestant_loss_deltas", "PRESERVE"),
            # the cumulative loss of every contestant in every round of every simulation
            list("sims", "FORALL", "collected_data", "FORALL", "contestant_loss_cumulates", "PRESERVE"),
            # the size of the suite
            list("suite", "suite_size", "PRESERVE"),
            # the base size of omega for the suite
            list("suite", "omega_size", "PRESERVE"),
            # the base number of rounds per simulation for the suite
            list("suite", "rounds_per_simulation", "PRESERVE"),
            # the name of the file the data is exported to
            list("suite", "data_export_filename", "PRESERVE"),
            # the name of the file the data is displayed to
            list("suite", "display_output_filename", "PRESERVE"),
            # the base initial state of the contestants of a simulation for the suite
            list("suite", "compiled_contestants", "PRESERVE"),
            # the boolean on whether a fixed seed was used or not
            list("suite", "use_seed", "PRESERVE"),
            # the regret type used
            list("suite", "regret_type", "PRESERVE"),
            # the boolean on whether to used a fixed list of realities instead of RNG
            list("suite", "use_user_realities", "PRESERVE")
        )

        if (working_configuration$use_seed) {
            # the fixed seed used
            data_to_keep <- c(data_to_keep, list(list("suite", "custom_seed", "PRESERVE")))
        }

        if (working_configuration$use_user_realities) {
            # the fixed list of realities used
            data_to_keep <- c(data_to_keep, list(list("suite", "user_realities", "PRESERVE")))
        }

        # for every player contestant, make sure the relevant data (i.e. the weights) gets kept too
        has_at_least_one_player <- FALSE
        for (contestant_index in seq_along(compiled_contestants)) {
            if (compiled_contestants[[contestant_index]]$playstyle == "player") {
                has_at_least_one_player <- TRUE
                data_to_keep <- c(
                    data_to_keep,
                    # the players don't get removed there
                    list(list(
                        "sims",
                        "FORALL",
                        "collected_data",
                        working_configuration$rounds_per_simulation, # TODO: make it so that this is sim-dependent
                        "contestants",
                        contestant_index,
                        "parameters",
                        "weights",
                        "PRESERVE"
                    ))
                )
            }
        }
        # TODO: this trick with keeping the players' weights is actually just circumventing analysis.
        # There should really be twee rounds of purging, one before and one after performing analysis.
        # There should be different data_to_keep instructions for the two purgings.
        # And when analysis is performed, the weights of the players in the last round of each sim
        # Should be stored somewhere separate so it doesn't clunkily get stored here the way it does so now.
    }

    # define a function "guess_executor" for performing a guess
    # TODO: make this maybe not take round_config or contestant, but playstyle, parameters, round_settings, round_data
    guess_executor <- function(contestant, contestant_index, round_config) {
        # get the guessing functions from the environment where this function is defined
        guessing_functions <- get(
            "compiled_playstyles_guessing_functions",
            envir = parent.env(environment()),
            inherits = FALSE
        )

        # get the correct guessing function for the playstyle of this contestant
        guessing_function <- guessing_functions[[contestant$playstyle]]

        # call that guessing function
        parameters <- contestant$parameters
        guessing_output <- guessing_function(parameters, contestant_index, round_config)

        # update the contestant parameters
        contestant$parameters <- guessing_output$parameters

        # return guess and updated contestant
        return(list(guess = contestant_output$guess, contestant = contestant))
    }

    # TODO: rewrite this part well
    # define an object "base_sim_config" that describes the basis for what every sim_config will start out as.
    base_sim_config <- list(
        rounds_total = working_configuration$rounds_per_simulation,
        round_config_default = list(
            prev_round_data = list(
                reality = NULL,
                contestants = compiled_contestants,
                contestant_guesses = NULL,
                contestant_lambdas = NULL,
                contestant_losses = rep.int(list(0), times = length(compiled_contestants))
            ),
            round_settings = list(
                omega_size = working_configuration$omega_size,
                round_index = 1,
                use_user_realities = working_configuration$use_user_realities,
                user_realities = working_configuration$user_realities
            ),
            round_data_initial_state = list(
                reality = NULL,
                contestants = rep.int(list(NULL), times = length(compiled_contestants)),
                contestant_guesses = rep.int(list(NULL), times = length(compiled_contestants)),
                contestant_lambdas = rep.int(list(NULL), times = length(compiled_contestants)),
                contestant_losses = rep.int(list(0), times = length(compiled_contestants))
            ),
            guess_executor = guess_executor
        )
    )

    # save all constructed objects in the working_configuration and return it as the compiled configuration.
    compiled_configuration <- working_configuration
    compiled_configuration <- sane_modify_list(compiled_configuration, list(
        compiled_aliases = compiled_aliases,
        compiled_contestants = compiled_contestants,
        data_to_keep = data_to_keep,
        guess_executor = guess_executor,
        base_sim_config = base_sim_config,
        initial_contestants = initial_contestants,
        has_at_least_one_player = has_at_least_one_player
    ))

    # return the compiled configuration
    return(compiled_configuration)
}
