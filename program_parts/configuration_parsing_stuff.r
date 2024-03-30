source("./program_parts/global_functions_stuff.r")

# turns a list of bare-bones playstyles into a more useful format for the configuration module
playstyles_compiler <- function(initial_playstyles, configuration) {
    # compiles the initial_playstyles into the following:
    # a list "guessing_functions" with the guessing functions of the playstyles.
    # a list "parameter_checkers" with functions for checking parameters.
    # a list "parameter_defaulters" with functions for defaulting parameters.

    # make sure every playstyle is actually named, cause, well, it needs to be identifiable
    the_names <- names(initial_playstyles)
    if (identical(the_names, NULL) && !identical(initial_playstyles, list())) {
        stop("No playstyle should be without name. But one of them is.")
    }
    if ("" %in% the_names) {
        stop("No playstyle should be without name. But one of them is.")
    }

    # declare the lists we'll be returning
    guessing_functions <- list()
    parameter_checkers <- list()
    parameter_defaulters <- list()

    # go through all playstyles and put the right things in the right lists
    for (name in the_names) {
        # make sure the playstyle description is a list with at least one element
        if (!is.list(initial_playstyles[[name]])) {
            stop(paste0("Playstyle \"", name, "\" was uncompilable."))
        }
        if (length(initial_playstyles[[name]]) == 0) {
            stop(paste0("Playstyle \"", name, "\" was uncompilable."))
        }

        guessing_functions[[name]] <- initial_playstyles[[name]]$guessing_function
        parameter_checkers[[name]] <- initial_playstyles[[name]]$parameter_checker
        parameter_defaulters[[name]] <- initial_playstyles[[name]]$parameter_defaulter
    }

    # put all the lists in one single list and return it
    return(list(
        guessing_functions = guessing_functions,
        parameter_checkers = parameter_checkers,
        parameter_defaulters = parameter_defaulters
    ))
} # compiles initial_playstyles into separate lists of things

# turns a list of user-declared contestants into a more useful format for the configuration module
contestants_primer <- function(initial_contestants,
                               parameter_checkers,
                               parameter_defaulters,
                               configuration) {
    # declare list of contestants we'll eventually return
    resulting_contestants <- list()

    # for each contestant:
    for (contestant_index in seq_along(initial_contestants)) {
        # prime the current contestant
        contestant <- initial_contestants[[contestant_index]]

        # if the contestant is a playstyle identifier, replace it by a full-fledged contestant
        if (is.character(contestant) && hasName(parameter_checkers, contestant)) {
            contestant <- list(playstyle = contestant)
        }

        # if the contestant isn't a list or doesn't specify a playstyle, error
        if (!is.list(contestant) || length(contestant) == 0 || !is.character(contestant[[1]])) {
            msg <- "Woah, woah, woah. You're trying to prime a contestant that's neither in list nor an identifier."
            try(stop(paste0("impermissible contestant: ", contestant, ".")))
        }

        # if the playstyle is not a named list entry, make it one
        if (!hasName(contestant, "playstyle")) {
            if (identical(names(contestant), NULL)) {
                names(contestant) <- rep.int(list(""), times = length(contestant))
            }
            names(contestant)[[1]] <- "playstyle"
        }

        # we either check whether the parameters are compatible, or assign the default parameters
        has_parameters <- hasName(contestant, "parameters")
        if (has_parameters) {
            has_parameters <- is.list(contestant$parameters) && length(contestant$parameters) != 0
        }

        # then, we call the relevant defaulter function on the contestant
        if (has_parameters) {
            # we need to check the parameters
            contestant <- parameter_checkers[[contestant$playstyle]](
                contestant$parameters,
                contestant_index,
                configuration
            )
        } else {
            # we need to default the parameters
            contestant <- parameter_defaulters[[contestant$playstyle]](
                contestant_index,
                configuration
            )
        }

        # just to assure that the parameters list is an actual list and not a vector:
        contestant$parameters <- as.list(contestant$parameters)

        resulting_contestants[[contestant_index]] <- contestant
    }

    # return the compiled contestant list
    return(resulting_contestants)
} # primes every contestant in a list and returns the list.

parse_configuration <- function(recovered_stuff) {
    # define default values for the settings
    default_configuration <- list(
        suite_size = 1,
        rounds_per_simulation = 10,
        omega_size = 2,
        data_export_filename = "./outputs/exported_data.rds",
        json_filename = "./outputs/json_output.json",
        use_seed = FALSE,
        custom_seed = 4219 * 0106 * 413,
        use_short_contestants = TRUE,
        use_predetermined_realities = FALSE,
        predetermined_realities = c(),
        keep_parameters = TRUE,
        recovered_stuff = recovered_stuff
    )
    # define playstyles (variable called initial_playstyles)
    source("./program_parts/builtin_playstyles.r")

    # source configuration and contestants from the user config file
    source("./inputs/user_config.r", local = TRUE)

    # combine the default values with the user-supplied ones, and rename everything accordingly
    if (exists("configuration", inherits = FALSE)) {
        configuration <- sane_modify_list(default_configuration, configuration)
    } else {
        configuration <- default_configuration
    }
    if (exists("contestants", inherits = FALSE)) {
        initial_contestants <- contestants
    } else {
        initial_contestants <- list()
    }

    # sanity checking the settings provided
    if (configuration$suite_size < 0) {
        stop("suite_size must be nonnegative.")
    }
    if (configuration$rounds_per_simulation < 0) {
        stop("rounds_per_simulation must be nonnegative.")
    }
    if (configuration$omega_size < 1) {
        stop("omega_size must be strictly positive.")
    }
    if (!is.character(configuration$data_export_filename)) {
        stop("data_export_filename must be a filename.")
    }
    if (!is.character(configuration$json_filename)) {
        stop("json_filename must be a filename.")
    }
    if (!is.logical(configuration$use_short_contestants)) {
        stop("use_short_contestants must be a logical value.")
    }
    if (!is.logical(configuration$use_seed)) {
        stop("use_seed must be a logical value.")
    }
    if (!sane_is_integer(configuration$custom_seed)) {
        stop("custom_seed must be an integer.")
    }
    if (!is.logical(configuration$use_predetermined_realities)) {
        stop("use_predetermined_realities must be a logical value.")
    }
    if (configuration$use_predetermined_realities) {
        if (!is.vector(configuration$predetermined_realities) || is.list(configuration$predetermined_realities) ||
            length(configuration$predetermined_realities) != configuration$rounds_per_simulation
        ) {
            stop("predetermined_realities must be a vector with one outcome as reality for every round.")
        }
        if (configuration$rounds_per_simulation > 0) {
            if (!all(sane_is_integer(configuration$predetermined_realities)) ||
                max(configuration$predetermined_realities) > working_configuration$omega_size ||
                min(configuration$predetermined_realities) < 1
            ) {
                stop("predetermined_realities must be a vector with one outcome as reality for every round.")
            }
        }
    }
    if (!is.logical(configuration$keep_parameters)) {
        stop("keep_parameters must be a logical value.")
    }

    # set random seed
    if (configuration$use_seed) {
        set.seed(configuration$custom_seed)
    } else {
        set.seed(NULL)
    }

    # compile the initial_playstyles into the following:
    # a list "guessing_functions" with the guessing functions of the playstyles.
    # a list "parameter_checkers" with functions for checking parameters.
    # a list "parameter_defaulters" with functions for defaulting parameters.
    playstyles_compiler_result <- playstyles_compiler(initial_playstyles, configuration)
    guessing_functions <- playstyles_compiler_result$guessing_functions
    parameter_checkers <- playstyles_compiler_result$parameter_checkers
    parameter_defaulters <- playstyles_compiler_result$parameter_defaulters

    # prime the initial_contestants i.e. make them ready for the simulation
    contestants <- contestants_primer(
        initial_contestants,
        parameter_checkers,
        parameter_defaulters,
        configuration
    )

    # save all constructed objects in the configuration.
    configuration <- sane_modify_list(configuration, list(
        contestants = contestants,
        guessing_functions = guessing_functions,
        data_store = list()
    ))

    # return the configuration
    return(configuration)
}
