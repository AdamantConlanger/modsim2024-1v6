source("./program_parts/global_functions_stuff.r")

cleanup_data <- function(configuration) {
    configuration <- configuration[names(configuration) != "guess_executor"]

    return(configuration)
}
