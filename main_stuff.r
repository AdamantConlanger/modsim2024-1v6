source("./program_parts/configuration_parsing_stuff.r")
source("./program_parts/analysis_stuff.r")
source("./program_parts/data_cleanup_stuff.r")
source("./program_parts/simulation_stuff.r")
source("./program_parts/exporting_stuff.r")
source("./program_parts/visualization_stuff.r")
source("./program_parts/jsonification_stuff.r")

print("Finished sourcing source files.")

# load stuff that needs to persist on into the next execution of this application
if (file.access("./resources/persistent_stuff.rds", mode = 0) == 0) {
    if (file.access("./resources/persistent_stuff.rds", mode = 4) == -1) {
        # file exists but isn't readable
        stop("./resources/persistent_stuff.rds exists, but the program can't read it.")
    }
    try(recovered_stuff <- readRDS("./resources/persistent_stuff.rds"))
} else {
    recovered_stuff <- list()
}

print("Finished recovering persistent stuff.")

# read in user's configuration and compile it into usable form.
# We'll also store the gathered data in this configuration object
configuration <- parse_configuration(recovered_stuff)

print("Finished parsing configuration.")

# perform simulation suite
configuration <- perform_suite(configuration)

print("Finished performing simulations.")

# remove data that doesn't have any use anymore. Mainly just guessing functions and the like.
configuration <- cleanup_data(configuration)

print("Finished cleaning raw data.")

# export the raw data to an RDS file
configuration <- export_data(configuration)

print("Finished exporting data to RDS file.")

# perform calculations and statistical analysis
configuration <- analyze_data(configuration)

print("Finished performing analysis on data.")

# print a user-friendly representation of the data and analysis results to a JSON file or something
# (and prettify the data a bit)
configuration <- print_data(configuration)

print("Finished human-readably writing data to file.")

# perform data visualization
configuration <- visualize_data(configuration)
# configuration <- visualize_weights(configuration)
# configuration <- visualize_lambdas(configuration)
# configuration <- visualize_losses_excess(configuration)
# configuration <- visualize_losses_excess_with_player(configuration)
# configuration <- visualize_lambdas_excess_with_player(configuration)
# configuration <- visualize_losses_for_sim_analysis(configuration)
# configuration <- visualize_regret_for_sim_analysis(configuration)

print("Finished visualizing data.")

# save stuff that needs to persist on into the next execution of this application
saveRDS(configuration$persistent_stuff, file = "./resources/persistent_stuff.rds")

print("Finished saving persistent stuff.")

print("Finished application execution.")
