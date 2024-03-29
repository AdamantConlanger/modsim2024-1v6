source("./program_parts/configuration_parsing_stuff.r")
source("./program_parts/analysis_stuff.r")
source("./program_parts/result_handling_stuff.r")
source("./program_parts/data_cleanup_stuff.r")
source("./program_parts/simulation_stuff.r")

print("Finished sourcing source files.")

# load stuff that needs to persist on into the next execution of this application
recovered_stuff <- readRDS("./resources/persistent_stuff.rds")

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
configuration <- print_data(configuration)

print("Finished human-readably writing data to file.")

# perform data visualization
configuration <- visualize_data(configuration)

print("Finished visualizing data.")

# save stuff that needs to persist on into the next execution of this application
saveRDS(configuration$persistent_stuff, file = "./resources/persistent_stuff.rds")

print("Finished saving persistent stuff.")

print("Finished application execution.")

# TODO: write the missing playstyles
# TODO: write guide file
# TODO: write a better summarizer for the data (like OLD/include/simulation_data_setup.r)
# TODO: deal with the TODOs in every file
# TODO: make it so that the user can choose to get a statistical summary of the suite results. A true summary.

# TODO: note to self: rlang::last_trace()
# TODO: note to self: is.element()
# TODO: maybe "list(1, 2) %in% list(list(1, 2))" and "list(1, 2) %in% list(1, 2)" are weird
# TODO: note to self: tools::assertCondition()
# TODO: note to self: message()
# TODO: note to self: a <- list(); a$b$c <- 3; print(a$b$c) works. But a[[2]]$b <- 4 does not.
