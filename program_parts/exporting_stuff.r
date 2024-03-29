source("./program_parts/global_functions_stuff.r")

export_data <- function(configuration) {
    export_location <- configuration$data_export_filename
    saveRDS(configuration$data_store, file = export_location)
    return(configuration)
} # exports data bank to permanent file.
