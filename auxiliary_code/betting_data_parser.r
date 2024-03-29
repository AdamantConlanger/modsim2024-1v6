# read the betting data from the csv
betting_data <- read.csv("./resources/E0.csv")


# define the names we'll be using
house_names <- list("B365", "BW", "IW", "PS", "WH", "VC")
outcome_names <- list(home = "H", draw = "D", away = "A")

stopifnot(length(outcome_names) > 0)

# result object
probability_storage <- list()

# for each house, we get the probabilities for each round and store them
for (house_name in house_names) {
    # make the list to store the probabilities of the current house in
    probability_list <- list()

    # make the list of combined house+outcome names
    combined_names <- list()
    for (j in seq_along(outcome_names)) {
        if (identical(names(outcome_names), NULL)) {
            combined_names[[j]] <- paste0(house_name, outcome_names[[j]])
        } else if (names(outcome_names)[[j]] == "") {
            combined_names[[j]] <- paste0(house_name, outcome_names[[j]])
        } else {
            combined_names[[names(outcome_names)[[j]]]] <- paste0(house_name, outcome_names[[j]])
        }
    }

    # for every round, calculate the probabilities and put them in the list
    for (i in seq_along(betting_data[[combined_names[[1]]]])) {
        # the probabilities are calculable like this, based on normalized exchange rates.
        # See Vovk and Zhdanov's paper for more info.
        rootable_function <- function(c, betting_data, i) {
            result <- 0
            for (combined_name in combined_names) {
                result <- result + betting_data[[combined_name]][[i]]^(-c)
            }
            return(result - 1)
        }
        minimum_rate <- c()
        for (combined_name in combined_names) {
            minimum_rate <- min(minimum_rate, betting_data[[combined_name]][[i]])
        }
        upper_bound <- log(length(combined_names), minimum_rate)
        the_c <- uniroot(
            rootable_function,
            interval = c(0, upper_bound),
            betting_data = betting_data,
            i = i,
            extendInt = "downX",
            f.lower = length(combined_names)
        )$root
        probabilities <- c()
        for (combined_name in combined_names) {
            probabilities <- c(probabilities, betting_data[[combined_name]][[i]]^(-the_c))
        }
        probability_list <- c(probability_list, list(probabilities))
    }

    # store the probability list in the storage
    probability_storage[[house_name]] <- probability_list
}


# write.csv(probability_storage, file = "probability_storage.csv")
for (house_name in house_names) {
    csv_file_name <- paste0("house_probabilities/", house_name, "_probabilities.csv")
    rds_file_name <- paste0("house_probabilities/", house_name, "_probabilities.rds")

    saveRDS(probability_storage[[house_name]], file = rds_file_name)

    # for the csv, we want to have the probabilities in transpose:
    # if outputting csvs with custom headers was possible, the header here would be "H", "D, "A"
    transposed_probs <- rep.int(list(c()), length(outcome_names))
    for (item in probability_storage[[house_name]]) {
        for (i in seq_along(outcome_names)) {
            transposed_probs[[i]] <- c(transposed_probs[[i]], item[[i]])
        }
    }

    write.csv(transposed_probs, file = csv_file_name)
}
print(probability_storage[["B365"]][[1]])
