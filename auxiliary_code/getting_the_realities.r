# read the betting data from the csv
betting_data <- read.csv("./resources/E0.csv")
outcomes_vector <- betting_data$FTR # "full time result", if I were to wager a guess

result_vector <- sapply(outcomes_vector, switch, H = 1, D = 2, A = 3, USE.NAMES = FALSE)

saveRDS(result_vector, file = "./resources/soccer_realities.rds")
