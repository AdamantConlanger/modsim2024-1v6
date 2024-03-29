configuration <- list(
    suite_size = 1,
    rounds_per_simulation = 380,
    omega_size = 3,
    use_seed = TRUE,
    custom_seed = 25,
    use_predetermined_realities = TRUE,
    predetermined_realities = readRDS("./resources/soccer_realities.rds")
    # note: the above causes the actual recorded outcome of the matches to be used as realities.
)

contestants <- list(
    B365 = list("house", parameters = list("./resources/B365_probabilities.rds")),
    BW = list("house", parameters = list("./resources/BW_probabilities.rds")),
    IW = list("house", parameters = list("./resources/IW_probabilities.rds")),
    PS = list("house", parameters = list("./resources/PS_probabilities.rds")),
    VC = list("house", parameters = list("./resources/VC_probabilities.rds")),
    WH = list("house", parameters = list("./resources/WH_probabilities.rds")),
    player = "player"
)
