configuration <- list(
    suite_size = 1,
    rounds_per_simulation = 20,
    omega_size = 2,
    use_seed = TRUE,
    custom_seed = 4219 * 0106 * 413,
    use_predetermined_realities = TRUE,
    predetermined_realities = rep.int(c(1), times = 20)
    # note: the above causes the first outcome to always occur as reality; "20" should number of rounds)
)

contestants <- list(
    "cheater",
    "wrong",
    "player"
)
