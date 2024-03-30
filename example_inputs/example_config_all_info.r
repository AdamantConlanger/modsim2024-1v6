# NOTE: for information about how this file works and some examples, see example_config_all_info.r.
# For the soccer data simulation, you can use example_config_soccer_data.r as a starting point.
# For the analysis simulations, you can use example_config_rudimentary.r as a starting point.

# you can specify the settings here. You can also leave out any one of these; it'll default to a reasonable value.
configuration <- list(
    suite_size = 1, # number of simulations to run. Must be nonnegative.
    rounds_per_simulation = 10, # number of rounds per simulation. Must be nonnegative.
    omega_size = 3, # number of outcomes reality can take on. Must be positive.
    data_export_filename = "./outputs/exported_data.rds", # where you want your data stored afterwards. Should be RDS.
    json_filename = "./outputs/json_output.json", # where you want to see the results. Should be JSON.
    use_seed = TRUE, # whether to use a seed to generate randomness for reproducibility. By default, FALSE.
    custom_seed = 25, # custom seed to use if use_seed == TRUE. By default, 4219 * 0106 * 413.
    use_short_contestants = TRUE, # whether to show the parameters of the contestants in the display output.
    use_predetermined_realities = TRUE, # if set to TRUE, uses realities provided here instead of random ones.
    predetermined_realities = c(1, 1, 2, 2, 3, 3, 2, 2, 1, 1), # sequence of realities to use in that case.
    keep_parameters = FALSE, # whether to show the contestants' parameters in the output (when they have params).
    keep_final_parameters = TRUE # whether to show the parameters in the final round(s), given the above is FALSE.
)

# you can specify the contestants here.
contestants <- list(
    # to describe a contestant, just say what playstyle they should have.
    "cheater",
    # you can have multiple contestants adopting the same playstyle.
    "cheater",
    # rest assured, they'll act independent of one another, unless the playstyle dictates they interact.
    "uniform",
    # you can specify parameters for a contestant by putting the playstyle and the parameters in list.
    list("toggle_cheater_wrong", parameters = list(9)),
    # if you want to specify multiple parameters, you just put them in that list together.
    list("consistent", parameters = list(1, 4, 0)),
    # players needn't be the last contestants to play.
    "player",
    # this contestant plays the probabilities of the B365 betting house.
    list("house", parameters = list("./resources/B365_probabilities.rds")),
    # this contestant just keeps guessing (1/5, 4/5, 0).
    list("consistent", parameters = list(1, 4, 0)),
    # this contestant always guesses correctly.
    "cheater",
    # this contestant always guesses wrong, but the outcome they choose instead is random.
    "wrong",
    # this contestant always goes all-in on a random outcome.
    "confident",
    # this contestant always guesses a uniform distribution.
    "uniform",
    # this contestant starts off as a cheater and then becomes wrong from round T onwards,
    # or halfway into the game if T isn't specified.
    "toggle_cheater_wrong",
    # and this contestant is the exact opposite, starting off as wrong and becoming a cheater.
    "toggle_wrong_cheater",
    # yes, it's possible to have multiple players.
    "player"
)

# Do please note that this is a severely simplified version of the full application.
# If you want normal distributions, control flow, guess merging, or guess copying,
# or if you want to define custom playstyles and the like, you'll have to use v5 instead of v6.
