# NOTE: for information about how this file works and some examples, see example_config_all_info.r.
# For the soccer data simulation, you can use example_config_soccer_data.r as a starting point.
# For the analysis simulations, you can use example_config_rudimentary.r as a starting point.

# you can specify the settings here. You can also leave out any one of these; it'll default to a reasonable value.
configuration <- list(
    suite_size = 1,
    rounds_per_simulation = 25,
    omega_size = 3
)

# you can specify the contestants here.
contestants <- list(
    "uniform",
    list("consistent", parameters = list(0.1, 0.3, 0.6)),
    "confident",
    "confident",
    "confident",
    "confident",
    "confident",
    "wrong",
    "uniform",
    list("toggle_wrong_cheater", parameters = list(15)),
    "player"
)

# Do please note that this is a severely simplified version of the full application.
# If you want normal distributions, control flow, guess merging, or guess copying,
# or if you want to define custom playstyles and the like, you'll have to use v5 instead of v6.
