# User Config Explained

This program works by reading in the `inputs/user_config.r` file
and parsing the settings listed there.
So here, I will explain how these settings work.

The first thing I want to note is that this is a watered-down version
of the original program, aimed at being comprehensible.
If you want control flow, normal distributions, guess merging,
or guess copying, or if you want to define custom playstyles and the like,
as well as many other features relating to the internal workings of the program,
you'll have to use version 5 instead of version 6.

Now, with that out of the way, let's begin.

The user configuration file consists of two components, both optional.
The first is the `configuration`, the second the `contestants`.

In the `configuration`, settings can be specified
in `setting_name = setting_value` format.
The `example_inputs/example_config_all_info.r` file has a description
of every setting, and serves as an example for how to run the program.
Do please note that actually copying over and using this example configuration
directly should raise exactly one warning about the seventh contestant and
the simulation size. This is normal.

In the `contestants`, contestants (i.e. players and experts) can be specified.
Players need not be last to guess, and there can be multiple players.
A nice overview of the various contestants is again given in the
aforementioned example configuration file.

To run the program, you simply edit the user config,
save the user config,
and run the `main_stuff.r` file.
The outputs of the program then appear in the `outputs` folder.
The `json_output.json` file here has a human-readable format.

There are two other example configuration files in `example_inputs`,
namely `example_config_rudimentary.r` and `example_config_soccer_data.r`.
These are skeletons that can be used as starting points for the
two respective parts of the research project.

Finally, if the last contestant is a player,
a visualization is performed showing the current and previous performances
of that player. You can remove the previous runs by
deleting the `resources/persistent_stuff.rds` file.

That'll be all; now go have fun simulating stuff.
