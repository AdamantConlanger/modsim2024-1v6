This program works as a pipeline.
Its execution starts in `main_stuff.r`,
where it sources the program parts and loads persistent data
from the persistent file in `resources`.

It then calls the configuration parser to read and parse
the user configuration file in `inputs` into a configuration object.
This configuration object contains all necessary stuff for the program to run.

Then it runs the simulation suite.

Then it cleans up the data gathered from the suite
and exports it to the RDS file in `outputs`.

Then it calculates various values and analyses the data.

Then it prints the data and analysis results to the JSON file in `outputs`,
and it visualizes the data.

Finally, it re-exports the persistent data (as well as new persistent
data) and finishes up.

For more info, see the `handige_handleiding` in version 5.