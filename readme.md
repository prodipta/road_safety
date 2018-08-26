# Analyzing Road Safety Data From Ahmedabad and Bangalore

The code folder has four files. Start with `event_map.R`. This reads the data file (csv) and creates an events data-frame (after mapping location data to a grid). It also generates the event points on the city map. Next comes `temporal.R`. It groups events by day-of-week and time-of-day (one hour bucket), enrich the `events` dataframe with these information and generates the corresponding plot. The `dataframe.R` generates the underlying data for the MLP model, mainly creating the required one-hot transformations and train-test splits. Finally `model.R` implemens and trains the model.

Some example images generated are in the image folder. The model folder has the MLP model in HD5

## Requirements

Type: Scripts
Description: Analyze traffic incidents data
Depends: R (>= 3.3.2), ggplot2, ggmap, reshape2, keras

