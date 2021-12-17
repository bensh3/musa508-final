# Northeast Corridor delay methodology

## Presentation
https://youtu.be/FoiRxNST2fg
## Data collection

- Kaggle
- GTFS
- riem weather
- NJT train cancellations by month

## Data processing

- Give each station a defined sequence ID number
- Filter for non-NA, for complete trips, year 2019
- Categorize trains by period of day
- Categorize into small delay and big delay

## Feature engineering

- Create feature for known bottleneck (interlocking, speed limit)
- Time lag
- Period of day
- Weather

## Modeling

- Model delay by station
- Model delay by train

## User Interface

![arch.diagram](https://github.com/bensh3/musa508-final/blob/main/figs/User_interface.jpg)

This is the user interface we recommend: the left is for normal commuters, we just add a delay prediction based on our model posted 2 hours before departure on the existing NJT APP. But for interest group, we are going to have a PC-based dashboard that include a interactive stringline, train’s current and historical operation status. The Service Recommendation section below includes our model results.
