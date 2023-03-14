running-performance-exploration
================

## About

This repository contains code to parse and analyze records of my running workouts. I use [Strava](https://www.strava.com) and sometimes a [Garmin](https://garmin.com) watch to record my outings.

The download files from Strava and Garmin both require some parsing so that they can be analyzed in a tabular format together. I do that using the `process-activity-files.R` script. The `process-summary-file.R` script adds a number of columns including historical weather data from [OpenWeatherMap](https://openweathermap.org/) for each workout to the combined resultant dataset. In that script, I also make some bespoke changes to the dataset to account for data collection issues I happen to be aware of. The `analyze.R` script contains code addressing a number of research questions related to my pace and performance over time.

## Selected results

*Last updated November 2022*

<img src="/PLOTS/2022-11-10%20-%20Pace%20plus%20over%20time.png" style="width:80.0%" />
