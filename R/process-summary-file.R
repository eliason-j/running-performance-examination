

# setup -------------------------------------------------------------------

rm(list = ls())

library(tidyverse)
library(sf)
library(spData)
library(owmr)
library(lubridate)

readRenviron("~/.Renviron")

export_date <- "2022-11-09"
PATH_IN <- str_c(getwd(), "/DATA/")

# unit conversion ---------------------------------------------------------

ONE_KM_IN_MI <- 0.621371
ONE_MI_IN_KM <- 1.60934
ONE_M_IN_FT <- 3.28084


# state boundary function --------------------------------------------------

lonlat_to_state <- function(pointsDF,
                            states = spData::us_states,
                            name_col = "NAME") {
  
  pointsDF <- pointsDF |> 
    mutate_all(~ ifelse(is.na(.), 0, .))
  
  ## Convert points data.frame to an sf POINTS object
  pts <- st_as_sf(pointsDF, coords = 1:2, crs = 4326)
  
  ## Transform spatial data to some planar coordinate system
  ## (e.g. Web Mercator) as required for geometric operations
  states <- st_transform(states, crs = 3857)
  pts <- st_transform(pts, crs = 3857)
  
  ## Find names of state (if any) intersected by each point
  state_names <- states[[name_col]]
  ii <- as.integer(st_intersects(pts, states))
  state_names[ii]
}


# lagged totals function --------------------------------------------------

get_recent_total <- function(id, n){
  at <- record_key$rdatetime_local[record_key$filename == id]
  since <- record_key$rdatetime_local[record_key$filename == id] - lubridate::days(n)
  sumdist <- record_key |> 
    filter(rdatetime_local > since & rdatetime_local < at) |> 
    pull(distance) |> 
    sum()
  return(sumdist)
}

# unix timestamp function -------------------------------------------------

time_since_epoch <- function(input) {
  x1 <- as.POSIXct(input)
  x2 <- format(x1, tz="GMT", usetz=F)
  x3 <- lubridate::ymd_hms(x2)
  epoch <- lubridate::ymd_hms('1970-01-01 00:00:00')
  time_since_epoch <- (x3 - epoch) / dseconds()
  return(time_since_epoch)
}


# query historical weather data function ----------------------------------

# api keys here
# https://home.openweathermap.org/api_keys

api_call_count <- 0
get_hist_weather <- function(id, key = Sys.getenv("OWM_API_KEY")){
  subsetted <- record_key |> filter(filename == id)
  url <- str_c("https://api.openweathermap.org/data/3.0/onecall/timemachine?lat=",subsetted$lat_start,"&lon=",subsetted$lon_start,"&dt=",subsetted$runix_utc,"&appid=",key,"&units=imperial")
  
  if(api_call_count>59){
    message("Approaching API rate limit; sleeping for 60 seconds")
    Sys.sleep(60)
    api_call_count <<- 0
  }
  # see more about units: https://openweathermap.org/api/one-call-3#data
  # temperature in Fahrenheit and wind speed in miles/hour
  out <- tryCatch({
    result <- httr::GET(url)
    api_call_count <<- api_call_count+1
    char <- rawToChar(result$content)
    dat <- jsonlite::fromJSON(char)$data
    dat$filename <- id
    dat$weather_description <- dat$weather[[1]]$description[1]
    print(dim(dat))
    dat
  }, error = function(e) {
    message("Error encountered")
    data.frame(filename = id)
  })
  return(out)
}

# read and clean ----------------------------------------------------------

records <- read_csv(str_c(PATH_IN,"results-",export_date,"/processed-activity-files.csv"))

record_key_raw <- read_csv(str_c(PATH_IN, "export-", export_date, "/", "activities.csv")) %>% 
  janitor::clean_names() %>% 
  janitor::remove_empty() %>% 
  mutate(activity_id = as.character(activity_id)) |> 
  select(filename, everything()) %>% 
  select(-ends_with("_1")) |> 
  filter(activity_type == "Run")

# manually identifying unusual activities ---------------------------------

# NOTE ---
# these id vectors should get a manual check each time this runs since
# these categories are based on whether i remember to add something in the 
# activity info

ids_treadmill <- c(
  record_key_raw |> 
    filter(str_detect(activity_name, regex('treadmill', ignore_case = T))) |> 
    pull(activity_id) |> 
    as.character(),
  c() # placeholder to add ids manually as needed
)

ids_interval <- c(
  record_key_raw |> 
    filter(str_detect(activity_description, "[0-9]x[0-9]")) |> 
    pull(activity_id) |> 
    as.character(),
  record_key_raw |> 
    filter(str_detect(activity_name, regex('run club', ignore_case = T)) | str_detect(activity_description, regex('run club', ignore_case = T))) |> 
    pull(activity_id) |> 
    as.character(),
  c(
    "4626041732", # "two minutes between laps"
    "1729060155", # mile repeats
    "6794901402", # broken run club
    "6215889944" # 60s between miles
  )
) |> 
  unique()

ids_short <- c(
  record_key_raw |> 
    filter(distance_7 < ONE_MI_IN_KM) |> 
    pull(activity_id) |> 
    as.character(),
  c() # placeholder to add ids manually as needed
)

ids_walk_or_hike <- c(
  "1812636545", # lake blanche hike (coded incorrectly)
  "3324264305", # phone call with wade--walk not run
  "7329897519", # first three with lily
  "7356911952", # with lily again
  "7515381326", # great falls
  "7669566211", # great falls
  "7858712164" # alternate walk run in hyde park with elise
  
)


# supplementary changes ---------------------------------------------------

record_key <- 
  record_key_raw |> 
  
  # trim filename
  mutate(
    filename = str_remove(filename, "activities\\/"),
    filename = str_remove(filename, ".gz")
  ) |> 
  
  # fix weird column naming
  rename(
    distance = distance_7,
    elapsed_time = elapsed_time_6
  ) |> 
  
  # remove unusual records
  filter(
    !(activity_id %in% c(ids_interval, ids_short, ids_treadmill, ids_walk_or_hike))
  ) |> 
  
  # change meters to feet
  mutate_at(vars(contains("elevation")), function(x){x <- x*ONE_M_IN_FT}) |> 
  
  # add date/time, unit cols
  mutate(
    rdatetime_utc = lubridate::as_datetime(activity_date, format = "%b %d, %Y, %I:%M:%S %p", tz = "UTC"),
    distance = distance*ONE_KM_IN_MI,
    duration = elapsed_time/60,
    duration_moving = moving_time/60,
    pace = duration/distance,
    pace_moving = duration_moving/distance
  ) |> 
  
  # select keep
  select(
    filename, activity_name, activity_description,
    distance, duration, duration_moving, pace, pace_moving,
    starts_with("elevation"),
    ends_with("_et"), ends_with("utc")
  ) %>% 
  
  # round numbers
  mutate_if(is.numeric, ~round(.x, 2)) |> 
  
  # add latitude/longitude ranges
  left_join(
    records |> 
      filter(!is.na(filename) & !is.na(position_long) & !is.na(position_lat)) |> 
      group_by(filename) |> 
      summarise(
        lon_start = position_long[timestamp == min(timestamp, na.rm = T)],
        lon_end   = position_long[timestamp == max(timestamp, na.rm = T)],
        lat_start =  position_lat[timestamp == min(timestamp, na.rm = T)],
        lat_end   =  position_lat[timestamp == max(timestamp, na.rm = T)]
      ) |> 
      ungroup(),
    by = "filename"
  ) |> 
  
  # add state corresponding to starting location
  mutate(
    state = lonlat_to_state(data.frame(x = lon_start, y = lat_start)),
    # i guess that function does poorly with literal edge cases (on the beach)
    state = case_when(
      filename %in% c("7269427548.gpx", "7274796753.gpx") ~ "North Carolina",
      filename %in% c("1610782727.gpx", "1609157048.gpx") ~ "California",
      filename %in% c("2126965824.fit", "2138729101.fit") ~ "Utah", # (no beach here, though?)
      T ~ state
    ),
    state = ifelse(is.na(state), "Intl", state),
    state_group = ifelse(state == "Utah", "Utah", "Other")
  ) |> 
  
  # fix times accordingly
  mutate(
    rdatetime_local = case_when(
      state %in% c("Utah") ~
        format(rdatetime_utc, tz="America/Denver",usetz=TRUE),
      state %in% c("District of Columbia","New York","North Carolina","Virginia") ~
        format(rdatetime_utc, tz="America/New_York",usetz=TRUE),
      state %in% c("California") ~
        format(rdatetime_utc, tz="America/Los_Angeles",usetz=TRUE),
      between(lubridate::as_date(rdatetime_utc), lubridate::as_date("2022-06-08"),lubridate::as_date("2022-06-12")) ~ format(rdatetime_utc, tz="America/New_York",usetz=TRUE), # see note about edge cases on the beach above
      between(lubridate::as_date(rdatetime_utc), lubridate::as_date("2022-09-01"),lubridate::as_date("2022-09-10")) ~ format(rdatetime_utc, tz="Asia/Istanbul",usetz=TRUE), # hacking intl ones until i find a better option
      between(lubridate::as_date(rdatetime_utc), lubridate::as_date("2022-09-17"),lubridate::as_date("2022-11-11")) ~ format(rdatetime_utc, tz="Europe/London",usetz=TRUE),
      T ~ NA_character_
    ) |> lubridate::as_datetime(),
    
    rdate = lubridate::date(rdatetime_local),
    rhour = lubridate::hour(rdatetime_local),
    rday = lubridate::day(rdatetime_local),
    rmonth = lubridate::month(rdatetime_local),
    ryear = lubridate::year(rdatetime_local),
    runix_utc = time_since_epoch(rdatetime_utc)
  ) |> 
  
  # combine single runs split into two (TODO later)
  mutate(
    time_since_last_run = rdatetime_local - lag(rdatetime_local)
  ) |> 
  
  # account for elevation changes (ie trips to ut)
  mutate(
    altitude_change = case_when(
      state != "Utah" & 
        (lag(state) == "Utah" | lag(state, 2) == "Utah") ~ -1,
      state == "Utah" & 
        (lag(state) != "Utah" | lag(state, 2) != "Utah") ~ 1,
      T ~ 0
    )
  )

# add historical weather data
# CAUTION running this portion will charge account ------------------------

record_key_weather <- 
  record_key |> 
  # head(3) |> 
  pull(filename) |> 
  map(get_hist_weather)

# END cautioned portion ---------------------------------------------------

final <- 
  record_key |> 
  left_join(
    record_key_weather |> 
      bind_rows() |> 
      select(
        filename, sunrise_unix_utc = sunrise, sunset_unix_utc = sunset,
        everything()
      ), by = "filename"
  ) |>
  mutate(
    rain_1h = rain$`1h`,
    rain_3h = rain$`3h`,
    snow_1h = snow$`1h`
  ) |> 
  select(-snow, -rain, -weather) |> 
  
  # add rolling totals for tiredness
  arrange(rdatetime_local) |>
  mutate(
    dist_lag3 = filename |> map(~get_recent_total(id = .x, 3)) |> unlist(),
    dist_lag7 = filename |> map(~get_recent_total(id = .x, 7)) |> unlist(),
    dist_lag90 = filename |> map(~get_recent_total(id = .x, 90)) |> unlist()
  )

record_key |> count(state)

record_key |> 
  filter(time_since_last_run < 120)

# write out ---------------------------------------------------------------

final %>%
  write_csv(
    # str_c(PATH_IN,"results-",export_date,"/",today(),"-processed-activity-summary.csv")
  )
