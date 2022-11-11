

# setup -------------------------------------------------------------------

# devtools::install_github("grimbough/FITfileR")
# devtools::install_github("trackerproject/trackeR")

library(tidyverse)
library(trackeR)
library(fitdc)

export_date <- "2022-11-09"
PATH_IN <- str_c(getwd(), "/DATA/export-", export_date, "/")

activity_names <- list.files(str_c(PATH_IN, "activities/"))

# get record names from my fitness watch
compressed_record_names <- activity_names[str_sub(activity_names,-6,-1) == "fit.gz"]

# unzip those files ------------------------------------------------------

str_c(PATH_IN,"activities", "/", compressed_record_names) |> 
  map(~ R.utils::gunzip(.x, remove = F))

# refresh activity names
activity_names_refreshed <- list.files(str_c(PATH_IN, "activities/"))

# get newly uncompressed files
uncompressed_fit_names <- activity_names_refreshed[str_sub(activity_names_refreshed,-3,-1) == "fit"] 
# specify exact fit bc only want uncompressed, no fitgz

# read .fit files (import step 1/2) ---------------------------------------

unpack_fit <- function(fit_name){
  record <- FITfileR::readFitFile(
    str_c(PATH_IN, "activities/", fit_name)
  ) %>% FITfileR::records()
  
  if(length(record) > 1) {
    record <- record %>% bind_rows() %>% mutate(filename = fit_name)
  }
  
  return(record)
}

fit_records <- uncompressed_fit_names |> 
  map(unpack_fit) |> 
  bind_rows() |> 
  rownames_to_column(var = "activity_name") |> 
  arrange(timestamp)

# read .gpx files (import step 2/2) ---------------------------------------

# get gpx names
gpx_names <- activity_names_refreshed[str_sub(activity_names_refreshed,-3,-1) == "gpx"]

# NOTE ---

# I added local helper functions as I was getting the following error while
# running trackeR::readGPX() in June 2022

# > Error in get(as.character(FUN), mode = "function", envir = envir) : 
# >   object 'km_per_h2m_per_s' of mode 'function' was not found

km_per_h2m_per_s <- function(variable) {
  variable * 1000/60/60
}
km2m <- function(variable) {
  variable * 1000
}

unpack_gpx <- function(gpx_name){
  record <- trackeR::readGPX(str_c(PATH_IN, "activities/", gpx_name)) %>% 
    as_tibble() %>% 
    mutate(filename = gpx_name) %>% 
    rename(
      timestamp = time, 
      position_lat = latitude, 
      position_long = longitude, 
      cadence = cadence_running
    )
  return(record)
}

gpx_records <- gpx_names |> 
  map(unpack_gpx) |> 
  bind_rows()

# combine both record types -----------------------------------------------

records <- bind_rows(
  fit_records,
  gpx_records
)

# cleanup
rm(list = setdiff(ls(), c("records", "PATH_IN", "export_date")))

# dir.create(str_c(getwd(),"/DATA/","results-",export_date))

records %>% 
  write_csv(
    str_c(getwd(),"/DATA/","results-",export_date,"/","processed-activity-files",".csv")
  )
