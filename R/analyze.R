
# setup -------------------------------------------------------------------

rm(list = ls())

library(tidyverse)
library(lubridate)
library(tidymodels)

readRenviron("~/.Renviron")

export_date <- "2022-11-09"
PATH_IN <- str_c(getwd(), "/DATA/")

runs_all <- read_csv(str_c(PATH_IN,"results-",export_date,"/",export_date,"-processed-activity-summary.csv")) |> 
  mutate(
    rdatetime_local_epoch = (rdatetime_local |> as.integer())/1000000,
    rmonthyear = ryear+(rmonth/12),
    
    # https://www.omnicalculator.com/physics/wet-bulb#how-to-calculate-the-wet-bulb-temperature
    wet_bulb = 
      temp * atan(0.151977 * (humidity + 8.313659)^(1/2)) + 
      atan(temp + humidity) - 
      atan(humidity - 1.676331) + 
      0.00391838 * (humidity)^(3/2) * atan(0.023101 * humidity) - 
      4.686035,
    cold = ifelse(temp<33,1,0),
    rain_1h = ifelse(is.na(rain_1h),0,rain_1h),
    time_of_day = case_when(
      between(rhour, 4, 10) ~ "early",
      between(rhour, 11, 15) ~ "middle",
      between(rhour, 16, 23) ~ "late",
      T ~ NA_character_
    ),
    temp_cat = case_when(
      temp<40~"cold",
      temp>=40&temp<80~"comf",
      temp>=80~"hot",
      T ~ NA_character_
    )
  )

runs <- runs_all |> 
  select(
    filename, 
    rdatetime_local_epoch,
    pace_moving, distance, starts_with("elevation"),
    state_group, altitude_change,
    starts_with("dist_lag"),
    temp, humidity, dew_point, wind_speed
  ) 

# eda ---------------------------------------------------------------------

hist(runs$pace_moving); summary(runs$pace_moving)
hist(runs$distance); summary(runs$distance)
plot(data = runs, pace_moving ~ distance)
plot(data = runs, pace_moving ~ humidity)
plot(data = runs, pace_moving ~ temp)
plot(data = runs, pace_moving ~ elevation_gain)
plot(data = runs, pace_moving ~ elevation_high)
ggplot(runs_all, aes(dist_lag90, pace_moving))+geom_point()+geom_smooth(method = 'lm',se = F)
ggplot(runs_all, aes(wet_bulb, pace_moving))+geom_point()+geom_smooth(method = 'lm',se = F)

# plots ------------------- ------------------------------------------------



# plot - hour of day ------------------------------------------------------

plot_hour_of_day <- 
  runs_all |> 
  ggplot(aes(as.factor(rhour))) +
  geom_histogram(stat = 'count') +
  facet_wrap(~ryear, nrow = length(unique(runs_all$ryear))) +
  theme_minimal() +
  ylab("Count") +
  ggtitle("Activity frequency by hour of day") +
  scale_x_discrete(name = "Hour of day", breaks = 0:23) +
  theme(panel.grid.minor.x = element_blank()) 

ggsave(
  str_c(getwd(), "/PLOTS/", lubridate::today(), " - Hour of day by year", ".png"),
  plot_hour_of_day,
  device = "png",
  height = 4,
  width = 2,
  scale = 2,
  bg = "white"
)

plot_miles_hour_of_day <- 
  runs_all |> 
  group_by(ryear, rhour) |> 
  summarize(miles = sum(distance)) |> 
  ggplot(aes(as.factor(rhour))) +
  geom_bar(aes(y = miles), stat = 'identity') +
  facet_wrap(~ryear, nrow = length(unique(runs_all$ryear))) +
  theme_minimal() +
  ylab("Miles") +
  ggtitle("Miles run by hour of day") +
  scale_x_discrete(name = "Hour of day", breaks = 0:23, labels = as.character(0:23)) +
  theme(panel.grid.minor.x = element_blank()) 

plot_miles_hour_of_day

ggsave(
  str_c(getwd(), "/PLOTS/", lubridate::today(), " - Miles run by hour of day by year", ".png"),
  plot_miles_hour_of_day,
  device = "png",
  height = 4,
  width = 2,
  scale = 2,
  bg = "white"
)


# plot - 90 day moving sum of mileage -------------------------------------

# include omitted runs 
tmp <- 
  record_key_raw |> 
  mutate(
    filename = str_remove(filename, "activities\\/"),
    filename = str_remove(filename, ".gz") %>% str_trim(),
    distance = distance_7*0.621371
  ) |> 
  mutate(rdatetime_local = lubridate::as_datetime(activity_date, format = "%b %d, %Y, %I:%M:%S %p", tz = "UTC")) |> 
  select(activity_id, rdatetime_local, distance) |> 
  arrange(rdatetime_local)

get_recent_total <- function(dat = tmp, id, n){
  at <- tmp$rdatetime_local[tmp$activity_id == id & !is.na(tmp$activity_id)]
  since <- tmp$rdatetime_local[tmp$activity_id == id & !is.na(tmp$activity_id)] - lubridate::days(n)
  sumdist <- tmp |> 
    filter(rdatetime_local > since & rdatetime_local < at) |> 
    pull(distance) |> 
    sum()
  return(sumdist)
}

tmp <- 
  tmp |> 
  mutate(dist_lag90 = activity_id |> map(~get_recent_total(dat = tmp, id = .x, n = 90)) |> unlist())

tmp |> select(rdatetime_local, dist_lag90, distance) |> view()

plot_moving_sum <- 
  tmp |> 
  ggplot(aes(rdatetime_local, dist_lag90)) +
  geom_line() +
  ylab("Total miles run in past 90 days") +
  xlab(NULL) +
  ggtitle("Moving sum for miles run in past 90 days") +
  theme_minimal()
plot_moving_sum

ggsave(
  str_c(getwd(), "/PLOTS/", lubridate::today(), " - 90-day moving sum for distance", ".png"),
  plot_moving_sum,
  device = "png",
  height = 3,
  width = 4,
  scale = 2,
  bg = "white"
)

# plot - moving pace -------------------------------------

plot_moving_pace <-
  runs_all |> 
  ggplot(aes(rdatetime_local, pace_moving)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ splines::bs(x, 16), se = F, size = 1/2, color = 'red') +
  ylab("Minutes per mile (moving pace)") +
  xlab(NULL) +
  ggtitle("Pace over time with trend") +
  theme_minimal()
plot_moving_pace

ggsave(
  str_c(getwd(), "/PLOTS/", lubridate::today(), " - Pace over time", ".png"),
  plot_moving_pace,
  device = "png",
  height = 3,
  width = 4,
  scale = 2,
  bg = "white"
)

plot_moving_pace_plus <-
  runs_all |> 
  ggplot(aes(rdatetime_local, pace_moving)) +
  geom_point(aes(size = distance*1.05), alpha = 1) +
  geom_point(aes(size = distance, color = wet_bulb), alpha = 1) +
  geom_smooth(method = "lm", formula = y ~ splines::bs(x, 16), se = F, size = 1/2, color = 'red') +
  scale_size_continuous(name = "Miles", range = c(1/7,1/7*25), breaks = seq(0,26, by = 5)) +
  scale_color_gradient2(low = "#00B0B0", midpoint = 50.03, high = "#B00000", name = "Wet bulb\ntemperature") +
  ylab("Minutes per mile (moving pace)") +
  xlab(NULL) +
  ggtitle("Pace over time with trend") +
  theme_minimal()
plot_moving_pace_plus

ggsave(
  str_c(getwd(), "/PLOTS/", lubridate::today(), " - Pace plus over time", ".png"),
  plot_moving_pace_plus,
  device = "png",
  height = 3,
  width = 4,
  scale = 2,
  bg = "white"
)

# plot - miles by location,  mapped ---------------------------------------

library(sf)
library(ggmap)

# ggmap::register_google(key = Sys.getenv("GMAPS_KEY"))

# map of us (using geographic center of lower 48)
map <- get_googlemap(center = c(-68.58, 39.83), 
                     size = c(640,640), 
                     scale = 2,
                     messaging = F,
                     maptype = "roadmap",
                     color = "bw",
                     style = 'feature:all|element:labels|visibility:off',
                     zoom = 2)
plot_location <-
  ggmap(map) +
  geom_point(
    data = runs_all |> 
      mutate(lon_start = round(lon_start,1),
             lat_start = round(lat_start,1)) |> 
      group_by(lon_start, lat_start) |> 
      summarise(miles = sum(distance)), 
    aes(
      x=lon_start,
      y=lat_start, 
      size = miles
    ),
    color = 'blue',
    alpha = 0.5
  ) +
  # ggrepel::geom_text_repel(
  #   data = runs_all |> 
  #     mutate(lon_start = round(lon_start,0),
  #            lat_start = round(lat_start,-1)) |> 
  #     group_by(lon_start, lat_start) |> 
  #     summarise(miles = sum(distance)), 
  #   aes(
  #     x=lon_start,
  #     y=lat_start,
  #     label = str_c(miles," mi")
  #   ),
#   size = 8*5/14, max.overlaps = 25
# ) +
ggtitle("Total miles by coordinates",str_c("2018-",max(runs_all$ryear))) +
  scale_size_continuous(guide = 'none', range = c(1,18)) +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_blank())
plot_location

ggsave(
  str_c(getwd(), "/PLOTS/", today(), " - Mileage total mapped", ".png"),
  plot_location,
  device = "png",
  height = 3,
  width = 3,
  scale = 2,
  bg = "white"
)


# plot - performance --------------------------------------------------------

lm_mod <- lm(
  data = runs_all,
  pace_moving ~ 
    distance + 
    I(elevation_gain^1.5) +
    I(sqrt(elevation_loss)) +
    I(wet_bulb^2.5)
)
summary(lm_mod)

runs_with_residuals <- 
  runs_all |> 
  select(filename,
         pace_moving,
         distance,
         elevation_gain,
         elevation_loss,
         cold,
         wet_bulb) |> 
  drop_na() |> 
  mutate(resid = lm_mod$residuals,
         pace_adj = pace_moving + resid*sd(pace_moving)) |> 
  left_join(
    runs_all |> select(filename, starts_with("activity"), rdatetime_local) 
  ) |> 
  select(starts_with("activity"), rdatetime_local, resid, pace_adj, pace_moving, everything())

plot_residuals <- 
  ggplot(runs_with_residuals, aes(y = resid, x = rdatetime_local)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ splines::bs(x, 16), size = 1/2, se = F) +
  theme_minimal() +
  ggtitle(str_wrap("Residuals for pace ~ distance + elevation_gain^1.5 + elevation_loss^.5 + cold:temp + wet bulb^2.5",80),
          str_wrap("Crude estimate of adjusted pace—how fast did I run controlling for route characteristics? Helps account for seasonality; trend line shown feels like it could be interpreted as my baseline performance over time", 115)) +
  theme_minimal() +
  xlab(NULL) + ylab("Residuals")
plot_residuals

ggsave(
  str_c(getwd(), "/PLOTS/", today(), " - Adjusted pace over time", ".png"),
  plot_residuals,
  device = "png",
  height = 3,
  width = 4,
  scale = 2,
  bg = "white"
)

plot_residuals


# test cold threshold -----------------------------------------------------

out <- list()
for(i in 20:45){
  mod_dat <- runs_all |> mutate(cold = ifelse(temp<i,1,0))
  lm_mod <- lm(
    data = mod_dat,
    pace_moving ~ 
      distance + 
      elevation_gain +
      elevation_loss +
      cold:temp +
      I(wet_bulb^2)
  )
  out[[i-19]] <- lm_mod
}

aics <- AIC(
  out[[1]],
  out[[2]],
  out[[3]],
  out[[4]],
  out[[5]],
  out[[6]],
  out[[7]],
  out[[8]],
  out[[9]],
  out[[10]],
  out[[11]],
  out[[12]],
  out[[13]],
  out[[14]],
  out[[15]],
  out[[16]],
  out[[17]],
  out[[18]],
  out[[19]],
  out[[20]],
  out[[21]],
  out[[22]],
  out[[23]],
  out[[24]],
  out[[25]]
)

opt_threshold <- (1:length(out))[aics$AIC==min(aics$AIC)]+19



# fx to find pace differences for lower and upper bounds of swings in predictors --------

get_swing_magnitude <- function(dat, model_obj, quantiles = c(.05,.5,.95)){
  
  coefs <- names(model_obj$coefficients)[-1]
  out <- data.frame(NULL)
  
  for(i in 1:length(coefs)){
    swing <- quantile(dat |> pull(coefs[i]),quantiles,na.rm=T)
    names(swing) <- NULL
    out <- bind_rows(out, data.frame(var = coefs[i], quantile = quantiles, val = swing))
  }
  
  predictions <- NULL
  for(i in 1:length(coefs)){
    test_df <- data.frame(
      V1 = out |> dplyr::filter(var == !!coefs[i], quantile != .5) |> pull(val)
    )
    names(test_df) = coefs[i]
    for(j in coefs[!(coefs %in% coefs[i])]){
      test_df <- test_df |> mutate(!!j := out |> dplyr::filter(var == !!j, quantile == .5) |> pull(val))
    }
    test_df$pred <- predict(
      model_obj,
      test_df#,interval = 'prediction'
    )
    test_df$changed_var <- coefs[i]
    predictions <- bind_rows(predictions, test_df)
  }
  
  final <-
    predictions |>
    pivot_longer(cols = c(-pred,-changed_var)) |> 
    group_by(changed_var, name) |> 
    summarise(
      min_val = min(value),
      max_val = max(value),
      pred_diff_seconds = round((max(pred)-min(pred))*60,0)
    ) |> 
    filter(changed_var==name) |> 
    select(-name) |> 
    arrange(desc(pred_diff_seconds))
  
  final <-
    final |>
    mutate(
      pred_diff_seconds = ifelse(
        coef(model_obj)[names(coef(model_obj)) == changed_var]>0,
        pred_diff_seconds,
        pred_diff_seconds*-1)
    )
  return(final) 
}

# split -------------------------------------------------------------------

# https://stackoverflow.com/questions/66639452/tuning-a-lasso-model-and-predicting-using-tidymodels

runs_split <- rsample::initial_split(
  runs |> 
    select(
      -filename, -rdatetime_local_epoch
      # pace_moving, distance, starts_with("elevation"), dist_lag3, dist_lag90, dew_point
    ) |>
    mutate(
      distance_dew_point = distance * dew_point,
      distance_elevation_gain = distance * elevation_gain
    ), prop = .9)
runs_train <- rsample::training(runs_split)
runs_test <- rsample::testing(runs_split)

folds <- rsample::vfold_cv(runs_train, v = 5, strata = pace_moving, nbreaks = 5)

rec <- recipes::recipe(pace_moving ~ ., data = runs_train) %>%
  recipes::step_center(all_predictors(), -all_nominal()) %>% 
  recipes::step_dummy(all_nominal())

lasso_mod <- 
  parsnip::linear_reg(
    mode = "regression",
    penalty = tune(),
    mixture = 1
  ) %>% 
  parsnip::set_engine("glmnet")

wf <- workflows::workflow() %>%
  workflows::add_model(lasso_mod) %>%
  workflows::add_recipe(rec)

my_grid <- tibble(penalty = 10^seq(-2, -1, length.out = 10))

my_res <- wf %>% 
  tune::tune_grid(resamples = folds,
                  grid = my_grid,
                  control = tune::control_grid(verbose = FALSE, save_pred = TRUE),
                  metrics = yardstick::metric_set(rmse))

best_mod <- my_res %>% select_best("rmse")
best_mod
final_fitted <- finalize_workflow(wf, best_mod) %>%
  fit(data = runs_train)

predict(final_fitted, runs_train)

runs_test <- runs_test |> 
  mutate(
    pace_predicted = predict(final_fitted, runs_test)
  )

(rmse <- sqrt(mean(as.numeric(unlist((runs_test$pace_moving - runs_test$pace_predicted)^2)))))
tidy(final_fitted)

# models ------------------------------------------------------------------

lm_mod <- lm(
  data = runs_all |> select(-filename) ,
  pace_moving ~ 
    distance + 
    I(elevation_gain^1.5) +
    I(sqrt(elevation_loss)) +
    I(wet_bulb^2.5) +
    dist_lag90 
)
summary(lm_mod)

plot(fitted.values(lm_mod), rstandard(lm_mod))

get_swing_magnitude(runs_all, lm_mod, quantiles = c(.05,.5,.95)) # swing for middle 90% of values
quantiles = c(.05,.5,.95)
data.frame(
  distance = predict(lm(pace_moving~distance,runs_all), 
                     data.frame(distance = quantile(runs_all$distance,quantiles))),
  elevation_gain = predict(lm(pace_moving~elevation_gain,runs_all), 
                           data.frame(elevation_gain = quantile(runs_all$elevation_gain,quantiles,na.rm = T))),
  elevation_loss = predict(lm(pace_moving~elevation_loss,runs_all), 
                           data.frame(elevation_loss = quantile(runs_all$elevation_loss,quantiles))),
  elevation_low = predict(lm(pace_moving~elevation_low,runs_all), 
                          data.frame(elevation_low = quantile(runs_all$elevation_low,quantiles))),
  wet_bulb = predict(lm(pace_moving~wet_bulb,runs_all), 
                     data.frame(wet_bulb = quantile(runs_all$wet_bulb,quantiles,na.rm=T))),
  dist_lag90 = predict(lm(pace_moving~dist_lag90,runs_all), 
                       data.frame(dist_lag90 = quantile(runs_all$dist_lag90,quantiles)))
  
) |> t() |> 
  
  as_tibble(rownames = "var") |> 
  mutate(diff = round((`95%`-`5%`)*60,0)) |> 
  arrange(desc(abs(diff))) |> 
  left_join(
    get_swing_magnitude(runs_all, lm_mod, quantiles = c(.05,.5,.95)) |> select(changed_var,pred_diff_seconds),
    by = c("var" = "changed_var")
  )
plot(lm_mod)
acf(lm_mod$residuals)

get_pace_difference_in_min_sec <- function(double_pair){
  diff <- max(double_pair) - min(double_pair)
  print(str_c("Difference is ",floor(diff)," minutes and ", round((diff - floor(diff))*60, 0), " seconds"))
}
predicted_values_for_wet_bulb_iqr <- 
  predict(
    lm_mod, 
    data.frame(
      distance = median(runs_all$distance,na.rm=T),
      elevation_gain = median(runs_all$elevation_gain,na.rm=T),
      elevation_loss = median(runs_all$elevation_loss,na.rm=T),
      wet_bulb = c(quantile(runs_all$wet_bulb,.25,na.rm=T),quantile(runs_all$wet_bulb,.75,na.rm=T)),
      dist_lag90 = median(runs_all$dist_lag90,na.rm=T)
    )
  ); names(predicted_values_for_wet_bulb_iqr) <- NULL
get_pace_difference_in_min_sec(predicted_values_for_wet_bulb_iqr)



get_swing_magnitude(runs_all, lm_mod)

lm_mod_small <- lm(
  data = runs_all |> select(-filename),
  pace_moving ~ 
    distance + 
    elevation_gain + 
    wet_bulb +
    dist_lag90
)
summary(lm_mod_small)
plot(lm_mod_small)
cor(model.matrix(lm_mod_small))
pairwise(model.matrix(lm_mod_small))

car::avPlots(lm_mod_small) # watch out for nonlinearity in x
plot(MASS::studres(lm_mod_small), lm_mod_small$fitted.values) # watch out for nonlinearity in y
ggplot(runs_all, aes(rdatetime_local, pace_moving)) + 
  geom_line() + theme_minimal() # not independent obv
acf(residuals(lm_mod_small))

# independence issues but some evidence that my lag term helps
stats::Box.test(MASS::studres(lm_mod_small),   lag = 1, type = "Ljung-Box")
stats::Box.test(
  MASS::studres(
    lm(
      data = runs_all |> select(-filename),
      as.formula(str_c("pace_moving ~", str_c(names(coef(lm_mod_small)[-1])[names(coef(lm_mod_small)[-1])!="dist_lag90"],collapse = " + ")))
    )), lag = 1, type = "Ljung-Box"
)
# example of independence in time
stats::Box.test(
  MASS::studres(
    lm(
      data = runs |> select(-filename) |> sample_frac(),
      pace_moving ~ distance + elevation_gain + altitude_change + temp + dew_point)
  ), 
  lag = 1, type = "Ljung-Box"
)

plot(runs_all$rdatetime_local, runs_all$pace_moving, type = 'l')
car::qqPlot(runs$pace_moving) # watch for non straight patterns (non-normality)
car::boxCox(lm_mod_small) # transformation needed?
car::vif(lm_mod_small) # watch for higher than 5

lm_mod_small |> 
  broom::tidy() |> 
  arrange(desc(abs(statistic)))



# account for autocorrelation ---------------------------------------------

library(nlme)
# https://stats.stackexchange.com/questions/367957/autoregression-in-nlme-undestanding-how-to-specify-corar1
# https://fromthebottomoftheheap.net/2014/05/09/modelling-seasonal-data-with-gam/
gls_mod <- gls(data = runs_all |> rownames_to_column("id"),
               model = 
                 pace_moving ~ 
                 distance + 
                 elevation_gain + 
                 wet_bulb +
                 dist_lag90,
               correlation = corCAR1(form = ~ 1 | rmonthyear),
               na.action=na.omit, 
               method = "ML")
summary(gls_mod)
acf(residuals(gls_mod))
stats::Box.test(gls_mod$residuals, lag = 1, type = "Ljung-Box")
stats::Box.test(gls_mod2$residuals, lag = 1, type = "Ljung-Box")


gls_mod2 = gls(
  pace_moving ~ 
    distance + 
    elevation_gain + 
    wet_bulb +
    dist_lag90,
  correlation=corCAR1(form = ~ rmonthyear|id),
  data = runs_all |> rownames_to_column("id") |> mutate(rdate = as.integer(rdate)),
  na.action = na.omit,
  method="ML"
)
gls_mod2$varBeta
summary(gls_mod2)
acf(residuals(gls_mod2))

anova(gls_mod, gls_mod2)



data.frame(
  gls_predict = predict(gls_mod, runs_all |> select(distance, wet_bulb, elevation_gain, dew_point, rmonthyear, dist_lag90) |> drop_na()),
  lm_predict = predict(lm_mod, runs_all |> select(distance, wet_bulb, elevation_gain, dew_point, rmonthyear, dist_lag90) |> drop_na()),
  actual = runs_all |> select(distance,wet_bulb,  elevation_gain, dew_point, rmonthyear, pace_moving, dist_lag90) |> drop_na() |> pull(pace_moving)
) |> 
  mutate(diff_gls = gls_predict - actual,
         diff_lm = lm_predict - actual) |> #view()
  summarise(
    rmse_gls = sqrt(mean(diff_gls^2)),
    rmse_lm = sqrt(mean(diff_lm^2))
  )


# predict marathon --------------------------------------------------------

lm_mod <- lm(
  data = runs_all |> select(-filename) |> mutate(elevation_ft_per_mi = elevation_gain / distance),
  pace_moving ~ 
    distance + 
    elevation_gain +
    # elevation_loss +
    elevation_ft_per_mi +
    wet_bulb +
    dist_lag90 
)

summary(lm_mod)
car::vif(lm_mod)

temp <- 59
humidity <- 58
wet_bulb <- temp * atan(0.151977 * (humidity + 8.313659)^(1/2)) + 
  atan(temp + humidity) - 
  atan(humidity - 1.676331) + 
  0.00391838 * (humidity)^(3/2) * atan(0.023101 * humidity) - 
  4.686035

predict(
  lm_mod, 
  newdata = data.frame(
    distance = 26.2,
    elevation_gain = 1089,
    elevation_loss = 981,
    elevation_ft_per_mi = 26.2/1089,
    wet_bulb,
    dist_lag90 = runs_all$dist_lag90[nrow(runs_all)]
  )
)



