##################################
##################################
ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg))
        install.packages(new.pkg, dependencies = TRUE)
    try(sapply(pkg, require, character.only = TRUE), silent = TRUE)
}
packages <- c("xts", "zoo", "astsa","dplyr","prophet","ggplot2","tidyr")
ipak(packages)
####################################
####################################
df <- read.csv("./data/example_wp_log_peyton_manning.csv", header = TRUE)
####################################
####################################
m <- prophet(df)
#####################################
#### predictions are made on a dataframe with a column ds containing
#### the dates for which predictions are to be made
#####################################
future <- make_future_dataframe(m, periods = 365)
tail(future)
######################################
######################################
# use the generic predict function to get our forecast
######################################
######################################
# The forecast object is a dataframe with a column yhat containing the forecast
# it has additional columns for uncertainty intervals and seasonal components
# 
forecast <- predict(m, future)
# 
tail(forecast[c("ds", "yhat", "yhat_lower", "yhat_upper")])
# can use the generic plot function to plot the forecast, by passing
# in the model and the forecast dataframe
plot(m, forecast)
# You can use the prophet_plot_components function to see the forecast broken 
# down into trend, weekly seasonality, and yearly seasonality.
prophet_plot_components(m, forecast)
######################################################
# forecasting growth
df <- read.csv("./data/example_wp_log_R.csv", header = TRUE)
# 
df_xts <- xts(df$y, order.by = as.Date(df$ds))
# 
plot(df_xts)

# specify the carrying capacity in column cap
# here we assume a particular value, but this would usually be set using data or expertise 
# about the market size
df$cap <- 8.5

# note that cap must be specified for every row in the data frame, and that is does not have to be constant
# if the market size is growing, then cap can be an increasing sequence

# fit the model as before
m <- prophet(df, growth = "logistic")

# future
# make a dataframe for future predictions, we must also specify the capacity in the future
# here we keep capacity constant at the same value as in the history
# and forecast 5 years into the future
# 
future <- make_future_dataframe(m, periods = 1826)
future$cap <- 8.5
fcst <- predict(m, future)
plot(m, fcst)



###############################################################
# Saturating minimum
# the logistic growth model can also handle a saturating minimum, which is specified
# with a column floor in the same way as the cap column specifies the maximum
# 
df$y <- 10 - df$y
df$cap <- 6
df$floor <- 1.5
future$cap <- 6
future$floor <- 1.5
m <- prophet(m, future)
plot(m, fcst)

# To use a logistic growth trend with a saturating minimum, a maximum capacity must also be specified.








##################################################
### real time series frequently have abrupt changes
### in their trajectories. 
### by default, prophet will automatically detect these changepoints
### and will allow the trend to adapt appropriately.
### if you want to have finer control, there are several input arguments
### you can use

### Prophet detects changepoints by first specifying a large number of potential
### changepoints at which the rate is allowed to change
### it then puts a sparse prior on the magnitudes of the rate changes
### L1 regularization
### n_changepoints
### 
plot(m, forecast) + add_changepoints_to_plot(m)

### changepoint_range

### adjust trend flexibility
### if the trend changes are being overfit or underfit, you can adjust the strength
### of the sparse prior using the input argument changepoint_prior_scale
### by default, this parameter is set to 0.05
### increasing it will make the trend more flexible
m <- prophet(df, changepoint.prior.scale = 0.5)
forecast <- predict(m, future)
plot(m, forecast)
### decreasing it will make the trend less flexible

### cross validation

### specifying the locations of the change points
m <- prophet(df, changepoints = c('2014-01-01'))
forecast <- predict(m, future)
plot(m, forecast)



##############################################################################
##############################################################################
### Seasonality, Holiday effects and regressors
### modeling holidays and special events
##############################################################################
##############################################################################

#
# If you have holidays or other recurring events that you’d like to model, 
# you must create a dataframe for them. It has two columns (holiday and ds) and a row 
# for each occurrence of the holiday. It must include all occurrences of the holiday, both 
# in the past (back as far as the historical data go) and in the future 
# (out as far as the forecast is being made). If they won’t repeat in the future, 
# Prophet will model them and then not include them in the forecast.

# can include columns lower_window and upper_window which extend the holiday out to
# lower window, upper window days around the date
# 

library(dplyr)
playoffs <- data_frame(
    holiday = 'playoff',
    ds = as.Date(c('2008-01-13', '2009-01-03', '2010-01-16',
                   '2010-01-24', '2010-02-07', '2011-01-08',
                   '2013-01-12', '2014-01-12', '2014-01-19',
                   '2014-02-02', '2015-01-11', '2016-01-17',
                   '2016-01-24', '2016-02-07')),
    lower_window = 0,
    upper_window = 1
)
# 
superbowls <- data_frame(
    holiday = 'superbowl',
    ds = as.Date(c('2010-02-07', '2014-02-02', '2016-02-07')),
    lower_window = 0,
    upper_window = 1
)
# 
holidays <- bind_rows(playoffs, superbowls)

# 
m <- prophet(df, holidays = holidays)
forecast <- predict(m, future)

# The holiday effect can be seen in the forecast dataframe:
# 
# R
forecast %>% 
    select(ds, playoff, superbowl) %>% 
    filter(abs(playoff + superbowl) > 0) %>%
    tail(10)

#
# The holiday effects will also show up in the components plot, 
# where we see that there is a spike on the days around playoff appearances, 
# with an especially large spike for the superbowl:
prophet_plot_components(m, forecast)

# built-in country holidays
m <- prophet(holidays = holidays)
m <- add_country_holidays(m, country_name = 'US')
m <- fit.prophet(m, df)

# 
m$train.holiday.names

#
# R
forecast <- predict(m, future)
prophet_plot_components(m, forecast)


#######################################################
## Fourier order of seasonalities
#######################################################
m <- prophet(df)
prophet::plot_yearly(m)

########################################################
# the default values are often appropriate, but they can be increased when
# the seasonality needs to fit higher-frequency changes, and generally less smooth
# the fourier order can be specified for each built-in seasonality when instantiating the model
# here it is increased to 20
m <- prophet(df, yearly.seasonality = 20)
prophet:::plot_yearly(m)



#########################################################
# Increasing the number of Fourier terms allows the seasonality to fit
# faster changing cycles, but can also lead to overfitting
##########################################################


# Specifying custom seasonalities
# Prophet will by default fit weekly and yearly seasonalities
# if the time series is more than two cycles long
# it will also fit daily seasonality for a sub-daily time series
# you can add other seasonalities (monthly, quarterly, hourly)
# using the add_seasonality method
# The inputs to this function are a name, the period of the seasonality in days
# and the fourier order for the seasonality
# default
# fourier order of 3 for weekly
# fourier order of 10 for yearly

m <- prophet(weekly.seasonality=FALSE)
m <- add_seasonality(m, name='monthly', period=30.5, fourier.order=5)
m <- fit.prophet(m, df)
forecast <- predict(m, future)
prophet_plot_components(m, forecast)



####################################
### Seasonalities that depend on other factors
####################################
# in some instances, the seasonality may depend on other factors, such as a weekly seasonal pattern
# that is different during the summer that it is during the rest of the year
# or a daily seasonal pattern that is different on weekends vs on weekdays
# these types of seasonal can be modeled using conditional seasonalities
# 

is_nfl_season <- function(ds) {
    dates <- as.Date(ds)
    month <- as.numeric(format(dates, '%m'))
    return(month > 8 | month < 2)
}
df$on_season <- is_nfl_season(df$ds)
df$off_season <- !is_nfl_season(df$ds)

# 
m <- add_seasonality(m, name='weekly_on_season', period=7, fourier.order=3, condition.name='on_season')
m <- add_seasonality(m, name='weekly_off_season', period=7, fourier.order=3, condition.name='off_season')
m <- fit.prophet(m, df)

future$on_season <- is_nfl_season(future$ds)
future$off_season <- !is_nfl_season(future$ds)
forecast <- predict(m, future)
prophet_plot_components(m, forecast)

# 


####################################
####################################
# Prior scale for holidays and seasonality
####################################
####################################
#
# If you find that the holidays are overfitting, you can adjust their prior scale to 
# smooth them using the parameter holidays_prior_scale. By default this parameter is 10,
# which provides very little regularization. Reducing this parameter dampens holiday effects:

m <- prophet(df, holidays = holidays, holidays.prior.scale = 0.05)
forecast <- predict(m, future)
forecast %>% 
    select(ds, playoff, superbowl) %>% 
    filter(abs(playoff + superbowl) > 0) %>%
    tail(10)

#
# R
m <- prophet()
m <- add_seasonality(
    m, name='weekly', period=7, fourier.order=3, prior.scale=0.1)


####################################
# Additional regressors 
# additional regressors can be added to the linear part of the model using the add_regressor method or
# function. A column with the regressor value will need to be present in both the fitting and prediction
# dataframes
# for example, we can add an additional effect on Sundays during the NFL season. 
# On the components plot, this effect will show up in the "extra_regressors" plot
# 
nfl_sunday <- function(ds) {
    dates <- as.Date(ds)
    month <- as.numeric(format(dates, '%m'))
    as.numeric((weekdays(dates) == "Sunday") & (month > 8 | month < 2))
}
df$nfl_sunday <- nfl_sunday(df$ds)

m <- prophet()
m <- add_regressor(m, 'nfl_sunday')
m <- fit.prophet(m, df)

future$nfl_sunday <- nfl_sunday(future$ds)

forecast <- predict(m, future)
prophet_plot_components(m, forecast)















#############################################
#############################################
####### Multiplicative seasonality
#############################################
#############################################
# additive seasonality might not always work
# example: airline passengers
# 

df <- read.csv('./data/example_air_passengers.csv', header = TRUE)
m <- prophet(df)
future <- make_future_dataframe(m, 50, freq = 'm')
forecast <- predict(m, future)
plot(m, forecast)

# this time series has a clear yearly cycyle
# but the seasonality in the forecast is too large at the start
# of the time series and too small at the end
# in this time series, the seasonality is not a constant additive factor
# as assumed by Prophet


# multiplicative factor
# 
m <- prophet(df, seasonality.mode = 'multiplicative')
forecast <- predict(m, future)
plot(m, forecast)

# 
# R
# The components figure will now show the seasonality as a percent of the trend:
prophet_plot_components(m, forecast)


# With seasonality_mode = "multiplicative", holiday effects will also be
# modeled as multiplicative
# Any added seasonalities or extra regressors will by default use whatever seasonality_mode is set to, but can be overriden by specifying mode='additive' or 
# mode='multiplicative' as an argument when adding the seasonality or regressor.

# 
m <- prophet(seasonality.mode = 'multiplicative')
m <- add_seasonality(m, 'quarterly', period = 91.25, fourier.order = 8, mode = 'additive')
m <- add_regressor(m, 'regressor', mode = 'additive')

# # 
# Additive and multiplicative extra regressors will show up in 
# separate panels on the components plot. Note, however, 
# that it is pretty unlikely to have a mix of additive and multiplicative seasonalities, 
# so this will generally only be used 
# if there is a reason to expect that to be the case.








###############################
# Uncertainty intervals
###############################
# by default, prophet will return uncertainty intervals for the forecast yhat
# there are several important assumptions behind these uncertainty intervals
# three sources of uncertainty in the forecast
# uncertainty in the trend
# uncertainty in the seasonality estimates
# additional observation noise

# uncertainty in the trend
# 
m <- prophet(df, interval.width = 0.95)
forecast <- predict(m, future)

# uncertainty in seasonality
# By default Prophet will only return uncertainty in the trend and observation noise. 
# To get uncertainty in seasonality, you must do full Bayesian sampling. 
# This is done using the parameter mcmc.samples (which defaults to 0).
m <- prophet(df, mcmc.samples = 300)
forecast <- predict(m, future)

#
prophet_plot_components(m, forecast)

# 
# You can access the raw posterior predictive samples in Python using the method m.predictive_samples(future), 
# or in R using the function predictive_samples(m, future).











###########################################
###########################################
### Outliers
###########################################
###########################################
df <- read.csv('./data/example_wp_log_R_outliers1.csv')
m <- prophet(df)
future <- make_future_dataframe(m, periods = 1096)
forecast <- predict(m, future)
plot(m, forecast)

# The trend forecast seems reasonable, but the uncertainty intervals seem way too wide. 
# Prophet is able to handle the outliers in the history, 
# but only by fitting them with trend changes. 
# The uncertainty model then expects future trend changes of similar magnitude.

# The best way to handle outliers is to remove them - Prophet has no problem with missing data. 
# If you set their values to NA in the history but leave the dates in future, 
# then Prophet will give you a prediction for their values.

outliers <- (as.Date(df$ds) > as.Date('2010-01-01')
             & as.Date(df$ds) < as.Date('2011-01-01'))
df$y[outliers] = NA
m <- prophet(df)
forecast <- predict(m, future)
plot(m, forecast)


#
df <- read.csv('./data/example_wp_log_R_outliers2.csv')
m <- prophet(df)
future <- make_future_dataframe(m, periods = 1096)
forecast <- predict(m, future)
plot(m, forecast)


#
outliers <- (as.Date(df$ds) > as.Date('2015-06-01')
             & as.Date(df$ds) < as.Date('2015-06-30'))
df$y[outliers] = NA
m <- prophet(df)
forecast <- predict(m, future)
plot(m, forecast)


















####################################
####################################
### non daily data
####################################
####################################
# Prophet can make forecasts for time series with sub-daily observations by passing 
# in a dataframe with timestamps in the ds column. 
# The format of the timestamps should be YYYY-MM-DD HH:MM:SS
df <- read.csv('./data/example_yosemite_temps.csv')
m <- prophet(df, changepoint.prior.scale=0.01)
future <- make_future_dataframe(m, periods = 300, freq = 60 * 60)
fcst <- predict(m, future)
plot(m, fcst)

# 
prophet_plot_components(m, fcst)


# You can use Prophet to fit monthly data. However, the underlying model 
# is continuous-time, which means that you can get strange results if you fit the model 
# to monthly data and then ask for daily forecasts. 
# Here we forecast US retail sales volume for the next 10 years:

# 
df <- read.csv('./data/example_retail_sales.csv')
m <- prophet(df, seasonality.mode = 'multiplicative')
future <- make_future_dataframe(m, periods = 3652)
fcst <- predict(m, future)
plot(m, fcst)

# only predict at the month level
future <- make_future_dataframe(m, periods = 120, freq = 'month')
fcst <- predict(m, future)
plot(m, fcst)

# 

















###############################################
###############################################
###### Diagnostics
###############################################
###############################################

###### Prophet includes functionality for time series cross validation
###### to measure forecast error using historical data
###### 

# Here we do cross-validation to assess prediction performance on a horizon of 365 days, 
# starting with 730 days of training data in the first cutoff and then making predictions every 180 days. 
# On this 8 year time series, this corresponds to 11 total forecasts.

df.cv <- cross_validation(m, initial = 730, period = 180, horizon = 365, units = 'days')
head(df.cv)


# custom cutoffs
# Custom cutoffs can also be supplied as a list of dates to the cutoffs keyword in 
# the cross_validation function in Python and R. For example, three cutoffs six months apart,
# would need to be passed to the cutoffs argument in a date format like:
cutoffs <- as.Date(c('2013-02-15', '2013-08-15', '2014-02-15'))
df.cv2 <- cross_validation(m, cutoffs = cutoffs, horizon = 365, units = 'days')


# performance metrics
# yhat, yhat_lower, yhat_upper compared to y
# as a function of the distance from the cutoff 
# the statistics computed are MSE, RMSE, mean absolute error,
# mean absolute percent error
df.p <- performance_metrics(df.cv)
head(df.p)


# 
plot_cross_validation_metric(df.cv, metric = 'mape')


# parallelizing cross validation
#



########################
### saving models
########################

### R rds
### python, json




##########################
## flat trends