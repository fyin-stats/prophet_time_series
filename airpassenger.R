#####################
#### Time series modeling with prophet
#####################

#### prophet treats this as a cruve fitting task
#### 
ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg))
        install.packages(new.pkg, dependencies = TRUE)
    try(sapply(pkg, require, character.only = TRUE), silent = TRUE)
}
packages <- c("xts", "zoo", "astsa","dplyr","prophet","ggplot2","tidyr","lubridate")
ipak(packages)

#####
# https://www.geeksforgeeks.org/time-series-analysis-using-facebook-prophet-in-r-programming/
# https://www.analyticsvidhya.com/blog/2018/05/generate-accurate-forecasts-facebook-prophet-python-r/
# https://facebook.github.io/prophet/docs/quick_start.html#r-api
# https://www.r-bloggers.com/2017/10/automatic-time-series-forecasting-with-prophet/


# geeks for geeks
ap <- read.csv("./data/example_air_passengers.csv", header = TRUE)
#
ap_xts <- xts(ap$y, order.by = as.Date(ap$ds))
# visualization
plot(ap_xts)

#

# call the prophet to fit the model
m <- prophet(ap)

# make predictions
future <- make_future_dataframe(m, periods = 365)

#
tail(future)

# forecast using predictions

# Forecast 
forecast <- predict(m, future) 
tail(forecast[c('ds', 'yhat',  
                'yhat_lower', 'yhat_upper')]) 


# plot the forecast
plot(m, forecast)

# plot the trend
prophet_plot_components(m, forecast)

# 
# The above graph shows the trend of the dataset that air passengers have been increased over a given period of time. In second graph, it shows seasonality of the dataset over a period of time 
# i.e., yearly and signifies that air passengers were maximum between months from June to August.



# use lubridate to change time zone
# 
pb.txt <- "2009-06-03 19:30"  
pb.date <- as.POSIXct(pb.txt, tz="Europe/London") 
# 
pb.date <- lubridate::with_tz(pb.date, "America/Los_Angeles")