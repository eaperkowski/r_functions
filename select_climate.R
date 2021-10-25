# selectClimate(df, date.column, date.format, sampling.date, days.since) :
#
#
# Takes a dataset and subsets it based on a selected number of days before
# a given sampling date. This function was originally created to subset
# climate data based on different intervals
#
# Function calls:
#
# df                   =     data frame
# date.column          =     date in a form that can be read by package
#                            lubridate
# date.format          =     takes any date format read by lubridate. These
#                            include: "ymd", "ydm", "mdy","myd", "dmy",
#                            "dym", and "yq". Note: date.column and 
#                            sampling.date function calls must be in same
#                            date format
# sampling.date        =     date of sampling
# days.since           =     desired date range
#
# Returns:
# A data frame containing subsetted values based on days since a given 
# sampling date
selectClimate <- function(df,
                          date.column = "",
                          sampling.date,
                          days.since) {
  df <- data.frame(df)
  df$date <- as.Date(df[, date.column])
  sampling.date <- as.Date(sampling.date)
  
  df <- subset(df, date < as.Date(sampling.date) & date > as.Date(sampling.date) - days.since)
  
  return(df)
}

