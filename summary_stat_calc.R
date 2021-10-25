# summary_stat_calc(data, n):
# Provides mean, standard deviation, standard error, and confidence intervals
# using a row of data and the number of data observations

#
# Arguments:
#    - data        = a column or list of data to calculate summary statistics
#    - n           = the number of observations from column or list of data
# Returns:
#    - Data frame with mean, standard deviation, standard error, lower CI, and 
#      upper CI. Values are converted to class(numeric)
summary_stat_calc <- function(data, n) {
  
  mean <- mean(data, na.rm = TRUE)
  stdev <- sd(data, na.rm = TRUE)
  se <-  stdev / sqrt(n)
  LCI <- mean - (1.96 * se)
  UCI <- mean + (1.96 * se)
  
  return(data.frame(mean = as.numeric(mean),
                    stdev = as.numeric(stdev),
                    se = as.numeric(se),
                    LCI = as.numeric(LCI),
                    UCI = as.numeric(UCI)))
}