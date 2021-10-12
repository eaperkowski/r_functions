# perc_change(value_1, value_2, percent = TRUE):
#
# Arguments:
#    - value_1    =  First value (usually a mean value) used to calculate percent 
#                    change
#    - value_2    =  Second value (usually a mean value) used to calculate percent 
#                    change
#    - percent    =  Boolean where percent = TRUE calculates percent change and
#                    percent = FALSE calculates the decimal form of the percentage
#
# Returns:
#    - Vector that lists percent change. If percent change is negative, it is a 
#      percent decrease. If percent change is positive, it is a percent increase
perc_change <- function(value_1, value_2, percent = TRUE) {
  change = (value_2 - value_1) / value_1
  
  if(percent == TRUE) return(change * 100)
  
  if(percent == FALSE) return(change)
}
