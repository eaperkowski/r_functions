# calculate_deltaS (tLeafMean, a, b):
# Calculates entropy value for vcmax or jmax using linear equation derived in 
# Kattge & Knorr (2007) paper.
# Arguments:
#    - tLeafMean    =  growing season leaf temperature mean. A value representing
#                      the mean of all leaf temperature measurements of an experiment
#                      Temperature must be in degrees Celsius.
# Returns:
#    - A vector of length 1 containing either a Vcmax or Jmax delta S value
calculateDeltaS <- function(tLeafMean, a, b) {
  deltaS <- a + b * tLeafMean
  return(deltaS)
}