# standardizeLimitations(Vcmax_est, Jmax_est, tLeaf, tLeafMean):
# First, calculates vcmax and jmax entropy values using linear equation derived in 
# Kattge & Knorr (2007). Then, standardizes Vcmax and Jmax estimates to 25
# degrees C using equations from Smith et al., 2019 and Kattge & Knorr (2007).
#
# Arguments:
#    - Vcmax_est    =  Original Vcmax estimate at tLeaf
#    - Jmax_est     =  Original Jmax estimate at tLeaf
#    - tLeaf        =  average leaf temperature during A/ci curve, must be in 
#                      degrees Celsius.
#    - tLeafMean    =  growing season leaf temperature mean. A value representing
#                      the mean of all leaf temperature measurements of an experiment
#                      Temperature must be in degrees Celsius.
# Returns:
#    - List of length two with Vcmax or Jmax estimate standardized to 25 degC
standardizeLimitations <- function(df,
                                   sample.col = "", 
                                   Vcmax_est, 
                                   Jmax_est, 
                                   tLeaf, 
                                   tGrow) {
  df <- data.frame(df)
  df$id <- df[, sample.col]

  ## delta S constants as per Kattge & Knorr 
  a_vcmax <- 668.39
  b_vcmax <- -1.07
  a_jmax <- 659.70
  b_jmax <- -0.75
  
  ## Calculate delta S for Vcmax
  S_vcmax <- a_vcmax + b_vcmax * tGrow
  
  ## Calculate delta S for Jmax
  S_jmax <- a_jmax + b_jmax * tGrow
  
  ## Constants to standardize Vcmax and Jmax estimates to 25 degC
  tK <- tLeaf + 273.15
  tO <- 298.15
  Ha_vcmax <- 71513
  Ha_jmax <- 49884
  Hd <- 200000
  R <- 8.314
  multOneVcmax <- exp((Ha_vcmax * (tK - tO)) / (R * tK * tO))
  multTwoVcmax <- (1 + exp((tO * S_vcmax - Hd)/(R * tO))) / (1 + exp((tK * S_vcmax - Hd)/(R * tK)))
  
  multipliersVcmax <- multOneVcmax * multTwoVcmax
  VcmaxStandard <- Vcmax_est / multipliersVcmax
  
  multOneJmax <- exp((Ha_jmax * (tK - tO)) / (R * tK * tO))
  multTwoJmax <- (1 + exp((tO * S_jmax - Hd)/(R * tO))) / (1 + exp((tK * S_jmax - Hd)/(R * tK)))
  
  multipliersJmax <- multOneJmax * multTwoJmax
  JmaxStandard <- Jmax_est / multipliersJmax
  
  df$vcmax.stand <- VcmaxStandard
  df$jmax.stand <- JmaxStandard
  
  return(df)
}
