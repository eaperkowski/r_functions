library("minpack.lm")

aciFit <- function(data, 
                   photo_vars = list(A = "A", Ci = "Ci",  Tleaf = "Tleaf",
                                     PPFD = "Qin", Rd = "rd"),
                    ci.transition = 400,
                    ci.transition2 = 1000,
                    useRd = TRUE,
                    fitTPU = FALSE) {
  
  #############################################################################
  ## Read data and convert to list. NOTE: Tleaf, PPFD, and Rd are all calculated
  ## as mean values, so only one value will appear in list
  #############################################################################
  if(useRd) {
  df <- data.frame(A = data[, photo_vars[[1]]],
             Ci = data[, photo_vars[[2]]],
             Tleaf = data[, photo_vars[[3]]],
             PPFD = data[, photo_vars[[4]]],
             Rd = data[, photo_vars[[5]]])
  }
  
  if(!useRd) {
    df <- data.frame(A = data[, photo_vars[[1]]],
               Ci = data[, photo_vars[[2]]],
               Tleaf = data[, photo_vars[[3]]],
               PPFD = data[, photo_vars[[4]]],
               Rd = NA)
  }
  
  #############################################################################
  ## Calculate gammastar, Kc, Ko, Km based on leaf temp using eqns. from
  ## Bernacchi & Long (2001) and Medlyn et al. (2002)
  #############################################################################
  
  # Constants
  gammastar25 = 42.75 # Bernacchi & Long (2001); μmol mol^-1
  Ko25 = 278.4 # Bernacchi & Long (2001); μmol mol^-1
  Kc25 = 404.9 # Bernacchi & Long (2001); μmol mol^-1
  Hgs = 37830 # Bernacchi and Long (2001); converted from kJ/mol to J/mol
  Hko = 36380 # Bernacchi and Long (2001); converted from kJ/mol to J/mol
  Hkc = 79430 # Bernacchi and Long (2001); converted from kJ/mol to J/mol
  R = 8.314 # Universal gas constant
  
  # Write list with temperature functions for each of gammastar, Ko, and Kc. 
  # Using parameters set from Bernacchi & Long (2001) Then, calculate Km 
  # following Eqn from Medlyn et al. (2002)
  df.coefs <- list(gammastar = gammastar25 * 
                     exp((Hgs * (mean(df$Tleaf) + 273.15 - 298))/
                           (298 * R * (mean(df$Tleaf) + 273.15))),
                   Ko = Ko25 * exp((Hko * (mean(df$Tleaf) + 273.15 - 298))/
                                     (298 * R * (mean(df$Tleaf) + 273.15))),
                   Kc = Kc25 * exp((Hkc * (mean(df$Tleaf) + 273.15 - 298))/
                                     (298 * R * (mean(df$Tleaf) + 273.15))))
  df.coefs$Km = df.coefs[["Kc"]] * (1 + (0.0020 * df.coefs[["Ko"]]))
  
  #############################################################################
  ## Prepare for curve fitting by subsetting list of A and Ci values such that
  ## Ci values  are equal or less than the set Ci transition/inflection point
  #############################################################################
  
  
  # If fitTPU = TRUE:
  #     - Subset list of A and Ci values from df such that data points used to
  #       fit Vcmax only include Ci values that are equal or less than the
  #       first Ci transition/inflection point
  #
  #     - Subset list of A and Ci values from df such that data points used to
  #       fit Jmax only include Ci values that are between the first and second
  #       set Ci transition/inflection points. Note that this is different if
  #       fitTPU = FALSE
  #
  #     - Subset list of A and Ci values from df such that data points used to
  #       fit TPU only include Ci values that are equal or greater than the
  #       second Ci transition/inflection point

  if(fitTPU) {
    
    data$Vcmax_pred <- (data$C - gamma_star) / (data$C + K_M)
    data$Jmax_pred <- (data$C - gamma_star) / (data$C + 2 * gamma_star)
    data$TPU_part <- (data$C - gamma_star) / (data$C - (1 + 3 * alpha_g) *
                                                  gamma_star)
    
    ## Vcmax curve fit
    vcmax.df <- subset(data, Ci < ci.transition)
    
    vcmax.fit <- A ~ ((vcmax*Ci)/ (Ci + Kc*(1 + (O/Ko)))) - Rd
    
    if(Rd) {
      
      nls(formula = vcmax.fit,
          start = list(vcmax = 50, Rd = 1), data = )
      
    }
    
    if(!Rd) {
      
    }
    
    ## Jmax curve fit
    jmax.df <- subset(data, Ci > ci.transition & Ci < ci.transition2)
    
    if(Rd) {
      
    }
    
    if(!Rd) {
      
    }
    
    ## Tpu curve fit
    tpu.df <- subset(data, Ci > ci.transition2)
    
    if(Rd) {
      
    }
    
    if(!Rd) {
      
    }
    
    
  }
  
  # If fitTPU = FALSE:
  #     - Subset list of A and Ci values from df such that data points used to
  #       fit Vcmax only include Ci values that are equal or less than the
  #       first Ci transition/inflection point
  #
  #     - Subset list of A and Ci values from df such that data points used to
  #       fit Jmax only include Ci values that are greater than the first Ci
  #       transition/inflection point. This is because there is no TPU-limited
  #       part of the curve
  if(!fitTPU) {
    ## Vcmax curve fit
    vcmax.df <- subset(data, Ci < ci.transition)
    
    if(Rd) {
      
      
    }
    
    if(!Rd) {
      
    }
    
    ## Jmax curve fit (note change in ci.transition subset conditions)
    jmax.df <- subset(data, Ci > ci.transition)
    
    if(Rd) {
      
    }
    
    if(!Rd) {
      
    }
  }
  
  #############################################################################
  ## Fit curves
  #############################################################################
  # Vcmax
  #vcmax.fit <- nls(formula = A ~ (vcmax*Ci)/ (Ci + Kc*(1 + (O/Ko))))
  
  # Jmax
  #jmax.fit <- nls(formula = A ~)
  
  # TPU


  
  
  output <- list(raw_data = as.data.frame(df), 
                 temp_resp = df.coefs, 
                 transition_points = list(ci.transition = ci.transition,
                                          ci.transition2 = ci.transition2))
  
  return(output)
  
}
 
## Sample response curve
test.df <- read.csv("/Users/eaperkowski/git/NxI_soybean_phys/licor_data_cleaned/aci/2021-10-14_aci_soybean_ozz_cleaned.csv") %>%
  filter(id == "r16_hn_yi") %>%
  mutate(Tleaf = TleafEB + 273.15) %>%
  select(A, Ci, Tleaf, Qin) %>%
  slice(-1, -14, -15)

## Test fit
test <- aciFit(test.df, useRd = TRUE)
test$raw_data
test$temp_resp
test$transition_points


vcmax.fit <- .[["raw_data"]][["A"]] ~ (vcmax * .[["raw_data"]][["Ci"]]) / (.[["raw_data"]][["Ci"]] + .[["temp_resp"]][["Kc"]] * (1 + (0.002 / .[["temp_resp"]][["Ko"]])))



library(photosynthesis)
fit_aci_response(data = test.df, 
                 varnames = list(A_net = "A",
                                 T_leaf = "Tleaf",
                                 C_i = "Ci",
                                 PPFD = "Qin"),
                 useg_mc = FALSE,
                 fitTPU = FALSE)





