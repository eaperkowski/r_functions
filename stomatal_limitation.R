## stomatal_limitation(A_net, Vcmax, elevation, temp):
#
# Calculates Γ* and the Michaelis-Menton for CO2 in Rubisco following 
# leaf temperature equations explained in Bernacchi et al. (2001). Then,
# calculates stomatal limitation from equations explained in Sharkey &
# Farquhar (1982)
#
#
# Arguments:
#    - Anet        =    Net photosynthesis (A)
#    - Vcmax        =   Unstandardized Vcmax measurement (because Anet is 
#                       not standardized) 
#    - leaf.temp    =   leaf temperature by which Anet and Vcmax were measured.
#                       Required to calculate Γ* and Km
#    - Anet_ca      =   Atmospheric CO2 level used for Anet (default: 420ppm)
#    - Rd.meas      =   Calculates as function of Vcmax (default: FALSE)
#    - Rd           =   measured dark respiration. Rd.meas should be set to TRUE
#                       if Rd data are included
#    - K            =   Boolean noting whether temperature is in K or not 
#                       (default: FALSE)
#
# Returns:
#    - Data frame containing Michaelis-Menton constants (Kc, Ko, Km), gammastar,
#      A_mod, and stomatal limitation value (stomLim).
# 
# Note: Stomatal limitation should be a value less than or equal to one
stomatal_limitation <- function(Anet, 
                                Vcmax, 
                                leaf.temp, 
                                Anet_ca = 420,
                                Rd.meas = FALSE, 
                                Rd, 
                                temp = c("C", "K")) {
  
  ## Global constants
  R = 8.314 # universal gas constant (J mol^-1 K^-1)
  Oi = 210 # leaf intercellular O2 concentration (mmol mol^-1)
  
  ## Leaf temperature correction
  leaf.temp <- ifelse(temp == "C", 
                      leaf.temp + 273.15, 
                      ifelse(temp == "K",
                             leaf.temp,
                             NA))

  ## Calculate Kc
  Kc <- 404.9 * exp((79430 * (leaf.temp - 298)) / 
                      (298 * R * leaf.temp))

  ## Calculate Ko
  Ko <- 278.4 * exp((36380 * (leaf.temp - 298)) / 
                      (298 * R * leaf.temp))
  
  ## Calculate Km
  Km <- Kc * (1 + (Oi / Ko))
  
  ## Calculate gamma.star
  gammastar <- 42.75 * exp((37830 * (leaf.temp - 298)) / 
                             (298 * R * leaf.temp))
   
   # Add contingency over whether Rd was measured or not
   if(Rd.meas == FALSE) {
     Rd <- 0.015 * Vcmax
   }
   
   if(Rd.meas == TRUE) {
     Rd <- Rd
   }

   # Calculate A_mod
   Amod = Vcmax * ((Anet_ca - gammastar) / (Anet_ca + Km)) - Rd
   
   # Determine stomatal limitation
   l <- 1 - (Anet / Amod)
   
   return(data.frame(Kc, Ko, Km, gammastar, l))
}


