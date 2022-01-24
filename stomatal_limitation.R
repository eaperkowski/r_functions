## stomatal_limitation(A_net, Vcmax, elevation, temp):
#
# Calculates Γ* and the Michaelis-Menton for CO2 in Rubisco following 
# leaf temperature equations explained in Bernacchi et al. (2001). Then,
# calculates stomatal limitation from equations explained in Sharkey &
# Farquhar (1982)
#
#
# Arguments:
#    - A_net        =  Net photosynthesis (A) at 400ppm CO2
#    - Vcmax        =  Unstandardized Vcmax measurement (because A_net is 
#                      not standardized) 
#    - elevation    =  elevation by which A_net and Vcmax were measured. Required
#                      to calculate Γ* and Km
#    - temp         =  temperature by which A_net and Vcmax were measured. Required
#                      to calculate Γ* and Km
#
# Returns:
#    - Data frame containing Michaelis-Menton constant (K), gammastar, A_mod,
#      and stomatal limitation value (stomLim).
# 
# Note: Stomatal limitation should be a value less than or equal to one
stomatal_limitation <- function(A_net, Vcmax, elevation, leaf.temp, 
                                Rd.meas = FALSE, Rd) {
   require(rpmodel)
   
   
   # Global constants
   p.atm = patm(elv = elevation)
   K = kmm(tc = leaf.temp, patm = p.atm)
   gamma.star = gammastar(tc = leaf.temp, patm = p.atm)
   
   # Add contingency over whether Rd was measured or not
   if(Rd.meas == FALSE) {
     Rd <- 0.015 * Vcmax
   }
   
   if(Rd.meas == TRUE) {
     Rd <- Rd
   }

   # Calculate A_mod
   A_mod = Vcmax * ((400 - gamma.star) / (400 + K)) - Rd
   
   # Determine stomatal limitation
   l <- 1 - (A_net / A_mod)
   
   return(data.frame("l" = l,
               "gamma.star" = gamma.star, 
               "Km" = K))
}