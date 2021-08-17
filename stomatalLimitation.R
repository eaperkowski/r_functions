## stomatalLimitation(A_net, Vcmax, elevation, temp):
#
# Calculates Γ* and the Michaelis-Menton for CO2 in Rubisco as per functions by
# Nick G. Smith (GitHub repo: SmithPlantEcophysLab/optimal_vcmax.R). Then,
# determines A assuming full stomatal openness (A_mod) and calculates stomatal 
# limitation.
#
# Pre-loaded functions within function:
#    - calc_patm       = calculates patm from elevation (optimal_vcmax_R/master/functions/patm.R)
#    - calc_km_pa      = calculates Michaelis Menton constants (../functions/calc_km.R)
#    - calc_gammastar  = calculates gamma star (Γ*) (../functions/calc_gammastar.R)
#
# Arguments:
#    - A_net        =  Net photosynthesis (A) at 400ppm CO2
#    - Vcmax        =  Unstandardized Vcmax measurement (because A_net is 
#                      not standardized) 
#    - elevation    =  elevation by which A_net and Vcmax were measured
#    - temp         =  temperature by which A_net and Vcmax were measured
#
# Returns:
#    - Data frame containing Michaelis-Menton constant (K), gammastar, A_mod,
#      and stomatal limitation value (stomLim).
# 
# Note: Stomatal limitation should be a value less than or equal to one
stomatalLimitation <- function(A_net, Vcmax, elevation, temp) {
   ### calc_patm function
   source("https://raw.githubusercontent.com/SmithEcophysLab/optimal_vcmax_R/master/functions/calc_patm.R")
   
   ### calc_gammastar function
   source("https://raw.githubusercontent.com/SmithEcophysLab/optimal_vcmax_R/master/functions/calc_gammastar.R")
   
   ### Calc_km function
   source("https://raw.githubusercontent.com/SmithEcophysLab/optimal_vcmax_R/master/functions/calc_km.R")
   
   # Global constants
   K = calc_km_pa(temp = temp, z = elevation)
   gStar_pa = calc_gammastar_pa(temp = temp, z = elevation)

   # Calculate A_mod
   A_mod = Vcmax * ((400 - gStar_pa) / (400 + K))
   
   # Determine stomatal limitation
   l <- 1 - (A_net / A_mod)
   
   return(data.frame(K = K,
                     gStar = gStar_pa,
                     A_mod = A_mod,
                     stomLim = l))
}