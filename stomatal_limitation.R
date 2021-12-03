## stomatal_limitation(A_net, Vcmax, elevation, temp):
#
# Calculates Γ* and the Michaelis-Menton for CO2 in Rubisco as per functions by
# Nick G. Smith (GitHub repo: SmithPlantEcophysLab/optimal_vcmax.R). Then,
# determines A assuming full stomatal openness (A_mod) and calculates stomatal 
# limitation.
#
# Pre-loaded functions within function:
#    - calc_patm       = calculates patm from elevation
#    - calc_km_pa      = calculates Michaelis Menton constants
#    - calc_gammastar  = calculates gamma star (Γ*)
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
stomatal_limitation <- function(A_net, Vcmax, elevation, temp, 
                                Rd.meas = FALSE, Rd) {
   ### calc_patm function
   calc_patm = function(z) {
      
      kPo = 101325   # standard atmosphere, Pa (Allen, 1973)
      kTo = 298.15   # base temperature, K (Prentice, unpublished)
      kL = 0.0065    # temperature lapse rate, K/m (Allen, 1973)
      kG = 9.80665   # gravitational acceleration, m/s**2 (Allen, 1973)
      kR = 8.3143    # universal gas constant, J/mol/K (Allen, 1973)
      kMa = 0.028963 # molecular weight of dry air, kg/mol (Tsilingiris, 2008)
      
      patm = kPo*(1.0 - kL*z/kTo)**(kG*kMa/(kR*kL))
      
      patm
   }
   
   ### calc_gammastar function
   calc_gammastar_pa = function(temp, z) {
      
      patm = calc_patm(z)
      rat = calc_patm(z) / calc_patm(0)
      
      #gammastar25 = 42.75  # ppm
      gammastar25 = 4.332 * rat  # Pa
      Hgm=37830 # J mol-1
      R = 8.314        # J K-1 mol-1
      O2 = 2.09476e5 # ppm
      O2_0 = O2 * 1e-6 * calc_patm(0)
      O2_z = O2 * 1e-6 * calc_patm(z)
      
      temp_k = 273.15+ temp
      
      gStar_pa = gammastar25*exp((Hgm/R)*(1/298.15-1/temp_k))
      
      gStar_pa
      
   }
   
   ### Calc_km function
   calc_km_pa = function(temp, z) {
      
      patm = calc_patm(z) 
      rat = patm / calc_patm(0)
      
      R = 8.314        
      O2 = 2.09476e5      
      Kc25 = 41.03 * rat 
      Ko25 = 28210 * rat 
      Hkc = 79430  
      Hko = 36380 
      
      temp_k = 273.15 + temp
      
      Kc_pa =Kc25 * exp(Hkc * ((temp_k - 298.15) / (298.15 * R * temp_k)))
      Ko_pa =Ko25* exp(Hko * ((temp_k - 298.15) / (298.15 * R * temp_k)))
      
      O2_pa = O2 * (1e-6) * patm 
      
      Km_pa = Kc_pa * (1 + O2_pa/Ko_pa)
      
      Km_pa 
      
   }
   
   # Global constants
   K = calc_km_pa(temp = temp, z = elevation)
   gStar_pa = calc_gammastar_pa(temp = temp, z = elevation)
   
   if(Rd.meas == FALSE) {
     Rd <- 0.015 * Vcmax
   }
   
   if(Rd.meas == TRUE) {
     Rd <- Rd
   }

   # Calculate A_mod
   A_mod = Vcmax * ((400 - gStar_pa) / (400 + K)) - Rd
   
   # Determine stomatal limitation
   l <- 1 - (A_net / A_mod)
   
   return(l)
}

