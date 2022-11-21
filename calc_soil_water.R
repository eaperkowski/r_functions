# calcSoilWaterCapacity(fsand, fclay, fom, zbed, zmax):
# Calculates soil water holding capacity following equations from
# Saxton & Rauls (2008). Equations listed here are explained in Appendix D
# of Stocker et al. (2020). Function calculates water storage at field
# capacity and permanent wilting point to estimate water holding capacity
# per unit ground area using soil texture as model inputs.
#
# NOTE: all variables here are included in SoilGrids data product
#
# Inputs:
#   - fsand       = Sand content (% by weight)
#   - fclay       = Clay content (% by weight)
#   - fom         = Organic matter content (% by weight)
#   - fgravel     = Gravel content (% by weight, particles > 2mm diameter)
#   - zbed        = bedrock depth (m)
#   - zmax        = maximum allowable bedrock depth. Set to 2 m 
#                   (Stocker et al. (2020)
#
# Outputs
#   - wfc         = Water storage at field capacity
#   - pwp         = Permanent wilting point
#   - whc         = Water holding capacity

calc_soil_water <- function(fsand = NA, fclay = NA, fom = NA,
                            fgravel = NA, zbed = NA, zmax = 2) {
  
  ## Calculate kfc (input for field capacity)
  kfc = (-0.251 * fsand) + (0.195 * fclay) + (0.011 * fom) + 
    0.006*(fsand*fom) - 0.027*(fclay*fom) + 0.452*(fsand*fclay) + 0.299
  
  ## Calculate water storage at field capacity
  wfc = kfc + (1.283 * (kfc)^2 - 0.374*kfc - 0.015)
  
  ## calculate kpwp (input for wilting point)
  kpwp = -0.024*fsand + 0.487*fclay + 0.006*fom + 
    0.005*(fsand*fom) - 0.013*(fclay*fom) + 0.068*(fsand*fclay) + 0.031
    
  ## Calculate permanent wilting point
  pwp = kpwp + (0.14 * kpwp - 0.02)
  
  # Calculate water holding capacity
  whc = (wfc - wpwp)*(1 - fgravel)*min(zbed, zmax)
  
  return(data.frame(wfc, pwp, whc))
  
}