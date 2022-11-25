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
#   - fsand       = Sand content (%, must be in decimal form)
#   - fclay       = Clay content (%, must be in decimal form)
#   - fom         = Organic matter content (%, must be in decimal form). 
#                   Can estimate from soil organic carbon 
#   - fgravel     = Gravel content (%, must be in decimal form)
#   - zbed        = bedrock depth (m)
#   - zmax        = maximum allowable bedrock depth. Set to 2 m 
#                   (Stocker et al. (2020)
#
# Outputs
#   - wfc         = Water storage at field capacity (m3 m^-3)
#   - pwp         = Permanent wilting point (m3 m^-3)
#   - whc         = Water holding capacity (m)

calc_soil_water <- function(id, fsand = NA, fclay = NA, fom = NA,
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
  whc = (wfc - pwp)*(1 - fgravel)*min(zbed, zmax)
  
  return(data.frame(id, wfc, pwp, whc))
  
}

## Tests
# df <- read.csv("/Users/eaperkowski/git/TX_ecolab_leafNitrogen/data_sheets/TXeco_soilgrid_data.csv")
# 
# calc_soil_water(id = df$id,
#                 fsand = df$perc.sand/100,
#                 fclay = df$perc.clay/100,
#                 fom = df$om/1000,
#                 fgravel = df$perc.gravel/100,
#                 zbed = df$bedrock/100)
