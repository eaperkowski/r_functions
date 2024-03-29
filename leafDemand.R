# leafDemand(vcmax25 = NA, jmax25 = NA, lma = NA, mn = 14, mr = 0.55,
# nr = 0.0114,n = 8, kcat = 3.5, nCyt = 0.124, jmc = 156, c = 0, tau = 1):
#
# Function Description: calculates percent of leaf nitrogen that is allocated
# to Rubisco (n.rubisco), percent of leaf nitrogen allocated to bioenergetics
# (n.bioenergetics), percent of nitrogen allocated to structure (n.struct). 
# The function then calculates leaf nitrogen demand following equations 
# explained in Dong et al. (2022). The function currently only includes 
# parameters for broadleaf deciduous species, but will soon be updated with 
# parameters for evergreen species
#
# Arguments:
#    - vcmax25         = Vcmax estimate standardized to 25degC
#    - mn              = molecular mass of nitrogen (14 g mol^-1)
#    - mr              = molecular mass of Rubisco (0.55 g mol^-1)
#    - nr              = nitrogen concentration of Rubisco (0.0114 mol g^-1)
#    - n               = numbcer of catalytic sits per mol Rubisco (8)
#    - kcat            = catalytic turnover rate (3.5 s^-1)
#    - nCyt            = nitrogen investment to bioenergetics
#    - jmc             = activity of electron transport
#    - c               = resorption efficiency of N
#    - tau             = leaf replacement time (years)
#    - k               = extinction coefficient for Vcmax 
#    - lai             = leaf area index (or total leaf area)
#
# Returns:
# List with percent nitrogen allocated to Rubisco (n.rubisco), percent
# nitrogen allocated to bioenergetics, percent nitrogen allocated to structure 
# (n.struct), and leaf nitrogen demand (n.demand). Note that function will 
# return NA values for any calculation if vcmax/jmax/lma is missing
nDemand <- function(vcmax25 = NA, jmax25 = NA, lma = NA, narea = NA, lai = NA, 
                    mn = 14, mr = 0.55, nr = 0.0114,n = 8, kcat = 3.5, 
                    nCyt = 0.124, jmc = 156, c = 0, tau = 1, k = 0.175) {
  
  ## N in Rubisco
  n.rubisco = (vcmax25 * mn * mr * nr) / (n * kcat)
  
  ## %N in rubisco
  prop.rubisco = n.rubisco / narea
  
  ## N in bioenergetics
  n.bioenergetics = (jmax25 * nCyt) / jmc
  
  ## %N in bioenergetics 
  prop.bioen = n.bioenergetics / narea
  
  ## Leaf nitrogen allocated to structure
  n.struct = 10^-2.67 * (lma ^ 0.99)
  
  ## Leaf nitrogen demand
  leaf.demand = ((1 - c) * n.rubisco) / tau
  
  ## Canopy Rubisco (g m^-2)
  canopy.n.rubisco = ((1 - exp(-k * lai)) * n.rubisco) / k
  
  
  
  ## Return data.frame
  return(list(n.rubisco = n.rubisco, 
              n.bioenergetics = n.bioenergetics, 
              n.struct = n.struct, 
              leaf.demand = leaf.demand,
              canopy.n.rubisco = canopy.n.rubisco))
}