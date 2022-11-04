###############################################################################
# p_rubisco(vcmax25, narea):
###############################################################################
#
# Calculates proportion of leaf nitrogen in Rubisco following equations from
# Niinemets et al. (1997) and Niinements et al. (1998)
#
# Function arguments:
#   - vcmax25     = maximum Rubisco carboxylation rate, standardized to 25degC
#                  (μmol m^-2 s^-1)
#   - narea       = leaf nitrogen per leaf area (gN m^-2)
#
# Returns:
# Vector with proportion of leaf N to Rubisco (p_rubisco; gN Rubisco gN^-1)
p_rubisco <- function(vcmax25, narea){
  
  vcr = 20.5 #umol CO2 (g Rubisco)-1 s-1 at 25C
  # vcmax25 in umol m-2 s-1
  # marea is g m-2
  # nmass is gN g-1
  # 6.25 converts nitrogen to protein (rubisco)
  
  p_rubisco = vcmax25 / (vcr * narea * 6.25)
  
  p_rubisco # g N Rubisco / g N
  
}

###############################################################################
# n_rubisco(vcmax25):
###############################################################################
#
# Calculates leaf nitrogen allocated to Rubisco following equations from
# Niinemets et al. (1997)
#
# Function arguments:
#   - vcmax25     = maximum Rubisco carboxylation rate, standardized to 25degC
#                  (μmol m^-2 s^-1)
#
# Returns:
# Vector with leaf N allocation to Rubisco (n_rubisco; gN Rubisco m^-2)
n_rubisco <- function(vcmax25){

  # Constants
  Mr = 550000 # g Rubisco per mol Rubisco
  Nr = 0.0114 # mol N per g Rubisco
  Mn = 14 # gN per mol N
  nr =  8 # mol Rubisco site per mol Rubisco
  kcat = 3500000 # µmol CO2 per mol Rubisco site per second
  
  # Calc N allocated to rubisco
  n_rubisco = vcmax25 * Mr * Nr * Mn * (1 / nr) * (1 / kcat)
  n_rubisco
}

###############################################################################
# p_bioenergetics(jmax25, narea):
###############################################################################
#
# Calculates proportion of leaf nitrogen allocated to bioenergetics. Function
# follows equations from Niinemets et al. (1997) and Niinements et al. (1998)
#
# Function arguments:
#   - jmax25     = maximum RuBP regeneration rate, standardized to 25degC
#                  (μmol m^-2 s^-1)
#   - narea      = leaf nitrogen per leaf area (gN m^-2)
#
#
# Returns:
# Vector with proportion of leaf N to bioenergetics (p_bioenergetics; 
# g N bioenergetics g N^-1)
p_bioenergetics <- function(jmax25, narea){
  
  # Constants
  jmc = 156 # capacity of electron transport per unit of cytochrome f (umol electrons (umol cyt f)-1 s-1)
  # jmax25 in umol m-2 s-1
  # marea is g m-2
  # nmass is gN g-1
  # 8.06 converts nitrogen to protein (cytf)
  
  # Calc proportion of N in bioenergetics
  p_bioenergetics = jmax25 / (jmc * narea * 8.06)
  
  p_bioenergetics # g N bioenergetics / g N
  
}

###############################################################################
# n_bioenergetics(jmax25, narea):
###############################################################################
#
# Calculates leaf nitrogen allocated to bioenergetics. Function
# follows equations from Niinemets et al. (1997)
#
# Function arguments:
#   - jmax25     = maximum RuBP regeneration rate, standardized to 25degC
#                  (μmol m^-2 s^-1)
#
# Returns:
# Vector with leaf N allocated to bioenergetics (n_bioenergetics; gN 
# bioenergetics m^-2)

n_bioenergetics = function(jmax25) {
  # Constants
  Ncyt = 0.124 # N investment in bioenergetics; gN (µmol cyt f)-1
  jmc = 156 # capacity of electron transport per unit of cytochrome f (umol electrons (umol cyt f)-1 s-1)
  
  # Calc N invested to bioenergetics
  n_bioenergetics = (jmax25 * Ncyt) / jmc
  n_bioenergetics
  
}

###############################################################################
# p_lightharvesting(chlorophyll, nmass):
###############################################################################
#
# Calculates proportion of leaf nitrogen allocated to light harvesting. Function
# follows equations from Niinemets et al. (1997) and Niinements et al. (1998)
#
# Function arguments:
#   - chlorophyll = chlorophyll content (mmol g^-1)
#   - nmass       = leaf nitrogen per leaf mass (g N g^-1)
#
#
# Returns:
# Vector with proportion of leaf N to light harvesting (p_lightharvesting; 
# g N light harvesting g N^-1)
p_lightharvesting <- function(chlorophyll, nmass){
  
  # chlorophyll content in mmol g-1≠
  # nmass in gN g-1
  chlorophyll_mol = chlorophyll / 1000
  cb = 2.75 / 1000 # chlorophyll binding in mol chlorophyll (g N chlorophyll)-1 assuming most is in LHCII
  
  p_lightharvesting = chlorophyll_mol *  (1 / nmass) * (1 / cb)
  
  p_lightharvesting # g N light harvesting / g N
  
  # cytf_m2 = (jmax25 / 156) / 1000 # mmol cyt f m-2
  # cytf_chlor = cytf_m2 * (1/marea) * (1/chlorophyll_mol) # mmol cyt f (mol chlorophyll)-1
  # psii_chlor = 1.98 * cytf_chlor - (0.365 * (cytf_chlor^2)) # mmol (mol chlorophyll)-1
  # psi_chlor = 1.7 # mmol (mol chlorophyll)-1
  # psii_g = psii_chlor * (chlorophyll / 1000) # mmol g-1
  # psi_g = psi_chlor * (chlorophyll / 1000) # mmol g-1
  # 
  # lhcii_g = chlorophyll - (psii_g + psi_g)
  # 
  # psii_prop = psii_g / chlorophyll
  # psi_prop = psi_g / chlorophyll
  # lhcii_prop = lhcii_g / chlorophyll
  # 
  # cb_psii = 0.858 # mmol chl (gN)-1
  # cb_psi = 2.18 # mmol chl (gN)-1
  # cb_lhcii = 2.75 # mmol chl (gN)-1
  # 
  # p_lightharvesting = (chlorophyll / nmass) * (1/((psii_prop*cb_psii) + (psi_prop*cb_psi) +(lhcii_prop*cb_lhcii)))
}

###############################################################################
# n_structure(lma):
###############################################################################
#
# Calculates leaf nitrogen content allocated to structure. Function uses
# LMA as an input per empirical slope of the relationship between LMA and
# the amount of nitrogen allocated to cell wall tissue. Eq. listed in legend
# of Fig. 4b
#
# Function arguments:
#   - lma           = leaf mass per area (g m^-2)
#
# Returns:
# Vector with leaf N content allocated to cell wall tissue
# (n_structure; gN m^-2)
n_structure = function(lma){
  
  n_structure = 0.000355 * (lma ^ 1.39)
  n_structure
  
}

###############################################################################
# p_structure(lma, narea):
###############################################################################
#
# Calculates leaf nitrogen content allocated to structure. Function uses
# LMA as an input per empirical slope of the relationship between LMA and
# the amount of nitrogen allocated to cell wall tissue. Eq. listed in legend
# of Fig. 4b, then divides by leaf nitrogen content per leaf area to estimate
# proportion of leaf N allocated to cell wall tissue
#
# Alternatively, function can calculate relative proportion of leaf N allocated
# to cell wall tissue following a second empirical equation that explains the
# relationship between LMA and the relative proportion of leaf N allocated to
# cell wall tissue (Ncw / Narea). This equation is listed in the legend of Fig.
# 4c. Use caution with this function, as the variance explained through this
# regression is less than the LMA-Ncw relationships (R^2 = 0.35 for 
# LMA-Ncw/Narea compared to 0.55 for LMA-Ncw)
#
# Function arguments:
#   - lma           = leaf mass per area (g m^-2)
#   - narea         = leaf nitrogen content per unit leaf area (gN m^-2)
#
# Returns:
# Vector with proportion of leaf N content allocated to cell wall tissue
# (p_structure; gN gN^-1)
p_structure = function(lma, narea, useEq = FALSE){
  
  p_structure = ifelse(useEq == FALSE,
                       0.000355 * (lma ^ 1.39) / narea,
                       28.7*log(lma) - 41.5)
  
p_structure
  
}





###############################################################################
# References
###############################################################################

# Niinemets Ü., Kull O. & Tenhunen J.D. (1998) An analysis of light effects on 
# foliar morphology, physiology, and light interception in temperate deciduous 
# woody species of contrasting shade tolerance. Tree Physiology 18, 681–696.

# Niinemets Ü. & Tenhunen J.D. (1997) A model separating leaf structural and 
# physiological effects on carbon gain along light gradients for the 
# shade-tolerant species Acer saccharum. Plant, Cell & Environment 20, 845–866.

# Dong N., Prentice I.C., Evans B.J., Caddy-Retalic S., Lowe A.J. & Wright I.J. 
# (2017) Leaf nitrogen from first principles: field evidence for adaptive 
# variation with climate. Biogeosciences 14, 481–495.
