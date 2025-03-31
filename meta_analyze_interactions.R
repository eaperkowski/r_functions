# meta_analyze_interaction() 
# 
# Function uses formulation for determining whether interactive effects of
# multifactor experiments are additive, synergistic, or antagonistic. Function
# calculates the interaction effect size (dI) and the weighted mean of dI
# (d_plus_plus) following descriptions from Yue et al. (2017)
#
# Function inputs:
#
#   - x_a     = treatment mean of driver A
#   - s_a     = treatment standard deviation of driver A
#   - n_a     = treatment sample size of driver A
#   - x_b     = treatment mean of driver B
#   - s_b     = treatment standard deviation of driver B
#   - n_b     = treatment sample size of driver B
#   - x_c     = treatment mean of control
#   - s_c     = treatment standard deviation of control
#   - n_c     = treatment sample size of control
#   - x_ab    = treatment mean of driver A + B
#   - s_ab     = treatment standard deviation of driver A + B
#   - n_ab    = treatment sample size of driver A + B
# 
# Function returns:
#
# A list containing the 

meta_analyze_interactions <- function(x_a, s_a, n_a,
                                      x_b, s_b, n_b,
                                      x_c, s_c, n_c,
                                      x_ab, s_ab, n_ab) {
  
  # Degrees of freedom
  m = (n_c + n_a + n_b + n_ab - 4)
  
  # Pooled standard deviation
  s = sqrt( 
    ((n_c - 1)*(s_c^2) + 
       (n_a - 1)*(s_a^2) +
       (n_b - 1)*(s_b^2) +
       (n_ab - 1)*(s_ab^2)) / m)
  
  # Correction term for small sample size
  j_m = 1 - (3 / (4 * m - 1))
  
  # Interaction effect size between variable A and B using Hedges' d
  # Note that this is the standardized mean difference not biased by
  # small sample sizes (Gurevitch & Hedges 2001)
  dI = (((x_ab - x_a) - (x_b - x_c)) / 2 * s) * j_m
  
  # Variance of the interaction effect size
  v2 = 0.25 * (1/n_c + 
                 1/n_a + 
                 1/n_b + 
                 1/n_ab + 
                 (dI^2 / (2 * (n_c + n_a + n_b + n_ab))))
  
  d_plusPlus = 

  
  return(dI, v2, s, m, j_m, d_plusPlus)
  
}