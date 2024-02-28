# calc_ndfa(ref.15n = NA, sample.15n = NA, B = NA):
#
# Helper fxn to calculates percent nitrogen fixed from the 
# atmosphere using leaf 15N data. Equations follow those 
# laid out in Andrews et al. (2011)
#
# Inputs:
#     - ref.15n     = reference 15N from non-fixing plant 
#     - sample.15n  = leaf 15N of unknown leaf
#     - B           = leaf 15N of fully fixing plant  
#
# Returns:
#     - Vector containing %Ndfa. Vector is already in percent form
calc_ndfa <- function(ref.15n = NA, sample.15n = NA, B = NA, use_atm_val = TRUE) {
  
  if(use_atm_val == TRUE) {
    
    d15n_atm <- 0.3663 # % of 15N in atmosphere (Dawson et al., 2002)
    
    ndfa <- (ref.15n - sample.15n) / (ref.15n - d15n_atm) * 100
  }
  
  else(ndfa <- (ref.15n - sample.15n) / (ref.15n - B) * 100)
  
  return(data.frame(ndfa))
}

# Test
#calc_ndfa(ref.15n = 0, sample.15n = 0.5, B = 1, use_atm_val = TRUE)
#calc_ndfa(ref.15n = 0, sample.15n = 0.5, B = 0, use_atm_val = TRUE)

# alc_ndfa(ref.15n = 0, sample.15n = 0.5, B = 1, use_atm_val = FALSE)
#calc_ndfa(ref.15n = 0, sample.15n = 0.1, B = 0.5, use_atm_val = FALSE)
