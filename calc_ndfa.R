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
calc_ndfa <- function(ref.15n = NA, sample.15n = NA, B = NA) {

  ndfa <- (ref.15n - sample.15n) / (ref.15n - B) * 100 
  
  return(ndfa)
}

# Test
#calc_ndfa(ref.15n = 1, sample.15n = 0.5, B = 0)
