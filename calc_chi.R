calc_chi <- function(leaf.d13c) {
  # Constants
  air = -8
  a = 4.4
  b = 27

  # Equations 
  delta <- (air - leaf.d13c) / (1 + leaf.d13c * 0.001)
  chi <- (delta - a) / (b - a)
  
  return(chi)
}