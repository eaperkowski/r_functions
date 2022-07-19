calc_chi_c3 <- function(leaf.d13c) {
  
  # Global constants
  air = -8
  a = 4.4
  b = 27
  
  # Leaf carbon discrimination relative to air
  delta <- (air - leaf.d13c) / (1 + leaf.d13c * 0.001)
  chi <- (delta - a) / (b - a)
  
  return(chi)
  
}


calc_chi_c4 <- function(leaf.d13c) {
  # Global constants
  air = -8
  a = 4.4
  c = -5.7
  d = 30
  phi = 0.37
  
  b.c4 = -5.7 + (d * phi)
  
  # Leaf carbon discrimination relative to air
  delta <- (air - leaf.d13c) / (1 + leaf.d13c * 0.001)
  chi <- (delta - a) / (b.c4 - a)
  
  return(chi)
  
}

