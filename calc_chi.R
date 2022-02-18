calc_chi <- function(leaf.d13c, type = c("c3", "c4")) {
  
  # Global constants
  air = -8
  a = 4.4
  b = 27
  c = -5.7
  d = 30
  phi = 0.4
  
  # Leaf carbon discrimination relative to air
  delta <- (air - leaf.d13c) / (1 + leaf.d13c * 0.001)
  
  if(type == "c3") {
    
  chi <- (delta - a) / (b - a)
  
  }
  
  else{
    b.c4 <- c + (d * phi)
    chi <- (delta - a) / (b.c4 - a)

  }
  
  return(chi)

}

