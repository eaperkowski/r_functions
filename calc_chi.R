calc_chi <- function(leaf.d13c, type = c("c3", "c4", NA)) {
  
  # Global constants
  air = -8
  a = 4.4
  b = 27
  c = -5.7
  d = 30
  phi = 0.4
  
  # Leaf carbon discrimination relative to air
  delta <- (air - leaf.d13c) / (1 + leaf.d13c * 0.001)
  
  if(is.na(type) == TRUE) {
    type == "c3"
    warning("No photosynthetic pathway input. Defaulting to c3 pathway")
  
  if(type == "c3") {
    
  chi <- (delta - a) / (b - a)
  
  }
  
  if(type == "c4") {
    b.c4 <- c + (d * phi)
    chi <- (delta - a) / (b.c4 - a)

  }
  
  if(type != "c3" & type != "c4") {
    
    chi <- (delta - a) / (b - a)
    warning("Invalid photosynthetic pathway. Defaulting to c3 pathway")
    
  }
  
  return(chi)

}
calc_chi(seq(-28, -26, 0.1), NA)
