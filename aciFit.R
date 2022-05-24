aciFit <- function(data, photo_vars = list(A = "A", Ci = "Ci",  Tleaf = "Tleaf",
                                            PPFD = "Qin", Rd = "rd"),
                    ci.transition = 400,
                    ci.transition2 = 1000,
                    useRd = TRUE,
                    fitTPU = FALSE,
                    elv = 0) {
  
  ## Create list
  if(useRd) {
  df <- list(A = data[, photo_vars[[1]]],
             Ci = data[, photo_vars[[2]]],
             Tleaf = mean(data[, photo_vars[[3]]]),
             PPFD = mean(data[, photo_vars[[4]]]),
             Rd = mean(data[, photo_vars[[5]]]))
  }
  
  if(!useRd) {
    df <- list(A = data[, photo_vars[[1]]],
               Ci = data[, photo_vars[[2]]],
               Tleaf = mean(data[, photo_vars[[3]]]),
               PPFD = mean(data[, photo_vars[[4]]]),
               Rd = NA)
  }
  
  ## Calculate gammastar, Kc, Ko, Km based on leaf temp
  gammastar25 = 42.75 # Bernacchi & Long (2001); μmol mol^-1
  Ko25 = 278.4 # Bernacchi & Long (2001); μmol mol^-1
  Kc25 = 404.9 # Bernacchi & Long (2001); μmol mol^-1
  Hgs = 37830 # Bernacchi and Long (2001); converted from kJ/mol to J/mol
  Hko = 36380 # Bernacchi and Long (2001); converted from kJ/mol to J/mol
  Hkc = 79430 # Bernacchi and Long (2001); converted from kJ/mol to J/mol
  R = 8.314 # Universal gas constant
  
  df.coefs <- list(gammastar = gammastar25 * 
                     exp((Hgs * (df$Tleaf + 273.15 - 298))/
                           (298 * R * (df$Tleaf + 273.15))),
                   Ko = Ko25 * exp((Hko * (df$Tleaf + 273.15 - 298))/
                                     (298 * R * (df$Tleaf + 273.15))),
                   Kc = Kc25 * exp((Hkc * (df$Tleaf + 273.15 - 298))/
                                     (298 * R * (df$Tleaf + 273.15))))
  df.coefs$Km = df.coefs[["Kc"]] * (1 + (0.0020 * df.coefs[["Ko"]]))
  
  
  # ## Gather Vcmax, Jmax, TPU estimates (if fitTPU = TRUE)
  # if(fitTPU) {
  #   vcmax.df <- subset(data, Ci < ci.transition)
  #   jmax.df <- subset(data, Ci > ci.transition & Ci < ci.transition2)
  #   tpu.df <- subset(data, Ci > ci.transition2)
  #   
  #   ## Nls Vcmax fit
  #   nls.vcmax <- nls(formula = Ac ~ (vcmax*Ci)/ (Ci + Kc*(1 + (O/Ko))))
  #   
  #   ## Nls Jmax fit
  #   nls.jmax <- nls(formula = A ~ )
  #   
  #   ## Nls TPU fit
  #   
  # }
  # ## Gather Vcmax, Jmax, TPU estimates (if fitTPU = FALSE)
  # if(fitTPU) {
  #   vcmax.df <- subset(data, Ci < ci.transition)
  #   jmax.df <- subset(data, Ci > ci.transition & Ci < ci.transition2)
  #   
  #   ## Nls Vcmax fit
  #   nls.vcmax <- nls(formula = A ~ )
  #   
  #   ## Nls Jmax fit
  #   nls.jmax <- nls(formula = A ~)
  #    
  #  
  #    
  #  }

  
  
  output <- list(raw_data = df, 
                 temp_resp = df.coefs, 
                 transition_points = list(ci.transition = ci.transition,
                                          ci.transition2 = ci.transition2))
  
  return(output)
  
}
 

test.df <- read.csv("/Users/eaperkowski/git/NxI_soybean_phys/licor_data_cleaned/aci/2021_10-14_aci_soybean_gib_cleaned.csv") %>%
  filter(id == "r13_hn_ni") %>%
  select(A, Ci, Tleaf = TleafEB, Qin) %>%
  mutate(rd = 0.672)

test <- aciFit(test.df, useRd = TRUE)
