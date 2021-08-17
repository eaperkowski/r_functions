compactLetterGrouping.1 <- function(df, 
                                    var = "variable", 
                                    resp = "response", 
                                    upper = TRUE,
                                    transform = c("log", "sqrt", "none"),
                                    barrier = 0.05) {
  library(dplyr)
  library(agricolae)
  library(data.table)
  library(tidyr)
  
  df <- data.frame(df)
  df$var <- df[ , var]
  df$response <- df[, resp]
  
  var.header <- var
  
  if(transform == "log") {
    var.lm <- lm(log(response) ~ var, data = df)
  }
  
  if(transform == "sqrt") {
    var.lm <- lm(sqrt(response) ~ var, data = df)
  }
  
  if(transform == "none") {
    var.lm <- lm(response ~ var, data = df)
  }
  
  df <- select(df, var, response)
  df <- group_by(df, var)
  df <- summarize(df,
                  y.label = max(response, na.rm = TRUE) + barrier)
  
  df.posthoc <- HSD.test(y = var.lm, trt = "var")
  df.posthoc <- data.frame(df.posthoc$groups)
  setDT(df.posthoc, keep.rownames = TRUE)
  names(df.posthoc)[names(df.posthoc) == "rn"] <- "var"
  df.posthoc <- data.frame(df.posthoc)
  
  df <- data.frame(full_join(df, df.posthoc, by = "var"))
  
  names(df)[names(df) == "var"] <- var.header
  
  if(upper == TRUE) {
    df$groups <- toupper(df$groups)
  }
  else(df$groups <- tolower(df$groups))
  
  return(df)
}
