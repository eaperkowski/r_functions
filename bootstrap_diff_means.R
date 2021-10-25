bootstrap_diff_means <- function(df, 
                                 group = "treatment", 
                                 var = "value", 
                                 tail ="one", 
                                 doPlot = TRUE) {
  df$var <- df[, var]
  df$group <- df[, group]
  df$group <- factor(df$group)
  ids <- levels(df$group)
  group1 <- subset(df, group == ids[1])
  group2 <- subset(df, group == ids[2])
  difference <- mean(group2$var) - mean(group1$var)

  # ------- Bootstrap portion -------------
  set.seed(5)
  nIter <- 5000
  null.distribution <- numeric(nIter)
  for (i in 1:nIter) {
    df$bs <- sample(df$var, replace = T, size = nrow(df))
    group1 <- subset(df, group == ids[1])
    group2 <- subset(df, group == ids[2])
    new.difference <- mean(group2$bs) - mean(group1$bs)
    null.distribution[i] <- new.difference
  }
  ci.95 <- quantile(null.distribution, probs=c(0.025, 0.975))
  
  # ------- P-value portion -------------
  cumprob <- ecdf(null.distribution)

  if(tail == "one") {
    if(difference <= 0) {
      p.value <- cumprob(quantile(difference)[1])
    } else {
      p.value <- 1-cumprob(quantile(difference)[1])
    }
  }
  if(tail == "two") {
    p.value.one.tail <- cumprob(quantile(-abs(difference))[1])
    p.value.other.tail <- 1-cumprob(quantile(abs(difference))[1])
    p.value <- p.value.one.tail + p.value.other.tail
  }
  
  if(doPlot = TRUE) {
    plot(density(null.distribution),
         main = "Density plot of the difference between two groups")
    polygon(density(null.distribution), col="gray")
    abline(v = difference, col = "red", lty = "dashed")
    if(tail == "two") {
      abline(v = - difference, col = "red", lty = "dashed")
    }
  }
  
  return(data.frame(mean.difference = difference,
                    lower.ci = ci.95[1],
                    upper.ci = ci.95[2],
                    P.value = p.value))
}