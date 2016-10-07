Outliers <- function(x){
  # Find outliers and replaced by NA
  qnt <- quantile(x, probs = c(0.25, 0.75), na.rm = T)
  H <- 1.5*IQR(x, na.rm = T)
  y <- x
  y[x < (qnt[1]-H)] <- NA
  y[x > (qnt[2]+H)] <- NA
  return(y)
}
