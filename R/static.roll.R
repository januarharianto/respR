static.roll <- function(df, width) {
  x <- roll::roll_lm(matrix(df[[1]]), matrix(df[[2]]), width)
  out <- na.omit(cbind(x$coefficients,x$r.squared))
  out <- data.table::data.table(out)
  data.table::setnames(out, 1:3, c("intercept", "rate", "rsq"))
  return(out)
}


