static.roll <- function(df, width) {
  x <- roll::roll_lm(matrix(df[[1]]), matrix(df[[2]]), width)$coefficients
  out <- data.table::data.table(na.omit(x))
  colnames(out) <- c("b0", "b1")
  return(out[,1:2])
}
