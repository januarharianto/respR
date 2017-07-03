# Check for NA and NaN ---------------------------------------------------------
# TRUE - there are NA values
# FALSE - no NA values
hasNA <- function(dt, summary = F) {
  check <- any(is.na(dt))
  if (check == T) {
    check %>% print()
  }
  if (check == F) {
    check %>% print()
  }
  if (summary == T) {
    which(is.na(dt), arr.ind = T) %>% print()
  }
}


# Check for duplicate time -----------------------------------------------------
?duplicated.array()
anyDuplicated(nonmon[1])
nonmon[duplicated(nonmon[,1]),]

dupes <- function(df, summary = F) {
  x      <- df[[1]]
  check  <- isTRUE(any(duplicated(x)))
  out    <- which(duplicated(x) == T)
  if(summary)
    return(out)
  else
    return(check)
}

# Check for monotonically increasing time --------------------------------------
# Are all the values in x equal to the cumulative maximum of the same values?
# https://stackoverflow.com/a/13094801

monotonic <- function(df, summary=F){
  x <- df[[1]]
  check <- all(x == cummax(x))
  out   <- which(diff(x) < 0) # ID rows that return FALSE
  if (summary)
    return(out)
  else
    return(check)
}

# Check for evenly spaced time -------------------------------------------------
# https://stackoverflow.com/a/4752580

evenSpaced <- function(df, col=1, tol=.Machine$double.eps * 100){
  x     <- diff(df[[col]])
  cond  <- range(x) / mean(x)
  check <- isTRUE(all.equal(cond[1], cond[2], tolerance = tol))
  return(check)
}

# Calculate mode ---------------------------------------------------------------
Mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}
