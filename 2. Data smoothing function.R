## Smoothing function

# x = a data frame of two columns, Time and O2 (numbers or integers)
# n = smoothing factor - number of oxygen measurements to average over. 
# span = back, centered or forward. Coverage of the smoothing; previous n points, forward n points, or 
# centred on current point. For even numbered n, centering is biased one point forward 
# truncate = if FALSE, modifies the smoothing factor at start and end of the O2 vector where n would
# be greater than the points available. The mean is then calculated only over those points available. 
# If TRUE, NA's are substituted and the resulting smoothed data will have n-1 fewer O2 datapoints than
# the original data at the start, end or both, depending on the 'span' operator

movav <- function(x, n = 10, span = "centered", truncate = FALSE) {
  if(span == "back"){
    if(truncate == FALSE){
      d1 <- c()   # empty vectors to feed into
      d2 <- c()
      
      for(i in 1:(n-1)){                      # mean of first n-1 rows
        d1 <- c(d1, mean(x[1:i,2]))}
      for(i in n:nrow(x)){                    # mean of remaining rows
        d2 <- c(d2, mean(x[(i-(n-1)):i,2]))}
      
      smoooothed <- setNames(data.frame(x$Time, c(d1,d2)), c(colnames(x)))
      
      ## for span=back, truncate=TRUE
    } else {
      d1 <- c()
      d2 <- c()
      
      for(i in 1:(n-1)){                      # NA's for first n-1 rows
        d1 <- c(d1, "NA")}
      
      for(i in n:nrow(x)){                    # mean of rows except first n-1 
        d2 <- c(d2, mean(x[(i-(n-1)):i,2]))}
      
      smoooothed <- setNames(data.frame(x$Time, c(d1, d2)), c(colnames(x)))}
    
      ## for span=forward, truncate=FALSE
  } else if(span == "forward"){
    if(truncate == FALSE){      
      d1 <- c()   # empty vectors to feed into
      d2 <- c()
      
      for(i in 1:(nrow(x)-n)){                      # mean of rows 1:(n-i)
        d1 <- c(d1, mean(x[i:(i+n-1),2]))}
      
      for(i in (nrow(x)-n+1):nrow(x)){                    # mean of remaining rows
        d2 <- c(d2, mean(x[i:nrow(x),2]))}
      
      smoooothed <- setNames(data.frame(x$Time, c(d1,d2)), c(colnames(x)))
      
      ## for span=forward, truncate=TRUE
    } else {
      d1 <- c()   # empty vectors to feed into
      d2 <- c()
      
      for(i in 1:(nrow(x)-n)){                      # mean of rows 1:(n-i)
        d1 <- c(d1, mean(x[i:(i+n-1),2]))}
      
      for(i in (nrow(x)-n+1):nrow(x)){              # NA's for remaining rows
        d2 <- c(d2, "NA")}
      
      smoooothed <- setNames(data.frame(x$Time, c(d1,d2)), c(colnames(x)))
      
      ## for span=centred, truncate=FALSE
    }} else if(truncate == FALSE){
      
      d1 <- c()
      d2 <- c()
      d3 <- c()
      
      for(i in 1:(n/2)){                      # mean of rows 1:(n/2)
        d1 <- c(d1, mean(x[i:(i+n/2-1),2]))}
      
      for(i in (n/2):(nrow(x)-n/2)){              # middle rows
        d2 <- c(d2, mean(x[i:(i+n/2-1),2]))}
      
      for(i in (nrow(x)-(ceiling(n/2)-2)):nrow(x)){       ## last n/2 rows. Ceiling rounds up odd numbers
        d3 <- c(d3, mean(x[i:nrow(x),2]))}
      
      smoooothed <- setNames(data.frame(x$Time, c(d1,d2,d3)), c(colnames(x)))

      ## for span=centred, truncate=TRUE
    } else { 
      
      d1 <- c()
      d2 <- c()
      d3 <- c()
      
      for(i in 1:(n/2)){                      # mean of rows 1:(n/2)
        d1 <- c(d1, "NA")}
      
      for(i in (n/2):(nrow(x)-n/2)){              # middle rows
        d2 <- c(d2, mean(x[i:(i+n/2-1),2]))}
      
      for(i in (nrow(x)-(ceiling(n/2)-2)):nrow(x)){       ## last n/2-1 rows. Ceiling rounds up odd numbers
        d3 <- c(d3, "NA")}
      
      smoooothed <- setNames(data.frame(x$Time, c(d1,d2,d3)), c(colnames(x)))
    }}


## Test 
data <- movav(rawdata, n=30)
plot(data, pch=".")
