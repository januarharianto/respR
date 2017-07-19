#' @title Wrapper for if-stop condition in functions.
#' @description A very simple function that may not be used...
#' @param condition The expression/condition to evaluate.
#' @param msg An error message which will be printed if condition is fulfiled.
#' @return NULL
#' @author Januar Harianto
#' @export
#' @examples
#' errorCheck(x > 5, "Value is bigger than 5.")
# credit to https://stackoverflow.com/a/8345314
errorCheck <- function (condition, msg) {
  if (condition)
    stop(msg, call. = FALSE)
}



#' @title Check for NA and NaN
#' @description Returns TRUE or FALSE. Also prints an index when summary = T.
#' Dataframe MUST be parsed through prepData() before this.
#' @param df Dataframe.
#' @param col Numeric. Selects column index to parse.
#' @param summary Logical. Defaults to FALSE.
#' @return NULL
#' @author Januar Harianto
#' @export
#' @examples
#' hasNA
# TRUE - there are NA values
# FALSE - no NA values
hasNA <- function(df, col=1, summary = F) {
  x <- df[[col]]
  check <- any(is.na(x))
  out <- which(is.na(x))
  if(summary)
    return(out)
  else
    return(check)
}



#' @title Check for duplicates
#' @author Januar Harianto
#' @export
dupes <- function(df, summary = F) {
  x      <- df[[1]]
  check  <- isTRUE(any(duplicated(x)))
  out    <- which(duplicated(x) == T)
  if(summary)
    return(out)
  else
    return(check)
}



#' @title Check for monotonically increasing data.
#' @author Januar Harianto
#' @export
# https://stackoverflow.com/a/13094801
# Maybe expand this to detect monotically decreasing data as well. Then, it
# might be implemented in a function to DETECT INTERMITTENT DATASET :D :D :D
monotonic <- function(df, summary=F){
  x <- df[[1]]
  check <- all(x == cummax(x))
  out   <- which(diff(x) < 0) # ID rows that return FALSE
  if (summary)
    return(out)
  else
    return(check)
}



#' @title Check for evenly spaced time data
#' @author Januar Harianto
#' @export
# https://stackoverflow.com/a/4752580
evenSpaced <- function(df, col=1, tol=.Machine$double.eps * 100){
  x     <- diff(df[[col]])
  cond  <- range(x) / mean(x)
  check <- isTRUE(all.equal(cond[1], cond[2], tolerance = tol))
  return(check)
}



#' @title Calculate mode. I may or may not use this...for auto detection of
#' best regression.
#' @author Januar Harianto
#' @export
Mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

# Just a benchmarking function (VERY SIMPLE), will delete this later -----------
# bench <- function(x){
#   system.time({x})
# }



theme_respr <- function() {
  theme_bw(base_size = 12) %+replace%
    theme(
      panel.background = element_rect(fill = "transparent", colour = NA),
      plot.background = element_rect(fill = "transparent", colour = NA),
      #plot.margin = unit(c(10,5,5,5),"mm"),

      panel.border = element_rect(fill = 'transparent', size = 1),
      # panel.grid.major = element_line(colour="#f0f0f0"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),

      axis.text	= element_text(size = rel(1)),
      axis.title = element_text(face = 'bold', size = rel(1)),
      # axis.title.y = element_text(angle = 90, margin = margin(0, 25, 0, 0)),
      # axis.title.x = element_text(margin = margin(25, 0, 0, 0)),
      axis.ticks = element_line(),

      legend.key = element_rect(colour = NA),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.key.size = unit(0.2, "cm"),
      legend.title = element_text(face="italic"),

      strip.background = element_rect(colour="#f0f0f0",fill="#f0f0f0"),
      strip.text = element_text(face="bold")
    )
}
