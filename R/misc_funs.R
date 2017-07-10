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
