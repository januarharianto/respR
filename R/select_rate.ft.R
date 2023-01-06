#' @title Select from flowthrough respirometry rate results based on a range of
#'   criteria
#'
#' @rdname select_rate
#' @export

select_rate.ft <- function(x, method = NULL, n = NULL){
  out <- select_rate(x, method, n)
  return(invisible(out))
}

