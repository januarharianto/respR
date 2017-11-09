# check os
os <- function() {
  if (.Platform$OS.type == "windows")
    "win" else if (Sys.info()["sysname"] == "Darwin")
      "mac" else if (.Platform$OS.type == "unix")
        "unix" else stop("Unknown OS")
}

# tic - for time elapsed
tic <- function(gcFirst = TRUE, type = c("elapsed", "user.self", "sys.self")) {
  type <- match.arg(type)
  assign(".type", type, envir = baseenv())
  if (gcFirst)
    gc(FALSE)
  tic <- proc.time()[type]
  assign(".tic", tic, envir = baseenv())
  invisible(tic)
}

# toc - for time elapsed
toc <- function() {
  type <- get(".type", envir = baseenv())
  toc <- proc.time()[type]
  tic <- get(".tic", envir = baseenv())
  elapsed <- (toc - tic)[[1]]
  return(elapsed)
}

