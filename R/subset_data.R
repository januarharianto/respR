#' Subset a `data.frame`, `inspect`, or `inspect.ft` object
#'
#' `subset_data` subsets a `data.frame`, `inspect`, or `inspect.ft` object based
#' on a given set of criteria. The function is ideal for passing only selected
#' regions of data to other functions such as [`calc_rate()`] and
#' [`auto_rate()`], either by saving the output as a new object or via the use
#' of pipes (`%>%` or `|>`). It is also very useful in analysis of
#' intermittent-flow data, where in a loop each replicate can be extracted and
#' passed to an analytical function such as `calc_rate` or `auto_rate`. See
#' examples and vignettes.
#'
#' The function can subset data based on ranges of `"time"`, `"oxygen"`, `"row"`
#' , or `"proportion"` of total oxygen used or produced (note, this last option
#' works poorly with noisy or fluctuating data). For data frames, to subset by
#' `"time"`, `"oxygen"`, or `"proportion"`, the time data is assumed to be in
#' the first column, and oxygen data in the second column. For [`inspect()`] and
#' [`inspect.ft()`] objects, the data will have been coerced to this structure
#' already. In these cases the `$dataframe` element in the output is replaced by
#' the subset, and in `inspect.ft` the `$data` element is also subset and
#' replaced. Note for `inspect.ft` objects, the oxygen data in column 2 will be
#' either `out.oxy` data or `delta.oxy` data depending on what was inspected.
#' The function can subset *any* data frame by `row`.
#'
#' When multiple columns are present, for example time in column 1, and multiple
#' columns of oxygen data, the subset object will include *all* columns. In the
#' case of subsetting `by = "oxygen"` or `by = "proportion"`, subsetting is
#' based on the *first* column of oxygen data only (i.e. column 2), and all
#' subsequent columns are subset between the same rows regardless of oxygen
#' values.
#'
#' For all methods, if exact matching values of `from` and `to` are not present
#' in the data, the closest values are used. For `"time"` and `"row"`
#' subsetting, `from` and `to` should be in the correct order. No warning or
#' messages are given if the input values are outside those in the data frame.
#' For instance, if `to = 100` and there are only 50 rows in the data, the last
#' row (50) will be used instead. The same for `from` and `to` time values
#' outside those in the data frame.
#'
#' For `"oxygen"` or `"proportion"` subsetting, `from` and `to` are generally
#' interchangeable, and the function will subset data *between* the first and
#' last occurrences (or closest occurrences) of these values. It works best with
#' generally increasing or decreasing oxygen data, and results may vary with
#' other data such as intermittent flow data or those in `inspect.ft` objects.
#'
#' **Note for `inspect` and `inspect.ft` object inputs:** after subsetting the
#' locations of any data issues highlighted when the object was originally
#' inspected will no longer be accurate. If these are important, best practice
#' is to subset the original dataframe, and then process the subset through
#' `inspect` or `inspect.ft`.
#'
#' A summary of the subset is printed to the console, to check it has subset the
#' data as expected. To suppress this changing the default `quiet = FALSE` to
#' `TRUE`.
#'
#' ## More
#'
#' For additional help, documentation, vignettes, and more visit the `respR`
#' website at <https://januarharianto.github.io/respR/>
#'
#' @return **Output**: If the input is an `inspect`, or `inspect.ft` object, the
#'   output is an object of the same class containing the subset data. For
#'   `data.frame` inputs the output is a `data.table` of the subset.
#'
#' @param x `data.frame`, `inspect`, or `inspect.ft` object. The data from which
#'   to produce a subset.
#' @param from numeric. The lower bounds of the subset based on the `by` input.
#' @param to numeric. The upper bounds of the subset based on the `by` input.
#' @param by string. "time"`, `"row"`, `"oxygen"` or `"proportion"`. Method by
#'   which to apply the `from` and `to` inputs.
#' @param quiet logical. Controls if a summary of the output is printed to the
#'   console. Default is `FALSE`.
#'
#' @export
#'
#' @examples
#' # Subset by time:
#' x <- subset_data(squid.rd, from = 2000, to = 4000, by = "time")
#'
#' # Subset by oxygen:
#' subset_data(sardine.rd, from = 94, to = 91, by = "oxygen")
#'
#' # Subset by row:
#' subset_data(flowthrough.rd, from = 10, to = 750, by = "row")
#'
#' # Subset multiple columns:
#' # In this case subsetting is based on the first two columns
#' subset_data(flowthrough.rd, from = 50, to = 600, by = "time")
#'
#' # Pass (via piping) only a subset of a dataset to inspect() and auto_rate()
#' subset_data(sardine.rd, from = 94, to = 91, by = "oxygen") %>%
#'    inspect(time = 1, oxygen = 2) %>%
#'    auto_rate()

subset_data <- function(x, from = NULL, to = NULL, by = "time", quiet = FALSE) {

  # Check if object is from respR function(s)
  if (any(class(x) %in% "inspect")) {
    dt <- data.table(x$dataframe)
  } else if (any(class(x) %in% "inspect.ft")) {
    dt <- data.table(x$dataframe)
  } else dt <- data.table(x)

  ## verify by input
  by <- verify_by(by, msg = "subset_data:")

  ## replace NULL inputs
  if(is.null(from)){
    if(by == "time") from <- min(nainf.omit(dt[[1]]))
    if(by == "row") from <- 1
    if(by == "oxygen") from <- dt[[2]][1] # first oxygen value
    if(by == "proportion")
      stop("subset_data: please enter a proportion 'from' input.")
  }
  if(is.null(to)){
    if(by == "time") to <- max(nainf.omit(dt[[1]]))
    if(by == "row") to <- nrow(dt)
    if(by == "oxygen") to <- dt[[2]][nrow(dt)] # last oxygen value
    if(by == "proportion")
      stop("subset_data: please enter a proportion 'to' input.")
  }

  ## if time, 'from' required, single val, not bigger than max value in time
  if(by == "time") {
    input.val(from,  num = TRUE, int = FALSE, req = FALSE,
              max = 1, min = 1, range = c(0, max(nainf.omit(dt[[1]]))),
              msg = "subset_data: 'from' -")
    ## if time, 'to' required, single val, greater than from
    input.val(to,  num = TRUE, int = FALSE, req = FALSE,
              max = 1, min = 1, range = c(from, Inf),
              msg = "subset_data: 'to' -")
    }

  ## if row, 'from' required, single val, integer
  if(by == "row") {
    input.val(from,  num = TRUE, int = TRUE, req = FALSE,
              max = 1, min = 1, range = c(1, nrow(dt)),
              msg = "subset_data: 'from' -")
    ## if row, 'to' required, single val, integer, greater than from
    input.val(to,  num = TRUE, int = TRUE, req = FALSE,
              max = 1, min = 1, range = c(from+1, Inf),
              msg = "subset_data: 'to' -")}

  ## if oxygen or prop, 'from' & 'to' required, single val, numeric
  if(by == "oxygen" || by == "proportion") {
    input.val(from,  num = TRUE, int = FALSE, req = FALSE,
              max = 1, min = 1, range = c(-Inf, Inf),
              msg = "subset_data: 'from' -")
    input.val(to,  num = TRUE, int = FALSE, req = FALSE,
              max = 1, min = 1, range = c(-Inf, Inf),
              msg = "subset_data: 'to' -")}

  out <- truncate_data(dt, from, to, by)

  # inspect.ft has an additional element that needs subset - $data
  if (any(class(x) %in% "inspect.ft")) {

    # get indices of start and end of subset
    # Must be a better way of doing this...

    # collapse list of lists to list of dfs
    dt2 <- sapply(x$data, function(z) rbind.data.frame(z))
    # collapse list of dfs to dt
    dt2 <- as.data.table(dt2)
    # for some stupid reason the names get changed
    names(dt2) <- names(x$dataframe)

    # start and end of rows which are identical
    start <- min(which(duplicated(rbind(dt2, out), fromLast = TRUE)))
    end <- max(which(duplicated(rbind(dt2, out), fromLast = TRUE)))

    # if out was empty, start and end are now Inf & -Inf, which produces errors
    if(start == Inf || start == -Inf) start <- 0
    if(end == Inf || end == -Inf) end <- 0

    # subset x$data elements
    data_sub <- lapply(x$data, function(y) {
      lapply(y, function(z){
        z[start:end]
      })
    })

    x$data <- data_sub
  }

  if(!quiet) {
    cat("\n# subset_data # -------------------------\n")
    cat("Original data:\n")
    print(dt, topn = 2)
    cat("\nSubset data:\n")
    print(data.table(out), topn = 2)
    cat("-----------------------------------------\n")
  }

  # if out is empty, warn.
  # Still return in case this would break loops or whatever
  if(nrow(out) == 0)
    warning("subset_data: subsetting criteria result in empty dataset!")

  if (any(class(x) %in% "inspect")) {
    x$dataframe <- out
    return(invisible(x))
  } else if (any(class(x) %in% "inspect.ft")) {
    x$dataframe <- out
    return(invisible(x))
  } else return(invisible(out))
}

